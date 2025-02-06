rm(list = ls())

library(brms)
library(gridExtra)
library(matlab)
library(abind)
library(reshape2)
library(ggridges)
library(stringr)
library(scales)
library(tidyr)
library(ggforce)
library(dplyr)
library(LianaRemovalRevisited)
library(cowplot)
library(ggplot2)
library(ggdist)
library(ggridges)

data <- readRDS("./outputs/QSM_metrics.RDS") %>%
  mutate(liana.cat = factor(case_when(Liana == 0 ~ "no",
                                      Liana == 1 ~ "low",
                                      Liana == 2 ~ "high"),
                            levels = c("no","low","high"))) %>%
  mutate(branch_vol_L = tree_vol_L - trunk_vol_L) %>%
  mutate(branch_biomass = WSG*branch_vol_L,
         branch_biomass_order = WSG*branch_volume_order,
         tree_biomass = WSG*tree_vol_L,
         trunk_biomass = WSG*trunk_vol_L)

WD = mean(data$WSG)

cols2test <- c("tree_biomass","trunk_biomass","branch_biomass")

data2keep <- data %>%
  dplyr::select(c("dbh","sp","liana.cat",cols2test))


data.long <- data2keep %>%
  pivot_longer(cols = -c(dbh,sp,liana.cat),
               names_to = "variable")

models <- c("power")
model.forms <- c("all","none","a","b") # "a","k","b", "ab","ak","bk")

cfiles <- list.files("./outputs/",
                     full.names = FALSE)

fit.all <- list()
df.qof <- data.frame()
#
# ################################################################################
# # For predictions
#
alpha <- 0.05
#
dbhs <- seq(floor(min(data$dbh,na.rm = TRUE)),
            ceiling(max(data$dbh,na.rm = TRUE)),
            length.out = 1000)
dbhs2 <- data$dbh

levels <- as.character(unique(data$liana.cat))

newdata <- newdata2 <- data.frame()

for (ilevel in seq(1,length(levels))){

  ccdf <- data %>%
    filter(liana.cat == levels[ilevel])

  cdbhs <- dbhs[dbhs>= min(ccdf$dbh,na.rm = TRUE) &
                  dbhs <= max(ccdf$dbh,na.rm = TRUE)]

  newdata <- bind_rows(list(newdata,
                            data.frame(
                              dbh = rep(cdbhs,1),
                              liana.cat = c(rep(levels[ilevel],length(cdbhs))))
  ))
}

DBH2test <- c(50)

for (ivar in seq(1,length(cols2test))){
  cvar <- cols2test[ivar]
  print("=========================================")
  print(cvar)

  print("Reading")

  all.possible.files <- crossing(models,model.forms) %>%
    mutate(n = paste0("Fit.",cvar,
                      ".",
                      as.character(models),
                      "_",
                      as.character(model.forms),
                      ".RDS")) %>%
    pull(n)

  tokeep <- cfiles %in% all.possible.files
  cfiles.filtered <- cfiles[tokeep]
  cnames.filtered <- tools::file_path_sans_ext(cfiles.filtered)

  # Compile the outputs

  fit.all[[cvar]] <- list()

  for (ifile in seq(1,length(cfiles.filtered))){

    print(paste("-",ifile/length(cfiles.filtered)))
    fit.all[[cvar]][[cnames.filtered[ifile]]] <-  readRDS(paste0("./outputs/",cfiles.filtered[ifile]))
  }


  # Select best model
  print("Processing")

  comparison <- loo_compare(lapply(fit.all[[cvar]], LOO))
  waic <- lapply(fit.all[[cvar]], function(x){
    W <- waic(x)
    return(W$estimates[3,1])})
  rhat.max <- lapply(fit.all[[cvar]],function(x){
    R <- rhat(x)
    return(max(R))})
  rhat.m <- lapply(fit.all[[cvar]],function(x){
    R <- rhat(x)
    return(mean(R))})

  df.qof <- bind_rows(df.qof,
                      data.frame(files = cnames.filtered,
                                 waic = unlist(waic),
                                 rhat.max = unlist(rhat.max),rhat.m = unlist(rhat.m)
                      ))
  best.model.names <- rownames(comparison)[1]
  best.model <- fit.all[[cvar]][[ best.model.names]]
  null.model <- fit.all[[cvar]][[paste0("Fit.",cvar,".power_none")]]


  ################################################################################
  # Predictions

  ccoef <- as.numeric(exp(summary(best.model)[["spec_pars"]][1]**2/2))
  ccoef.null <- as.numeric(exp(summary(null.model)[["spec_pars"]][1]**2/2))

  pp <- melt(posterior_epred(best.model,
                             newdata = newdata,
                             re_formula = NA)) %>%
    rename(rep = Var1,
           id = Var2) %>%
    mutate(var = ccoef*exp(value)) %>%
    group_by(id) %>%
    summarise(var.m = mean(var,na.rm = TRUE),
              var.low = quantile(var,alpha/2,na.rm = TRUE),
              var.high = quantile(var,1 - alpha/2,na.rm = TRUE))

  # pp.resi <-  pp <- melt(posterior_epred(best.model,
  #                                        newdata = data[,c("dbh","sp","liana.cat")],
  #                                        re_formula = NA)) %>%
  #   rename(rep = Var1,
  #          id = Var2) %>%
  #   mutate(var = ccoef*exp(value)) %>%
  #   group_by(id) %>%
  #   summarise(var.m = mean(var,na.rm = TRUE),
  #             var.low = quantile(var,alpha/2,na.rm = TRUE),
  #             var.high = quantile(var,1 - alpha/2,na.rm = TRUE)) %>%
  #   bind_cols(AGB = data$Biomass)
  #
  # RSE <- data.frame(model = "best",
  #                   residual = (pp.resi$var.m-pp.resi$AGB))
  #

  pp.null <- melt(posterior_epred(null.model,
                                  newdata = newdata,
                                  re_formula = NA)) %>%
    rename(rep = Var1,
           id = Var2) %>%
    mutate(var = ccoef.null*exp(value)) %>%
    group_by(id) %>%
    summarise(var.m = mean(var,na.rm = TRUE),
              var.low = quantile(var,alpha/2,na.rm = TRUE),
              var.high = quantile(var,1 - alpha/2,na.rm = TRUE))

#   pp.null.resi <-  pp <- melt(posterior_epred(null.model,
#                                          newdata = data[,c("dbh","sp")],
#                                          re_formula = NA)) %>%
#     rename(rep = Var1,
#            id = Var2) %>%
#     mutate(var = ccoef*exp(value)) %>%
#     group_by(id) %>%
#     summarise(var.m = mean(var,na.rm = TRUE),
#               var.low = quantile(var,alpha/2,na.rm = TRUE),
#               var.high = quantile(var,1 - alpha/2,na.rm = TRUE)) %>%
#     bind_cols(AGB = data$Biomass)
#
#   RSE <- bind_rows(RSE,
#                    data.frame(model = "null",
#                     residual = (pp.null.resi$var.m-pp.resi$AGB)))
#
#   RSE %>%
#     group_by(model) %>%
#     summarise(RMSE = sqrt(1/length(model)*sum((residual)**2)))

  newdata[[paste0(cvar,".m")]] <- pp[["var.m"]]
  newdata[[paste0(cvar,".null.m")]] <- pp.null[["var.m"]]

  for (cDBH2test in DBH2test){
    cnewdata2 <- bind_rows(list(data.frame(
      dbh = rep(cDBH2test,length(levels)),
      liana.cat = levels)))

    cnewdata2 <- cnewdata2 %>%
      mutate(id = 1:length(dbh))

    cmodel <- best.model
    null.model <- null.model


    temp2 <- melt(posterior_epred(cmodel,
                                  newdata = cnewdata2,
                                  re_formula = NA)) %>%
      rename(rep = Var1,
             id = Var2) %>%
      left_join(cnewdata2 %>%
                  dplyr::select(c(id,dbh,liana.cat)),
                by = "id") %>%
      mutate(var = ccoef*exp(value)) %>%
      filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
      ungroup() %>%
      dplyr::select(-c(value,id)) %>%
      pivot_wider(names_from = liana.cat,
                  values_from = var) %>%
      pivot_longer(cols = c("low","high"),
                   names_to = "liana.cat")

    newdata2 <- bind_rows(newdata2,
                          temp2 %>%
                            mutate(target = cDBH2test,
                                   variable = cvar))

  }
}

data2plot <- newdata2 %>%
  filter(!is.na(value)) %>%
  group_by(target,liana.cat,variable) %>%
  mutate(signif_rel = case_when(quantile((value-no)/no*100,probs = 1-alpha/2,na.rm = TRUE) < 0 ~ 0.3,
                                quantile((value-no)/no*100,probs = alpha/2,na.rm = TRUE) > 0 ~ 0.3,
                                TRUE ~ 0.2),
         signif_rel2 = case_when(quantile((value-no)/no*100,probs = 1-alpha/2,na.rm = TRUE) < 0 ~ 1,
                                 quantile((value-no)/no*100,probs = alpha/2,na.rm = TRUE) > 0 ~ 1,
                                 TRUE ~ 0.4))


data2plot %>%
  filter(!is.na(value),
         liana.cat == "high") %>%
  summarise(m = 100*median((value-no)/no,na.rm = TRUE),
            low = 100*quantile((value-no)/no,alpha/2,na.rm = TRUE),
            high = 100*quantile((value-no)/no,1-alpha/2,na.rm = TRUE),

            m.abs = median((value-no),na.rm = TRUE),
            low.abs = quantile((value-no),alpha/2,na.rm = TRUE),
            high.abs = quantile((value-no),1-alpha/2,na.rm = TRUE))



data2plot <- newdata2 %>%
  filter(variable %in% c(cols2test)) %>%
  filter(target == 50) %>%
  filter(!is.na(value)) %>%
  group_by(liana.cat,variable) %>%
  mutate(signif_rel = case_when(quantile((value-no)/no*100,probs = 1-alpha/2,na.rm = TRUE) < 0 ~ 0.3,
                                quantile((value-no)/no*100,probs = alpha/2,na.rm = TRUE) > 0 ~ 0.3,
                                TRUE ~ 0.2),
         signif_rel2 = case_when(quantile((value-no)/no*100,probs = 1-alpha/2,na.rm = TRUE) < 0 ~ 1,
                                 quantile((value-no)/no*100,probs = alpha/2,na.rm = TRUE) > 0 ~ 1,
                                 TRUE ~ 0.4)) %>%
  # mutate(variable = case_when(variable == ,)) %>%
  mutate(variable.fac = as.factor(variable))



ggplot(mapping = aes(x = 100*(value-no)/no,
                     y = 1,
                     fill = liana.cat, color = liana.cat, alpha = signif_rel)) +
  stat_pointinterval(data = data2plot %>%
                       filter(variable != "trunk_biomass"),
                     aes(alpha = signif_rel2,
                         y = 1),
                     .width = c(1 - alpha),
                     alpha = 0.) +
  geom_density_ridges(data = data2plot %>%
                        filter(variable != "trunk_biomass") %>%
                        group_by(variable.fac) %>%
                        filter(!all((value - no) == 0)), color = NA,
                      scale = 0.6 ) +
  stat_pointinterval(data = data2plot %>%
                       filter(variable != "trunk_biomass"),
                     aes(alpha = signif_rel2,
                         y = 1),
                     .width = c(1 - alpha)) +
  geom_vline(xintercept = 0, linetype = 1) +
  theme_minimal() +
  labs(y = "", x = "") +
  facet_wrap(~ variable.fac,
             scales = "free") +
  scale_y_continuous(limits = c(1,1.07),
                     breaks = NULL) +
  theme(legend.position = c(0.9,0.6),
        text = element_text(size = 20)) +
  scale_fill_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred")) +
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  guides(fill = "none", color = "none", alpha = "none")


predictions.long <- newdata %>%
  pivot_longer(cols = c(paste0(cols2test,".m"),
                        paste0(cols2test,".null.m")),
               names_to = "type") %>%
  mutate(variable = sub("\\.m","",
                        sub("\\.null.m","",type)),
         type = case_when(grepl("null",type) ~ "null",
                          TRUE ~ "best")) %>%
  mutate(liana.cat = case_when(type == "null" ~ "null",
                               TRUE ~ liana.cat)) %>%
  group_by(liana.cat,variable) %>%
  filter(variable %in% cols2test)



data.long2plot <- data.long %>%
  mutate(variable.fac = factor(variable,
                               levels = (c("tree_biomass",
                                           "trunk_biomass",
                                           "branch_biomass",

                                           "crown_vol",
                                           "crown_area_conv",
                                           "crown_length",

                                           "branch_area",
                                           "Nbranches",
                                           "branch_len")))) %>%
  mutate(value = case_when(grepl("biomass",variable) ~ value/1e3,
                           TRUE ~ value))

predictions.long2 <- predictions.long %>%
  mutate(variable.fac = factor(variable,
                               levels = (c("tree_biomass",
                                           "trunk_biomass",
                                           "branch_biomass",

                                           "crown_vol",
                                           "crown_area_conv",
                                           "crown_length",

                                           "branch_area",
                                           "Nbranches",
                                           "branch_len")))) %>%
  mutate(value = case_when(grepl("biomass",variable) ~ value/1e3,
                           TRUE ~ value))

ggplot() +
  geom_point(data = data.long2plot %>%
               filter(variable != "tree_biomass"),
             aes(x = dbh, y = value, color = liana.cat),
             alpha = 0.2, size = 0.5) +

  geom_line(data = predictions.long2 %>%
              filter(variable != "tree_biomass"),
            aes(x = dbh,y = value,
                color = liana.cat)) +

  scale_x_log10() +
  scale_y_log10() +
  theme_bw() +
  facet_wrap(~ variable.fac,
             scales = "free",nrow = 1) +
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred",
                                "null" = "black")) +
  scale_fill_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred",
                               "null" = "black")) +
  guides(color = "none", fill = "none") +
  labs(x = "", y = "") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(2, "lines"))

newdata2 %>%
  mutate(diff_h = value - no) %>%
  group_by(variable,target,liana.cat) %>%
  summarise(m = 100*median(diff_h/no,na.rm = TRUE),
            m.low = 100*quantile(diff_h/no,alpha/2,na.rm = TRUE),
            m.high = 100*quantile(diff_h/no,1-alpha/2,na.rm = TRUE),

            m.abs = median(diff_h,na.rm = TRUE),
            m.abs.low = quantile(diff_h,alpha/2,na.rm = TRUE),
            m.abs.high = quantile(diff_h,1-alpha/2,na.rm = TRUE))


# Plus Figure3_QSM

