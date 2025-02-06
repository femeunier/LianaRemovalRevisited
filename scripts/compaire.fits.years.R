rm(list = ls())

library(brms)
library(gridExtra)
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
library(ggdist)
library(ggplot2)

# Load the data
all.df <- readRDS("./outputs/BCI.COI.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10)

all.df.title <- all.df %>%
  group_by(year) %>%
  mutate(year.N = paste0(year,", N = ", length(year)," (",length(year[which(liana.cat == "no")]), "-",
                         length(year[which(liana.cat == "low")]), "-",
                         length(year[which(liana.cat == "high")]), ")"),
         N.low = length(year[which(liana.cat == "low")]),
         N.high = length(year[which(liana.cat == "high")]),
         N.tot = length(year))

years <- unique(all.df$year)


models <- c("weibull","power",'gmm')
model.forms <- c("all","none","a","b","k","ab","ak","bk")

# Compile the outputs
fit.all.years <- list()

print("Reading")

years2keep <- c()
df.rhat <- data.frame()
for (iyear in seq(1,length(years))){

  cyear <- years[iyear]
  cyear.corrected <- gsub(" ", "",cyear, fixed = TRUE)

  print(paste("-",cyear))
  fit.all.years[[iyear]] <- list()

  all.possible.files <- crossing(years[iyear], models,model.forms) %>%
    mutate(n = paste0("Fit.",
                      as.character(cyear.corrected),
                      ".",
                      as.character(models),
                      "_",
                      as.character(model.forms),
                      ".RDS")) %>%
    pull(n)

  # Transfer files
  dir.create(file.path("./outputs/",paste0("BCI.",cyear.corrected)),showWarnings = FALSE)
  transfer.files("*.RDS",
                 base = "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/",
                 source = file.path("data",paste0("BCI.",cyear.corrected)),
                 destination = file.path("./outputs/",paste0("BCI.",cyear.corrected)),
    show.progress = TRUE)

  cfiles <- list.files(file.path("./outputs/",paste0("BCI.",cyear.corrected)),full.names = TRUE,pattern = "*.RDS")

  tokeep <- basename(cfiles) %in% all.possible.files

  cfiles.filtered <- cfiles[tokeep]
  cnames.filtered <- tools::file_path_sans_ext(cfiles.filtered)

  if (length(cfiles.filtered) == 0) next()

  for (ifile in seq(1,length(cfiles.filtered))){

    print(paste("--",ifile/length(cfiles.filtered)))
    fit.all.years[[iyear]][[cnames.filtered[ifile]]] <-  readRDS(paste0(cfiles.filtered[ifile]))

    # check
    all.rhat <- rhat(fit.all.years[[iyear]][[cnames.filtered[ifile]]])

    df.rhat <- bind_rows(list(
      df.rhat,
      data.frame(year = cyear,
                 model = basename(cnames.filtered[ifile]),
                 rhat.m = mean(all.rhat),
                 rhat.max = max(all.rhat))))
  }
}

# Select best model

comparison <- best.model <- pp.check.best <- pp.check.wb <- list()
best.model.names <- c()

actual.df <- data.frame()

print("Processing")

for (iyear in seq(1,length(years))){

  print(iyear/length(years))

  cyear <- years[iyear]
  actual.df <- bind_rows(list(actual.df,
                              all.df %>% filter(year == cyear)))


  if (length(fit.all.years[[iyear]]) > 1){
    X <- (lapply(fit.all.years[[iyear]], LOO,r_eff = NA))
    comparison[[iyear]] <- brms::loo_compare(X)
    best.model.names[iyear] <- rownames(comparison[[iyear]])[1]
  } else {
    comparison[[iyear]] <- NULL
    best.model.names[iyear] <- names(fit.all.years[[iyear]])
  }

  # best.model.names[iyear] <-  names(fit.all.years[[iyear]])[
  #   grepl("weibull_k",names(fit.all.years[[iyear]]))]


  best.model[[iyear]] <- fit.all.years[[iyear]][[best.model.names[iyear]]]
  pp.check.best[[iyear]] <- pp_check(best.model[[iyear]], ndraws = 500)

}

# best.model[[1]] <- fit.all.years[[iyear]][[1]]
# plot_collection <- grid.arrange(grobs = pp.check.best, nrow = 1, ncol = length(pp.check.best))
#
# ce <- liana.effect <-
#   list()
#
# for (iyear in seq(1,length(years))){
#
#   cmodel <- fit.all.years[[iyear]][[best.model.names[iyear]]]
#   ce[[iyear]] <- plot(conditional_effects(cmodel,"dbh:liana.cat",
#                                           points = TRUE) ,plot = FALSE)[[1]] +
#     theme_bw() +
#     scale_y_continuous(limits = c(2.5,4)) +
#     ggtitle(paste("year of data collection:",years[iyear]))
#
#
#   liana.effect[[iyear]] <- mcmc_plot(cmodel, variable = "*liana*", regex = TRUE) +
#     scale_x_continuous(limits = c(-0.35,0)) +
#     ggtitle(paste("year:",years[iyear]))
# }
#
#
# plot_collection <- grid.arrange(grobs = ce,
#                                 ncol = length(years), nrow = 1)
#
# plot_collection <- grid.arrange(grobs = liana.effect,
#                                 nrow = length(years), ncol = 1)
# similar.models <- lapply(fit.all.years, function(x){
#   names.x = names(x)
#   # print(names.x)
#   return(x[[which(grepl("weibull_a$",names.x))]])})
#
null.models <- lapply(fit.all.years, function(x){
  names.x = names(x)
  return(x[[which(grepl("weibull_none",names.x))]])})
#
# posteriors <- bind_rows(lapply(similar.models,function(x){
#   temp <- as.array(x)
#   CN <- colnames(temp[,1,])
#   return(
#     melt(temp[,,grepl(paste(c("_Intercept","liana.catlow","liana.cathigh"),collapse="|"),
#                       CN)]))
# }),.id = "year") %>%
#   mutate(year = as.factor(years[as.numeric(year)])) %>%
#   rename(sample = iteration,
#          parname = variable) %>%
#   mutate(par.type = case_when(grepl("^b_*",parname) ~ "Fixed effect",
#                               TRUE ~ "Other")) %>%
#   mutate(param = case_when(par.type == "Fixed effect" ~ sub("\\_.*","",str_replace(parname,"^b_","")),
#                            TRUE ~ "Other"),
#          fac = case_when(par.type == "Fixed effect" ~ sub(".*\\_","",str_replace(parname,"^b_","")),
#                          TRUE ~ "Other"))
#
#
# ggplot(data = posteriors %>%
#          filter(fac == "Intercept")) +
#   # filter(par.type == "Fixed effect")
#   geom_density_ridges(aes(x = value, fill = fac, y = as.factor(year)),
#                       alpha = 0.3) +
#   facet_wrap(~ param, scales = "free") +
#   theme_bw()
#
# ggplot(data = posteriors %>%
#          group_by(year,param) %>%
#          mutate(rel.effect = value/mean(value[fac == "Intercept"]),
#                 category = sub("^liana.cat","",fac)) %>%
#          filter(par.type == "Fixed effect", fac != "Intercept")) +
#   geom_density_ridges(aes(x = rel.effect, fill = category, y = as.factor(year)),
#                       alpha = 0.3) +
#   # facet_wrap(~ param, scales = "free") +
#   geom_vline(xintercept = 0) +
#   labs(x = "Effect on a (Weibull)",
#        y = "",
#        fill = "Liana category") +
#   theme_bw() +
#   theme(text = element_text(size = 20),
#         legend.position = c(0.2,0.9))


(lapply(fit.all.years[[1]], function(x) {
  WAIC <- waic(x)
  return(WAIC$waic)}
  ))
(lapply(fit.all.years[[2]], function(x) {
  WAIC <- waic(x)
  return(WAIC$waic)}
))
(lapply(fit.all.years[[3]], function(x) {
  WAIC <- waic(x)
  return(WAIC$waic)}
))


################################################################################
alpha = 0.25

temp <- bind_rows((lapply(1:length(years),
                 function(iyear){

                   print(iyear)
                   cdf <- all.df %>%
                     filter(year == years[iyear])

                   dbhs <- seq(floor(min(cdf$dbh)),
                               ceiling(max(cdf$dbh)),
                               1)

                   levels <- as.character(unique(cdf$liana.cat))

                   newdata <- data.frame()

                   for (ilevel in seq(1,length(levels))){

                     ccdf <- cdf %>% filter(liana.cat == levels[ilevel])

                     cdbhs <- dbhs[dbhs>= min(ccdf$dbh,na.rm = TRUE) &
                                     dbhs <= max(ccdf$dbh,na.rm = TRUE)]
                     cdbhs <- seq(floor(min(ccdf$dbh,na.rm = TRUE)),
                                  ceiling(max(ccdf$dbh,na.rm = TRUE)),
                                  1)

                      newdata <- bind_rows(list(newdata,
                                               data.frame(
                                                 dbh = rep(cdbhs,1),
                                                 liana.cat = c(rep(levels[ilevel],length(cdbhs))))
                   ))
                   }

                   newdata <- newdata %>%
                     mutate(id = 1:length(dbh),
                            year = years[iyear])

                   cmodel <- best.model[[iyear]]
                   null.model <- null.models[[iyear]]

                   ccoef <- as.numeric(exp( (summary(cmodel)[["spec_pars"]][1]**2)/2))
                   ccoef.null <- as.numeric(exp(summary(null.model)[["spec_pars"]][1]**2/2))
                   # ccoef <- 1


                   pp <- melt(posterior_predict(cmodel,
                                           newdata = newdata,
                                           re_formula = NA)) %>%
                     rename(rep = Var1,
                            id = Var2) %>%
                     group_by(id) %>%
                     filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
                     mutate(h = ccoef*exp(value)) %>%
                     summarise(h.m = mean(h,na.rm = TRUE),
                               h.low = quantile(h,alpha/2,na.rm = TRUE),
                               h.high = quantile(h,1 - alpha/2,na.rm = TRUE))


                   pp.null <- melt(posterior_predict(null.model,
                                                     newdata = newdata,
                                                     re_formula = NA)) %>%
                     rename(rep = Var1,
                            id = Var2) %>%
                     group_by(id) %>%
                     filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
                     mutate(h.null = ccoef.null*exp(value)) %>%
                     summarise(h.null.m = mean(h.null,na.rm = TRUE),
                               h.null.low = quantile(h.null,alpha/2,na.rm = TRUE),
                               h.null.high = quantile(h.null,1 - alpha/2,na.rm = TRUE))


                   pep <- melt(posterior_epred(cmodel,
                                          newdata = newdata,
                                          re_formula = NA)) %>%
                     rename(rep = Var1,
                            id = Var2) %>%
                     group_by(id) %>%
                     filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
                     mutate(h.pred = ccoef*exp(value)) %>%
                     summarise(h.pred.m = mean(h.pred,na.rm = TRUE),
                               h.pred.low = quantile(h.pred,alpha/2,na.rm = TRUE),
                               h.pred.high = quantile(h.pred,1 - alpha/2,na.rm = TRUE))


                   pep.null <- melt(posterior_epred(null.model,
                                               newdata = newdata,
                                               re_formula = NA)) %>%
                     rename(rep = Var1,
                            id = Var2) %>%
                     group_by(id) %>%
                     filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
                     mutate(h.null.pred = ccoef.null*exp(value)) %>%
                     summarise(h.null.pred.m = mean(h.null.pred,na.rm = TRUE),
                               h.null.pred.low = quantile(h.null.pred,alpha/2,na.rm = TRUE),
                               h.null.pred.high = quantile(h.null.pred,1 - alpha/2,na.rm = TRUE))

                   new.data.full <- newdata

                   new.data.full[["h.m"]] <- pp[["h.m"]]
                   new.data.full[["h.low"]] <- pp[["h.low"]]
                   new.data.full[["h.high"]] <- pp[["h.high"]]

                   new.data.full[["h.pred.m"]] <- pep[["h.pred.m"]]
                   new.data.full[["h.pred.low"]] <- pep[["h.pred.low"]]
                   new.data.full[["h.pred.high"]] <- pep[["h.pred.high"]]

                   new.data.full[["h.null.m"]] <- pp.null[["h.null.m"]]
                   new.data.full[["h.null.low"]] <- pp.null[["h.null.low"]]
                   new.data.full[["h.null.high"]] <- pp.null[["h.null.high"]]

                   new.data.full[["h.null.pred.m"]] <- pep.null[["h.null.pred.m"]]
                   new.data.full[["h.null.pred.low"]] <- pep.null[["h.null.pred.low"]]
                   new.data.full[["h.null.pred.high"]] <- pep.null[["h.null.pred.high"]]

                   ggplot(data = new.data.full) +
                     geom_line(aes(x = dbh, y = h.m, color = liana.cat)) +
                     geom_line(aes(x = dbh, y = h.null.m), color = "black",linetype = 2) +
                     geom_point(data = cdf,
                                aes(x = dbh, y = h, color = liana.cat)) +
                     theme_bw()

                   return(new.data.full)
                 })))

temp.title <- temp %>%
  left_join(all.df.title %>% dplyr::select(year,year.N) %>% distinct(),
            by = "year") %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high")))


ggplot(data = temp.title) +
  geom_point(data = all.df.title %>%
               filter(year %in% years),
             aes(x = dbh,y = h, color = as.factor(liana.cat)),
             size = 0.5, alpha = 0.25) +
  # geom_ribbon(aes(x = dbh, y = h.pred.m, fill = as.factor(liana.cat),
  #                 ymin = h.pred.low, ymax = h.pred.high), color = NA, alpha = 0.5) +
  geom_line(aes(x = dbh,y = h.m, color = as.factor(liana.cat))) +

  # geom_ribbon(aes(x = dbh, y = h.null.pred.m,
  #                 ymin = h.null.pred.low, ymax = h.null.pred.high), color = NA, alpha = 0.5, fill = "darkgrey") +
  geom_line(aes(x = dbh,y = h.null.m), color = "black") +
  facet_wrap(~ year.N) +
  scale_x_log10(limits = c(10,300),
                breaks = c(10,20,50,100,200)) +
  scale_y_log10(limits = c(10,60)) +
  labs(x = "DBH (cm)", y = 'Height (m)', color = "Liana infestation", fill = "Liana infestation") +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = c(0.9,0.2))

# predict.wide <- temp %>%
#   dplyr::select(dbh,liana.cat,year,h.m) %>%
#   pivot_wider(names_from = liana.cat,
#               values_from = h.m) %>%
#   mutate(dbh.cat = factor(case_when(dbh <= 30 ~ "Small",
#                              dbh <= 60 ~ "Intermediate",
#                              TRUE ~ "Large"),
#                           levels = c("Small","Intermediate","Large"))) %>%
#   mutate(diff = high - no,
#          diff.rel = (high - no)/no)
#
# gg.dist <- ggplot(data = all.df.title %>% filter(year %in% years)) +
#   geom_sina(aes(x = year, y = dbh, color = year), size = 0.1) +
#   coord_flip() +
#   theme_minimal() +
#   guides(color = "none") +
#   labs(x = "", y = "") +
#   scale_y_continuous(breaks = c()) +
#   theme(text = element_text(size = 20),panel.grid = element_blank())
#
# gg.effect <- ggplot(data = predict.wide) +
#   geom_line(aes(x = dbh, y = -diff.rel*100, color = year)) +
#   geom_hline(yintercept = 0, linetype = 2, color = "black") +
#   theme_bw() +
#   labs(x = "DBH (cm)", y = "Relative difference in height (%)") +
#   guides(color = "none") +
#   theme(text = element_text(size = 20))
#
# plot_grid(gg.dist,gg.effect,align = "hv",rel_heights = c(1,2.5),
#           ncol = 1)
#
# predict.wide <- temp %>%
#   dplyr::select(dbh,liana.cat,h.m,year) %>%
#   pivot_wider(names_from = liana.cat,
#               values_from = h.m) %>%
#   mutate(dbh.cat = factor(case_when(dbh <= 30 ~ "Small",
#                                     dbh <= 60 ~ "Intermediate",
#                                     TRUE ~ "Large"),
#                           levels = c("Small","Intermediate","Large"))) %>%
#   mutate(diff.high = high - no,
#          diff.high.rel = (high - no)/no,
#          diff.low = low - no,
#          diff.low.rel = (low - no)/no) %>%
#   pivot_longer(cols = c(diff.high,diff.high.rel,diff.low,diff.low.rel)) %>%
#   mutate(type = case_when(grepl('rel',name) ~ "relative",
#                           TRUE ~ "absolute"),
#          liana.cat = factor(case_when(grepl("high",name) ~ "high",
#                                grepl("low",name) ~ "low",
#                                TRUE ~ "other"),
#                             levels = c("low","high")))
#
#
# ggplot(data = predict.wide %>%
#          filter(!is.na(value))) +
#   geom_line(aes(x = dbh, y = value, color = year)) +
#   facet_grid(type ~ (liana.cat),scales = "free") +
#   geom_hline(yintercept = 0, linetype = 2) +
#   theme_bw()
#
# ggplot(data = predict.wide %>%
#          filter(!is.na(value),
#                 type == "relative")) +
#   geom_line(aes(x = dbh, y = value*100, color = liana.cat)) +
#   facet_wrap( ~ (year),nrow = 1, scales = "fixed") +
#   geom_hline(yintercept = 0, linetype = 2) +
#   theme_bw()



################################################################################

# Levels <- c("no","low","high")
# temp2 <- bind_rows((lapply(1:length(years),
#                           function(iyear){
#
#                             print(iyear)
#                             cdf <- all.df %>%
#                               filter(year == years[iyear])
#
#
#                             newdata <- cdf %>%
#                               mutate(id = 1:length(dbh),
#                                      liana.cat = relevel(as.factor(as.character(liana.cat)),"no"),
#                                      year = years[iyear])
#
#                          # best_model
#                           cmodel <- best.model[[iyear]]
#                           ccoef <- as.numeric(exp(summary(cmodel)[["spec_pars"]][1]**2/2))
#                           # ccoef <- 1
#                           pp <- posterior_predict(cmodel,
#                                                   newdata = newdata,
#                                                   re_formula = NA)
#
#
#                           # Null_model
#                           null.model <- null.models[[iyear]]
#                           ccoef.null <- as.numeric(exp(summary(null.model)[["spec_pars"]][1]**2/2))
#                           # ccoef <- 1
#                           pp.null <- posterior_predict(null.model,
#                                                        newdata = newdata,
#                                                        re_formula = NA)
#
#                           new.data.full <- newdata %>%
#                             left_join(melt(pp) %>%
#                                         rename(rep = Var1,
#                                                id = Var2),
#                                       by = "id") %>%
#                             mutate(h = ccoef*exp(value)) %>%
#                             group_by(year,id,liana.cat,dbh) %>%
#                             summarise(h.m = mean(h,na.rm = TRUE),
#                                       .groups = "keep") %>%
#
#
#                             left_join(melt(pp.null) %>%
#                                         rename(rep = Var1,
#                                                id = Var2),
#                                       by = "id") %>%
#                             mutate(h.null = ccoef.null*exp(value)) %>%
#                             group_by(year,id,liana.cat,dbh) %>%
#                             summarise(h.m = unique(h.m),
#                                       h.null.m = mean(h.null,na.rm = TRUE),
#                                       .groups = "keep")
#
#
#                             return(new.data.full)
#                           })))
#
#
#
# predict.wide2 <- temp2 %>%
#   mutate(dbh.cat = factor(case_when(dbh <= 30 ~ "Small",
#                                     dbh <= 60 ~ "Intermediate",
#                                     TRUE ~ "Large"),
#                           levels = c("Small","Intermediate","Large")),
#          liana.cat = factor(as.character(liana.cat),
#                             levels = c("no","low","high"))) %>%
#   mutate(diff = h.m - h.null.m,
#          diff.rel = (h.m - h.null.m)/h.null.m)
#
#
# ggplot(data = predict.wide2) +
#   geom_boxplot(aes(x = as.factor(liana.cat), y = -diff.rel, fill = year)) +
#   geom_hline(yintercept = 0, linetype = 2, color = "black") +
#   labs(x = "") +
#   theme_bw()

saveRDS(temp.title,
        paste0("./outputs/Model.Predictions.BCI",".RDS"))

################################################################################

DBH2test <- 50

temp3 <- bind_rows((lapply(1:length(years),
                           function(iyear){

                             print(iyear)
                             cdf <- all.df %>%
                               filter(year == years[iyear])


                             levels <- as.character(unique(cdf$liana.cat))

                             newdata <- bind_rows(list(data.frame(
                               dbh = rep(DBH2test,length(levels)),
                               liana.cat = levels)))

                             newdata <- newdata %>%
                               mutate(id = 1:length(dbh),
                                      year = years[iyear])

                             cmodel <- best.model[[iyear]]
                             null.model <- null.models[[iyear]]

                             ccoef <- as.numeric(exp( (summary(cmodel)[["spec_pars"]][1]**2)/2))
                             ccoef.null <- as.numeric(exp(summary(null.model)[["spec_pars"]][1]**2/2))
                             # ccoef <- 1

                             pp <- melt(posterior_epred(cmodel,
                                                        newdata = newdata,
                                                        re_formula = NA)) %>%
                               rename(rep = Var1,
                                      id = Var2) %>%
                               left_join(newdata %>%
                                           dplyr::select(c(id,liana.cat)),
                                         by = "id") %>%
                               mutate(h = ccoef*exp(value)) %>%
                               filter((abs(value - median(value,na.rm = TRUE)) < 1.5*sd(value,na.rm = TRUE))) %>%
                               ungroup() %>%
                               dplyr::select(-c(value,id)) %>%
                               pivot_wider(names_from = liana.cat,
                                           values_from = h)

                             return(pp %>%
                                      mutate(year = years[iyear]))
                           })))



temp3.title <- temp3 %>%
  left_join(all.df.title %>% dplyr::select(year,
                                           N.low,N.high,N.tot) %>% distinct(),
            by = "year") %>%
  mutate(year.tot = paste0(year," (N = ",N.tot,")")) %>%
  mutate(high = high - no,
         low = low - no) %>%
  pivot_longer(cols = c(low,high),
               names_to = "liana.cat",
               values_to = "diff_h") %>%
  mutate(N.cat = case_when(liana.cat == "low" ~ N.low,
                           liana.cat == "high" ~ N.high,
                           TRUE ~ NA)) %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("low","high"))) %>%
  group_by(year,liana.cat) %>%
  mutate(signif_rel = case_when(quantile(diff_h/no*100,probs = 0.975,na.rm = TRUE) < 0 ~ 1,
                                quantile(diff_h/no*100,probs = 0.025,na.rm = TRUE) > 0 ~ 1,
                                TRUE ~ 0.2),
         signif_rel2 = case_when(quantile(diff_h/no*100,probs = 0.975,na.rm = TRUE) < 0 ~ 1,
                                 quantile(diff_h/no*100,probs = 0.025,na.rm = TRUE) > 0 ~ 1,
                                 TRUE ~ 0.4))

mean.cat <- temp3.title %>%
  group_by(liana.cat) %>%
  summarise(diff_h_m = mean(diff_h,na.rm = TRUE),
            diff_h_m_rel = 100*mean(diff_h/no,na.rm = TRUE),
            .groups = "keep")

ggplot(data = temp3.title %>%
         filter(year %in% years),
       aes(x = diff_h/no*100,
           y = year.tot,
           color = liana.cat,
           fill = liana.cat,
           alpha = signif_rel)) +
  geom_text(data = temp3.title %>%
              group_by(year.tot,liana.cat) %>%
              summarise(N.cat = unique(N.cat),
                        signif_rel = 1,
                        .groups = "keep"),
            aes(x = 5, label = paste("N = ",as.character(N.cat))),
            color = "black", fill = NA,hjust = 0) +
  geom_vline(xintercept = 0,linetype = 1) +
  geom_vline(data = mean.cat %>%
               mutate(signif_rel = 1),
             aes(xintercept = diff_h_m_rel,
                 color = liana.cat),
             linetype = 2) +
  # stat_interval(.width = c(.5, .8, .95)) +
  stat_halfeye(color = NA) +

  stat_pointinterval(aes(alpha = signif_rel2),
                     .width = c(.66, .95),
                     position = position_dodge(width = 0)) +
  facet_wrap(~ liana.cat) +
  scale_x_continuous(limits = c(-15,15)) +
  labs(y = "", color = "", fill = "") +
  theme_bw() +
  guides(alpha = "none")


ggplot(data = temp3.title %>%
         filter(year %in% years),
       aes(x = diff_h/no*100,
           y = year.tot,
           color = liana.cat,
           fill = liana.cat,
           alpha = signif_rel)) +
  geom_vline(xintercept = 0,linetype = 1) +
  stat_halfeye(color = NA) +

  stat_pointinterval(aes(alpha = signif_rel2),
                     .width = c(.66, .95),
                     position = position_dodge(width = 0.1)) +
  # facet_wrap(~ liana.cat) +
  scale_x_continuous(limits = c(-15,5)) +
  labs(y = "", color = "", fill = "") +
  theme_bw() +
  guides(alpha = "none")

# ggplot(data = temp3.title %>%
#          filter(year %in% years,
#                 liana.cat == "high"),
#        aes(x = diff_h,
#            y = year,
#            color = liana.cat,
#            fill = liana.cat)) +
#   geom_vline(xintercept = 0,linetype = 2) +
#   # stat_interval(.width = c(.5, .8, .95)) +
#   stat_halfeye(alpha = 0.2, color = NA) +
#
#   stat_pointinterval( .width = c(.66, .95),
#                       position = position_dodge(width = 0.1)) +
#
#   scale_x_continuous(limits = c(-20,10)) +
#   facet_wrap(~ year.group, scales = "free") +
#   theme_minimal()

saveRDS(temp3.title,
        paste0("./outputs/Main.OP.BCI",DBH2test,".RDS"))

