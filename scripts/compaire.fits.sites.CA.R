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
library(reshape2)

# Load the data
all.df <- readRDS("./outputs/All.CA.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10)

all.df %>%
  group_by(site,liana.cat) %>%
  summarise(N = n(),
            .groups = "keep") %>%
  pivot_wider(names_from = liana.cat,
              values_from = N) %>%
  mutate(ratio.no = 100*no/sum(c(no,low,high)),
         ratio.low = 100*low/sum(c(no,low,high)),
         ratio.high = 100*high/sum(c(no,low,high)))

all.df %>%
  group_by(liana.cat) %>%
  summarise(N = n())

all.df <- bind_rows(all.df,
                    all.df %>% mutate(site = "Total"))

all.df.title <- all.df %>%
  group_by(site) %>%
  mutate(site.N = paste0(site,", N = ", length(site)," (",length(site[which(liana.cat == "no")]), "-",
                         length(site[which(liana.cat == "low")]), "-",
                         length(site[which(liana.cat == "high")]), ")"),
         N.low = length(site[which(liana.cat == "low")]),
         N.high = length(site[which(liana.cat == "high")]),
         N.tot = length(site))


ggplot(data = all.df.title,
       aes(x = dbh, y = area,
           color = as.factor(liana.cat),
           fill = as.factor(liana.cat))) +
  geom_point() +
  stat_smooth(method = "lm",
              se = TRUE) +
  scale_x_log10(limits = c(1,1000)) +
  scale_y_log10(limits = c(1,2000)) +
  facet_wrap(~ site, nrow = 1) +
  theme_bw()

sites <- unique(all.df$site)
# sites <- "Total"
# sites <- "BCI"

models <- c("power")
model.forms <- c("a","none","all","b")

# Compile the outputs
fit.all.sites <- list()

print("Reading")

sites2keep <- c()
df.rhat <- data.frame()
for (isite in seq(1,length(sites))){

  csite <- sites[isite]
  csite.corrected <- gsub(" ", "",csite, fixed = TRUE)

  print(paste("-",csite))
  fit.all.sites[[isite]] <- list()

  all.possible.files <- crossing(sites[isite], models,model.forms) %>%
    mutate(n = paste0("Fit.",
                      as.character(csite.corrected),
                      ".",
                      as.character(models),
                      "_",
                      as.character(model.forms),
                      ".CA.RDS")) %>%
    pull(n)

  # Transfer files
  dir.create(file.path("./outputs/",csite.corrected),showWarnings = FALSE)
  transfer.files("*CA.RDS",
                 base = "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/",
                 source = file.path("data",csite.corrected),
                 destination = file.path("./outputs/",csite.corrected),
    show.progress = TRUE)

  cfiles <- list.files(file.path("./outputs/",csite.corrected),full.names = TRUE,pattern = "*CA.RDS")

  tokeep <- basename(cfiles) %in% all.possible.files

  cfiles.filtered <- cfiles[tokeep]
  cnames.filtered <- tools::file_path_sans_ext(cfiles.filtered)

  if (length(cfiles.filtered) == 0) next()

  for (ifile in seq(1,length(cfiles.filtered))){

    print(paste("--",ifile/length(cfiles.filtered)))
    fit.all.sites[[isite]][[cnames.filtered[ifile]]] <-  readRDS(paste0(cfiles.filtered[ifile]))

    # check
    all.rhat <- rhat(fit.all.sites[[isite]][[cnames.filtered[ifile]]])

    df.rhat <- bind_rows(list(
      df.rhat,
      data.frame(site = csite,
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

for (isite in seq(1,length(sites))){

  print(isite/length(sites))

  csite <- sites[isite]
  csite.corrected <- gsub(" ", "",csite, fixed = TRUE)

  actual.df <- bind_rows(list(actual.df,
                              all.df %>% filter(site == csite)))


  if (length(fit.all.sites[[isite]]) > 1){
    X <- (lapply(fit.all.sites[[isite]], LOO,r_eff = NA))
    comparison[[isite]] <- brms::loo_compare(X)
    best.model.names[isite] <- rownames(comparison[[isite]])[1]
  } else {
    comparison[[isite]] <- NULL
    best.model.names[isite] <- names(fit.all.sites[[isite]])
  }

  saveRDS(comparison[[isite]],paste0("./outputs/",csite.corrected,"/Diagnostics.CA.RDS"))
  best.model[[isite]] <- fit.all.sites[[isite]][[best.model.names[isite]]]
  pp.check.best[[isite]] <- pp_check(best.model[[isite]], ndraws = 500)

}


# best.model[[1]] <- fit.all.sites[[isite]][[1]]
# plot_collection <- grid.arrange(grobs = pp.check.best, nrow = 1, ncol = length(pp.check.best))
#
# ce <- liana.effect <-
#   list()
#
# for (isite in seq(1,length(sites))){
#
#   cmodel <- fit.all.sites[[isite]][[best.model.names[isite]]]
#   ce[[isite]] <- plot(conditional_effects(cmodel,"dbh:liana.cat",
#                                           points = TRUE) ,plot = FALSE)[[1]] +
#     theme_bw() +
#     scale_y_continuous(limits = c(2.5,4)) +
#     ggtitle(paste("site of data collection:",sites[isite]))
#
#
#   liana.effect[[isite]] <- mcmc_plot(cmodel, variable = "*liana*", regex = TRUE) +
#     scale_x_continuous(limits = c(-0.35,0)) +
#     ggtitle(paste("Site:",sites[isite]))
# }
#
#
# plot_collection <- grid.arrange(grobs = ce,
#                                 ncol = length(sites), nrow = 1)
#
# plot_collection <- grid.arrange(grobs = liana.effect,
#                                 nrow = length(sites), ncol = 1)
# similar.models <- lapply(fit.all.sites, function(x){
#   names.x = names(x)
#   # print(names.x)
#   return(x[[which(grepl("weibull_a$",names.x))]])})

null.models <- lapply(fit.all.sites, function(x){
  names.x = names(x)
  return(x[[which(grepl("power_none",names.x))]])})

# posteriors <- bind_rows(lapply(similar.models,function(x){
#   temp <- as.array(x)
#   CN <- colnames(temp[,1,])
#   return(
#     melt(temp[,,grepl(paste(c("_Intercept","liana.catlow","liana.cathigh"),collapse="|"),
#                       CN)]))
# }),.id = "site") %>%
#   mutate(site = as.factor(sites[as.numeric(site)])) %>%
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
#   geom_density_ridges(aes(x = value, fill = fac, y = as.factor(site)),
#                       alpha = 0.3) +
#   facet_wrap(~ param, scales = "free") +
#   theme_bw()
#
# ggplot(data = posteriors %>%
#          group_by(site,param) %>%
#          mutate(rel.effect = value/mean(value[fac == "Intercept"]),
#                 category = sub("^liana.cat","",fac)) %>%
#          filter(par.type == "Fixed effect", fac != "Intercept")) +
#   geom_density_ridges(aes(x = rel.effect, fill = category, y = as.factor(site)),
#                       alpha = 0.3) +
#   # facet_wrap(~ param, scales = "free") +
#   geom_vline(xintercept = 0) +
#   labs(x = "Effect on a (Weibull)",
#        y = "",
#        fill = "Liana category") +
#   theme_bw() +
#   theme(text = element_text(size = 20),
#         legend.position = c(0.2,0.9))

################################################################################
alpha = 0.25

temp <- bind_rows((lapply(1:length(sites),
                 function(isite){

                   print(isite)
                   cdf <- all.df %>%
                     filter(site == sites[isite])

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
                            site.group = sites[isite])

                   cmodel <- best.model[[isite]]
                   null.model <- null.models[[isite]]

                   ccoef <- as.numeric(exp( (summary(cmodel)[["spec_pars"]][1]**2)/2))
                   ccoef.null <- as.numeric(exp(summary(null.model)[["spec_pars"]][1]**2/2))
                   # ccoef <- 1


                   # pp <- melt(posterior_predict(cmodel,
                   #                         newdata = newdata,
                   #                         re_formula = NA)) %>%
                   #   rename(rep = Var1,
                   #          id = Var2) %>%
                   #   group_by(id) %>%
                   #   filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
                   #   mutate(CA = ccoef*exp(value)) %>%
                   #   summarise(CA.m = median(CA,na.rm = TRUE),
                   #             CA.low = quantile(CA,alpha/2,na.rm = TRUE),
                   #             CA.high = quantile(CA,1 - alpha/2,na.rm = TRUE))
                   #
                   #
                   # pp.null <- melt(posterior_predict(null.model,
                   #                                   newdata = newdata,
                   #                                   re_formula = NA)) %>%
                   #   rename(rep = Var1,
                   #          id = Var2) %>%
                   #   group_by(id) %>%
                   #   filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
                   #   mutate(CA.null = ccoef.null*exp(value)) %>%
                   #   summarise(CA.null.m = median(CA.null,na.rm = TRUE),
                   #             CA.null.low = quantile(CA.null,alpha/2,na.rm = TRUE),
                   #             CA.null.high = quantile(CA.null,1 - alpha/2,na.rm = TRUE))


                   pep <- melt(posterior_epred(cmodel,
                                          newdata = newdata,
                                          re_formula = NA)) %>%
                     rename(rep = Var1,
                            id = Var2) %>%
                     group_by(id) %>%
                     filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
                     mutate(CA.pred = ccoef*exp(value)) %>%
                     summarise(CA.pred.m = median(CA.pred,na.rm = TRUE),
                               CA.pred.low = quantile(CA.pred,alpha/2,na.rm = TRUE),
                               CA.pred.high = quantile(CA.pred,1 - alpha/2,na.rm = TRUE))


                   pep.null <- melt(posterior_epred(null.model,
                                               newdata = newdata,
                                               re_formula = NA)) %>%
                     rename(rep = Var1,
                            id = Var2) %>%
                     group_by(id) %>%
                     filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
                     mutate(CA.null.pred = ccoef.null*exp(value)) %>%
                     summarise(CA.null.pred.m = median(CA.null.pred,na.rm = TRUE),
                               CA.null.pred.low = quantile(CA.null.pred,alpha/2,na.rm = TRUE),
                               CA.null.pred.high = quantile(CA.null.pred,1 - alpha/2,na.rm = TRUE))

                   new.data.full <- newdata

#                    new.data.full[["CA.m"]] <- pp[["CA.m"]]
#                    new.data.full[["CA.low"]] <- pp[["CA.low"]]
#                    new.data.full[["CA.high"]] <- pp[["CA.high"]]

                   new.data.full[["CA.pred.m"]] <- pep[["CA.pred.m"]]
                   new.data.full[["CA.pred.low"]] <- pep[["CA.pred.low"]]
                   new.data.full[["CA.pred.high"]] <- pep[["CA.pred.high"]]

                   # new.data.full[["CA.null.m"]] <- pp.null[["CA.null.m"]]
                   # new.data.full[["CA.null.low"]] <- pp.null[["CA.null.low"]]
                   # new.data.full[["CA.null.high"]] <- pp.null[["CA.null.high"]]

                   new.data.full[["CA.null.pred.m"]] <- pep.null[["CA.null.pred.m"]]
                   new.data.full[["CA.null.pred.low"]] <- pep.null[["CA.null.pred.low"]]
                   new.data.full[["CA.null.pred.high"]] <- pep.null[["CA.null.pred.high"]]

                   ggplot(data = new.data.full) +
                     geom_line(aes(x = dbh, y = CA.pred.m, color = liana.cat)) +
                     geom_line(aes(x = dbh, y = CA.null.pred.m), color = "black",linetype = 2) +
                     geom_point(data = cdf,
                                aes(x = dbh, y = area, color = liana.cat)) +
                     scale_x_log10() +
                     scale_y_log10() +
                     theme_bw()

                   return(new.data.full)
                 })))

temp.title <- temp %>%
  left_join(all.df.title %>%
              rename(site.group = site) %>%
              dplyr::select(site.group,site.N) %>% distinct(),
            by = "site.group") %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high")))


ggplot(data = temp.title) +
  geom_point(data = all.df.title %>%
               filter(site %in% sites),
             aes(x = dbh,y = area, color = as.factor(liana.cat)),
             size = 0.5, alpha = 0.25) +
  # geom_ribbon(aes(x = dbh, y = CA.pred.m, fill = as.factor(liana.cat),
  #                 ymin = CA.pred.low, ymax = CA.pred.high), color = NA, alpha = 0.5) +
  geom_line(aes(x = dbh,y = CA.pred.m, color = as.factor(liana.cat))) +

  # geom_ribbon(aes(x = dbh, y = CA.null.pred.m,
  #                 ymin = CA.null.pred.low, ymax = CA.null.pred.high), color = NA, alpha = 0.5, fill = "darkgrey") +
  geom_line(aes(x = dbh,y = CA.null.pred.m), color = "black") +
  facet_wrap(~ site.N) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "DBH (cm)", y = 'Crown area (m)', color = "Liana infestation", fill = "Liana infestation") +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = c(0.9,0.1))


saveRDS(temp.title,
        "./outputs/Model.predictions.CA.RDS")

# predict.wide <- temp %>%
#   dplyr::select(dbh,liana.cat,site,CA.m) %>%
#   pivot_wider(names_from = liana.cat,
#               values_from = CA.m) %>%
#   mutate(dbCA.cat = factor(case_when(dbh <= 30 ~ "Small",
#                              dbh <= 60 ~ "Intermediate",
#                              TRUE ~ "Large"),
#                           levels = c("Small","Intermediate","Large"))) %>%
#   mutate(diff = high - no,
#          diff.rel = (high - no)/no)
#
# gg.dist <- ggplot(data = all.df.title %>% filter(site %in% sites)) +
#   geom_sina(aes(x = site, y = dbh, color = site), size = 0.1) +
#   coord_flip() +
#   theme_minimal() +
#   guides(color = "none") +
#   labs(x = "", y = "") +
#   scale_y_continuous(breaks = c()) +
#   theme(text = element_text(size = 20),panel.grid = element_blank())
#
# gg.effect <- ggplot(data = predict.wide) +
#   geom_line(aes(x = dbh, y = -diff.rel*100, color = site)) +
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
#   dplyr::select(dbh,liana.cat,CA.m,site) %>%
#   pivot_wider(names_from = liana.cat,
#               values_from = CA.m) %>%
#   mutate(dbCA.cat = factor(case_when(dbh <= 30 ~ "Small",
#                                     dbh <= 60 ~ "Intermediate",
#                                     TRUE ~ "Large"),
#                           levels = c("Small","Intermediate","Large"))) %>%
#   mutate(diff.high = high - no,
#          diff.higCA.rel = (high - no)/no,
#          diff.low = low - no,
#          diff.low.rel = (low - no)/no) %>%
#   pivot_longer(cols = c(diff.high,diff.higCA.rel,diff.low,diff.low.rel)) %>%
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
#   geom_line(aes(x = dbh, y = value, color = site)) +
#   facet_grid(type ~ (liana.cat),scales = "free") +
#   geom_hline(yintercept = 0, linetype = 2) +
#   theme_bw()
#
# ggplot(data = predict.wide %>%
#          filter(!is.na(value),
#                 type == "relative")) +
#   geom_line(aes(x = dbh, y = value*100, color = liana.cat)) +
#   facet_wrap( ~ (site),nrow = 1, scales = "fixed") +
#   geom_hline(yintercept = 0, linetype = 2) +
#   theme_bw()



################################################################################

# Levels <- c("no","low","high")
# temp2 <- bind_rows((lapply(1:length(sites),
#                           function(isite){
#
#                             print(isite)
#                             cdf <- all.df %>%
#                               filter(site == sites[isite])
#
#
#                             newdata <- cdf %>%
#                               mutate(id = 1:length(dbh),
#                                      liana.cat = relevel(as.factor(as.character(liana.cat)),"no"),
#                                      site = sites[isite])
#
#                          # best_model
#                           cmodel <- best.model[[isite]]
#                           ccoef <- as.numeric(exp(summary(cmodel)[["spec_pars"]][1]**2/2))
#                           # ccoef <- 1
#                           pp <- posterior_predict(cmodel,
#                                                   newdata = newdata,
#                                                   re_formula = NA)
#
#
#                           # Null_model
#                           null.model <- null.models[[isite]]
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
#                             group_by(site,id,liana.cat,dbh) %>%
#                             summarise(CA.m = mean(h,na.rm = TRUE),
#                                       .groups = "keep") %>%
#
#
#                             left_join(melt(pp.null) %>%
#                                         rename(rep = Var1,
#                                                id = Var2),
#                                       by = "id") %>%
#                             mutate(CA.null = ccoef.null*exp(value)) %>%
#                             group_by(site,id,liana.cat,dbh) %>%
#                             summarise(CA.m = unique(CA.m),
#                                       CA.null.m = mean(CA.null,na.rm = TRUE),
#                                       .groups = "keep")
#
#
#                             return(new.data.full)
#                           })))
#
#
#
# predict.wide2 <- temp2 %>%
#   mutate(dbCA.cat = factor(case_when(dbh <= 30 ~ "Small",
#                                     dbh <= 60 ~ "Intermediate",
#                                     TRUE ~ "Large"),
#                           levels = c("Small","Intermediate","Large")),
#          liana.cat = factor(as.character(liana.cat),
#                             levels = c("no","low","high"))) %>%
#   mutate(diff = CA.m - CA.null.m,
#          diff.rel = (CA.m - CA.null.m)/CA.null.m)
#
#
# ggplot(data = predict.wide2) +
#   geom_boxplot(aes(x = as.factor(liana.cat), y = -diff.rel, fill = site)) +
#   geom_hline(yintercept = 0, linetype = 2, color = "black") +
#   labs(x = "") +
#   theme_bw()

################################################################################
alpha <- 0.11
DBH2test <- 25

temp3 <- bind_rows((lapply(1:length(sites),
                           function(isite){

                             print(isite)
                             cdf <- all.df %>%
                               filter(site == sites[isite])


                             levels <- as.character(unique(cdf$liana.cat))

                             newdata <- bind_rows(list(data.frame(
                               dbh = rep(DBH2test,length(levels)),
                               liana.cat = levels)))

                             newdata <- newdata %>%
                               mutate(id = 1:length(dbh),
                                      site = sites[isite])

                             cmodel <- best.model[[isite]]
                             null.model <- null.models[[isite]]

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
                               mutate(CA = ccoef*exp(value)) %>%
                               filter((abs(value - median(value,na.rm = TRUE)) < 1.5*sd(value,na.rm = TRUE))) %>%
                               ungroup() %>%
                               dplyr::select(-c(value,id)) %>%
                               pivot_wider(names_from = liana.cat,
                                           values_from = CA)

                             return(pp %>%
                                      mutate(site = sites[isite]))
                           })))

temp3.title <- temp3 %>%
  left_join(all.df.title %>% dplyr::select(site,
                                           N.low,N.high,N.tot) %>% distinct(),
            by = "site") %>%
  mutate(site.tot = paste0(site," (N = ",N.tot,")")) %>%
  mutate(high = high - no,
         low = low - no) %>%
  pivot_longer(cols = c(low,high),
               names_to = "liana.cat",
               values_to = "diff_CA") %>%
  mutate(N.cat = case_when(liana.cat == "low" ~ N.low,
                           liana.cat == "high" ~ N.high,
                           TRUE ~ NA)) %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("low","high"))) %>%
  group_by(site,liana.cat) %>%
  mutate(signif_rel = case_when(quantile(diff_CA/no*100,probs = 1-alpha/2,na.rm = TRUE) < 0 ~ 0.3,
                                quantile(diff_CA/no*100,probs = alpha/2,na.rm = TRUE) > 0 ~ 0.3,
                                TRUE ~ 0.2),
         signif_rel2 = case_when(quantile(diff_CA/no*100,probs = 1-alpha/2,na.rm = TRUE) < 0 ~ 1,
                                 quantile(diff_CA/no*100,probs = alpha/2,na.rm = TRUE) > 0 ~ 1,
                                 TRUE ~ 0.4))

mean.cat <- temp3.title %>%
  group_by(liana.cat) %>%
  summarise(diff_CA_m = mean(diff_CA,na.rm = TRUE),
            diff_CA_m_rel = 100*mean(diff_CA/no,na.rm = TRUE),
            .groups = "keep")

ggplot(data = temp3.title %>%
         filter(site %in% sites),
       aes(x = diff_CA/no*100,
           y = site.tot,
           color = liana.cat,
           fill = liana.cat,
           alpha = signif_rel)) +
  geom_text(data = temp3.title %>%
              group_by(site.tot,liana.cat) %>%
              summarise(N.cat = unique(N.cat),
                        signif_rel = 1,
                        .groups = "keep"),
            aes(x = 40, label = paste("N = ",as.character(N.cat))),
            color = "black", fill = NA,hjust = 0) +
  geom_vline(xintercept = 0,linetype = 1) +
  geom_vline(data = mean.cat %>%
               mutate(signif_rel = 1),
             aes(xintercept = diff_CA_m_rel,
                 color = liana.cat),
             linetype = 2) +
  # stat_interval(.width = c(.5, .8, .95)) +
  stat_halfeye(color = NA) +

  stat_pointinterval(aes(alpha = signif_rel2),
                     .width = c(.66, .95),
                     position = position_dodge(width = 0)) +
  facet_wrap(~ liana.cat) +
  # scale_x_continuous(limits = c(-45,30)) +
  labs(y = "", color = "", fill = "") +
  theme_bw() +
  guides(alpha = "none",
         fill = "none", color = "none")



ggplot(data = temp3.title %>%
         filter(site %in% sites),
       aes(x = diff_CA/no*100,
           y = site.tot,
           color = liana.cat,
           fill = liana.cat,
           alpha = signif_rel)) +
  geom_vline(xintercept = 0,linetype = 1) +
  # stat_interval(.width = c(.5, .8, .95)) +
  stat_halfeye(color = NA) +

  stat_pointinterval(aes(alpha = signif_rel2),
                     .width = c(1-alpha),
                     position = position_dodge(width = 0.1)) +
  # scale_x_continuous(limits = c(-45,30)) +
  labs(y = "", color = "", fill = "") +
  theme_bw() +
  guides(alpha = "none",
         fill = "none", color = "none")


saveRDS(temp3.title,
        paste0("./outputs/Model.predictions.CA.",DBH2test,".RDS"))


#
# RE <- ranef(best.model[[1]])
#
# df.re <- melt(RE$site) %>%
#   rename(site = Var1,
#          type = Var2,
#          param = Var3) %>%
#   pivot_wider(values_from = value,
#               names_from = type)
#
# ggplot(data = df.re,
#        aes(x = site, y = Estimate, ymin = Q2.5, ymax = Q97.5 )) +
#   geom_point() +
#   geom_errorbar(width = 0) +
#   geom_hline(yintercept = 0,
#              linetype = 2) +
#
#   facet_wrap(~ param) +
#
#   coord_flip() +
#   theme_bw()
#
