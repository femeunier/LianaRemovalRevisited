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
all.df <- bind_rows(readRDS("./outputs/All.COI.data.RDS") %>%
                      mutate(sp = str_squish(sp)) %>%
                      filter(dbh >= 10),
                    readRDS("./outputs/All.COI.data.RDS") %>%
                      mutate(sp = str_squish(sp)) %>%
                      filter(dbh >= 10) %>% mutate(site = "Total"))

all.df <- bind_rows(readRDS("./outputs/All.COI.data.RDS") %>%
                      mutate(sp = str_squish(sp)) %>%
                      filter(dbh >= 10))

all.df %>%
  group_by(liana.cat) %>%
  summarise(N = n())

all.df.title <- all.df %>%
  group_by(site) %>%
  mutate(site.N = paste0(site,", N = ", length(site)," (",length(site[which(liana.cat == "no")]), "-",
                         length(site[which(liana.cat == "low")]), "-",
                         length(site[which(liana.cat == "high")]), ")"),
         N.low = length(site[which(liana.cat == "low")]),
         N.high = length(site[which(liana.cat == "high")]),
         N.tot = length(site))

sites <- unique(all.df.title$site)

# # sites <- c("BUL","DAN","LAM","SGW")
# # sites <- c(readRDS("./data/rainfor2.md.RDS") %>%
# #   pull(group) %>% unique())
# # sites <- c("TGS","TAM","STN",
# #            "STL","STD","SAT","PND","PUY","PEC","JUV")
#
# # sites <- c("Casa_Roubik","BCI","group_North",
# #            "Canal","group_Metro","Gigante",
# #            "Sherman")
# #
# # sites <- c("129","357")
#
# # Compile the outputs
# fit.all.sites <- list()
#
# print("Reading")
#
# sites2keep <- best.model.names <- c()
# all.diagnosis <- data.frame()
#
# for (isite in seq(1,length(sites))){
#
#   csite <- sites[isite]
#   csite.corrected <- gsub(" ", "",csite, fixed = TRUE)
#
#
#   print(paste("-",csite,"-",isite/length(sites)))
#   fit.all.sites[[isite]] <- list()
#
#   dir.create(file.path("./outputs/",csite.corrected),
#              showWarnings = FALSE)
#
#   # # Check which files
#   transfer.files("Diagnostics.RDS",
#                  base = "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/",
#                  source = file.path("data",csite.corrected),
#                  destination = file.path("./outputs/",csite.corrected),
#                  show.progress = FALSE)
#
#
#   diagnosis.file <- file.path("./outputs/",csite.corrected,"Diagnostics.RDS")
#   if (!file.exists(diagnosis.file)) next()
#
#   raw.diagnosis <- readRDS(diagnosis.file)
#
#   all.diagnosis <- bind_rows(list(all.diagnosis,
#                                   raw.diagnosis %>%
#                                     mutate(site = csite)))
#
#   Diagnstocis.Bayesian.site <- raw.diagnosis %>%
#     arrange((waic)) %>%
#     filter(rhat.max < 1.05)
#
#   Diagnstocis.Bayesian.site.best <-
#     bind_rows(
#       Diagnstocis.Bayesian.site %>%
#         group_by(site) %>%
#         filter(fe == "none") %>%
#         filter(waic == min(waic)),
#       Diagnstocis.Bayesian.site %>%
#         group_by(site) %>%
#         filter(waic == min(waic))) %>%
#     distinct()
#
#   all.possible.files <- Diagnstocis.Bayesian.site.best %>%
#     mutate(file = paste0("Fit.",ifelse(csite.corrected == "Total.re",
#                                        "Total",
#                                        csite.corrected),".",model.name,".RDS")) %>%
#     pull(file)
#
#   # Transfer files
#   dir.create(file.path("./outputs/",csite.corrected),showWarnings = FALSE)
#   transfer.files(all.possible.files,
#                  base = "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/",
#                  source = file.path("data",csite.corrected),
#                  destination = file.path("./outputs/",csite.corrected),
#                  show.progress = FALSE)
#
#
#   cfiles <- file.path("./outputs/",csite.corrected,all.possible.files)
#
#   tokeep <- basename(cfiles) %in% all.possible.files
#
#   cfiles.filtered <- cfiles[tokeep]
#   cnames.filtered <- tools::file_path_sans_ext(cfiles.filtered)
#
#   if (length(cfiles.filtered) == 0) next()
#
#   for (ifile in seq(1,length(cfiles.filtered))){
#
#     print(paste("--",ifile/length(cfiles.filtered)))
#     fit.all.sites[[isite]][[cnames.filtered[ifile]]] <-  readRDS(paste0(cfiles.filtered[ifile]))
#   }
#
#   best.model.names[isite] <- file.path("./outputs/",csite.corrected,
#                                        paste0("Fit.",ifelse(csite.corrected == "Total.re",
#                                                             "Total",
#                                                             csite.corrected),".",Diagnstocis.Bayesian.site %>%
#     group_by(site) %>%
#     filter(waic == min(waic)) %>% pull(model.name)))
# }
#
# best.model.all <- all.diagnosis %>%
#   filter(rhat.max < 1.05) %>%
#   group_by(site) %>%
#   arrange(waic) %>%
#   slice_head(n = 1) %>%
#   dplyr::select(site,model.name)
#
# check.all.diagnosis <- all.diagnosis %>%
#  group_by(site) %>%
#  summarise(N = length(site),
#           Nsuccessful = length(which(rhat.max <= 1.05))) %>%
#   left_join(best.model.all,
#             by = "site") %>%
#  arrange((Nsuccessful)) %>%
#   left_join(all.df.title %>%
#               group_by(site) %>%
#               summarise(Ntot = length(site)) %>%
#               dplyr::select(site,Ntot),
#             by = "site")
#
# check.all.diagnosis %>%
#   filter(Nsuccessful < N) %>%
#   pull(site) %>% unique()
#
# sum(check.all.diagnosis$Nsuccessful)/sum(check.all.diagnosis$N)
#
# saveRDS(check.all.diagnosis,
#         "./outputs/check.all.diagnosis.RDS")
#
# all.diagnosis %>%
#   filter(rhat.max > 1.05) %>%
#   group_by(model.name) %>%
#   summarise(N = n()) %>%
#   arrange(desc(N))
#
#
# # Select best model
#
# actual.df <- data.frame()
#
# print("Processing")
# best.model <- list()
# for (isite in seq(1,length(sites))){
#
#   csite <- sites[isite]
#   print(paste(csite, "-",isite/length(sites)))
#
#   actual.df <- bind_rows(list(actual.df,
#                               all.df %>% filter(site == csite)))
#
#   # if (length(fit.all.sites[[isite]]) > 1){
#   #
#   #   X <- lapply(fit.all.sites[[isite]],waic)
#   #   comparison[[isite]] <- brms::loo_compare(X)
#   #   best.model.names[isite] <- rownames(comparison[[isite]])[1]
#   #
#   # } else {
#   #   comparison[[isite]] <- NULL
#   #   best.model.names[isite] <- names(fit.all.sites[[isite]])
#   # }
#
#   best.model[[isite]] <- fit.all.sites[[isite]][[best.model.names[isite]]]
#
# }
#
# alpha = 0.25
#
#
# null.models <- lapply(fit.all.sites, function(x){
#   names.x = names(x)
#   return(x[[which(grepl("_none",names.x))]])})
#
#
# # stop()
# # null.pred <- residuals(null.models[[1]],re_formula = NA)
# # best.pred <- predict(best.model[[1]])
#
# temp <- bind_rows((lapply(1:length(sites),
#                           function(isite){
#
#                             print(isite)
#                             csite <- sites[isite]
#                             cdf <- all.df %>%
#                               filter(site == csite)
#
#                             dbhs <- seq(floor(min(cdf$dbh)),
#                                         ceiling(max(cdf$dbh)),
#                                         1)
#
#                             levels <- as.character(unique(cdf$liana.cat))
#
#                             newdata <- data.frame()
#
#                             for (ilevel in seq(1,length(levels))){
#
#                               ccdf <- cdf %>% filter(liana.cat == levels[ilevel])
#
#                               cdbhs <- dbhs[dbhs>= min(ccdf$dbh,na.rm = TRUE) &
#                                               dbhs <= max(ccdf$dbh,na.rm = TRUE)]
#                               cdbhs <- seq(floor(min(ccdf$dbh,na.rm = TRUE)),
#                                            ceiling(max(ccdf$dbh,na.rm = TRUE)),
#                                            1)
#
#                               newdata <- bind_rows(list(newdata,
#                                                         data.frame(
#                                                           dbh = rep(cdbhs,1),
#                                                           liana.cat = c(rep(levels[ilevel],length(cdbhs))))
#                               ))
#                             }
#
#                             newdata <- newdata %>%
#                               mutate(id = 1:length(dbh),
#                                      site = sites[isite])
#
#                             cmodel <- best.model[[isite]]
#                             null.model <- null.models[[isite]]
#
#                             ccoef <- as.numeric(exp( (summary(cmodel)[["spec_pars"]][1]**2)/2))
#                             ccoef.null <- as.numeric(exp(summary(null.model)[["spec_pars"]][1]**2/2))
#                             # ccoef <- 1
#
#
#                             pp <- melt(posterior_predict(cmodel,
#                                                          newdata = newdata,
#                                                          re_formula = NA)) %>%
#                               rename(rep = Var1,
#                                      id = Var2) %>%
#                               group_by(id) %>%
#                               filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
#                               mutate(h = ccoef*exp(value)) %>%
#                               summarise(h.m = median(h,na.rm = TRUE),
#                                         h.low = quantile(h,alpha/2,na.rm = TRUE),
#                                         h.high = quantile(h,1 - alpha/2,na.rm = TRUE))
#
#
#                             pp.null <- melt(posterior_predict(null.model,
#                                                               newdata = newdata,
#                                                               re_formula = NA)) %>%
#                               rename(rep = Var1,
#                                      id = Var2) %>%
#                               group_by(id) %>%
#                               filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
#                               mutate(h.null = ccoef.null*exp(value)) %>%
#                               summarise(h.null.m = median(h.null,na.rm = TRUE),
#                                         h.null.low = quantile(h.null,alpha/2,na.rm = TRUE),
#                                         h.null.high = quantile(h.null,1 - alpha/2,na.rm = TRUE))
#
#
#                             pep <- melt(posterior_epred(cmodel,
#                                                         newdata = newdata,
#                                                         re_formula = NA)) %>%
#                               rename(rep = Var1,
#                                      id = Var2) %>%
#                               group_by(id) %>%
#                               filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
#                               mutate(h.pred = ccoef*exp(value)) %>%
#                               summarise(h.pred.m = median(h.pred,na.rm = TRUE),
#                                         h.pred.low = quantile(h.pred,alpha/2,na.rm = TRUE),
#                                         h.pred.high = quantile(h.pred,1 - alpha/2,na.rm = TRUE))
#
#
#                             pep.null <- melt(posterior_epred(null.model,
#                                                              newdata = newdata,
#                                                              re_formula = NA)) %>%
#                               rename(rep = Var1,
#                                      id = Var2) %>%
#                               group_by(id) %>%
#                               filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
#                               mutate(h.null.pred = ccoef.null*exp(value)) %>%
#                               summarise(h.null.pred.m = median(h.null.pred,na.rm = TRUE),
#                                         h.null.pred.low = quantile(h.null.pred,alpha/2,na.rm = TRUE),
#                                         h.null.pred.high = quantile(h.null.pred,1 - alpha/2,na.rm = TRUE))
#
#                             new.data.full <- newdata
#
#                             new.data.full[["h.m"]] <- pp[["h.m"]]
#                             new.data.full[["h.low"]] <- pp[["h.low"]]
#                             new.data.full[["h.high"]] <- pp[["h.high"]]
#
#                             new.data.full[["h.pred.m"]] <- pep[["h.pred.m"]]
#                             new.data.full[["h.pred.low"]] <- pep[["h.pred.low"]]
#                             new.data.full[["h.pred.high"]] <- pep[["h.pred.high"]]
#
#                             new.data.full[["h.null.m"]] <- pp.null[["h.null.m"]]
#                             new.data.full[["h.null.low"]] <- pp.null[["h.null.low"]]
#                             new.data.full[["h.null.high"]] <- pp.null[["h.null.high"]]
#
#                             new.data.full[["h.null.pred.m"]] <- pep.null[["h.null.pred.m"]]
#                             new.data.full[["h.null.pred.low"]] <- pep.null[["h.null.pred.low"]]
#                             new.data.full[["h.null.pred.high"]] <- pep.null[["h.null.pred.high"]]
#
#                             ggplot(data = new.data.full) +
#                               geom_line(aes(x = dbh, y = h.pred.m, color = liana.cat)) +
#                               geom_line(aes(x = dbh, y = h.null.pred.m), color = "black",linetype = 2) +
#                               geom_point(data = cdf,
#                                          aes(x = dbh, y = h, color = liana.cat)) +
#                               # scale_x_log10() +
#                               # scale_y_log10() +
#                               theme_bw()
#
#                             return(new.data.full)
#                           })))
#
# temp.title <- temp %>%
#   left_join(all.df.title %>% dplyr::select(site,site.N) %>% distinct(),
#             by = "site") %>%
#   mutate(liana.cat = factor(liana.cat,
#                             levels = c("no","low","high")))

system2("rsync",c("-avz",
                  "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/Model.predictions.RDS",
                  "./outputs/"))
temp.title <- readRDS("./outputs/Model.predictions.RDS")

ggplot(data = temp.title) +
  geom_point(data = all.df.title %>%
               filter(site %in% sites),
             aes(x = dbh,y = h, color = as.factor(liana.cat)),
             size = 0.5, alpha = 0.25) +
  # geom_ribbon(aes(x = dbh, y = h.pred.m, fill = as.factor(liana.cat),
  #                 ymin = h.pred.low, ymax = h.pred.high), color = NA, alpha = 0.5) +
  geom_line(aes(x = dbh,y = h.pred.m, color = as.factor(liana.cat))) +

  # geom_ribbon(aes(x = dbh, y = h.null.pred.m,
  #                 ymin = h.null.pred.low, ymax = h.null.pred.high), color = NA, alpha = 0.5, fill = "darkgrey") +
  geom_line(aes(x = dbh,y = h.null.pred.m), color = "black") +
  facet_wrap(~ site.N, scales = "free") +
  scale_x_log10(limits = c(10,300),
                     breaks = c(10,20,50,100,200)) +
  scale_y_log10(limits = c(1,60)) +
  labs(x = "DBH (cm)", y = 'Height (m)', color = "Liana infestation", fill = "Liana infestation") +
  theme_bw() +
  theme(text = element_text(size = 20))



################################################################################

Afritron.sites <- readRDS("./data/Afritron/Afritron.metadata.RDS") %>%
  pull(site)

alpha <- 0.11

# for (DBH2test in c(25,100,150,50)){
#
#   print(DBH2test)
#
#   temp3 <- bind_rows((lapply(1:length(sites),
#                              function(isite){
#
#                                print(isite)
#                                cdf <- all.df %>%
#                                  filter(site == sites[isite])
#
#
#                                levels <- as.character(unique(cdf$liana.cat))
#
#                                newdata <- bind_rows(list(data.frame(
#                                  dbh = rep(DBH2test,length(levels)),
#                                  liana.cat = levels)))
#
#                                newdata <- newdata %>%
#                                  mutate(id = 1:length(dbh),
#                                         site = sites[isite])
#
#                                cmodel <- best.model[[isite]]
#                                null.model <- null.models[[isite]]
#
#                                ccoef <- as.numeric(exp( (summary(cmodel)[["spec_pars"]][1]**2)/2))
#                                ccoef.null <- as.numeric(exp(summary(null.model)[["spec_pars"]][1]**2/2))
#                                # ccoef <- 1
#
#                                pp <- melt(posterior_epred(cmodel,
#                                                           newdata = newdata,
#                                                           re_formula = NA)) %>%
#                                  rename(rep = Var1,
#                                         id = Var2) %>%
#                                  left_join(newdata %>%
#                                              dplyr::select(c(id,liana.cat)),
#                                            by = "id") %>%
#                                  mutate(h = ccoef*exp(value)) %>%
#                                  filter((abs(value - median(value,na.rm = TRUE)) < 1.5*sd(value,na.rm = TRUE))) %>%
#                                  ungroup() %>%
#                                  dplyr::select(-c(value,id)) %>%
#                                  pivot_wider(names_from = liana.cat,
#                                              values_from = h)
#
#                                return(pp %>%
#                                         mutate(site = sites[isite]))
#                              })))
#
#   temp3.title <- temp3 %>%
#     left_join(all.df.title %>% dplyr::select(site,
#                                              N.low,N.high,N.tot) %>% distinct(),
#               by = "site") %>%
#     mutate(site.tot = paste0(site," (N = ",N.tot,")")) %>%
#     mutate(high = high - no,
#            low = low - no) %>%
#     pivot_longer(cols = c(low,high),
#                  names_to = "liana.cat",
#                  values_to = "diff_h") %>%
#     mutate(N.cat = case_when(liana.cat == "low" ~ N.low,
#                              liana.cat == "high" ~ N.high,
#                              TRUE ~ NA)) %>%
#     mutate(site.group = case_when(site %in% c("Pasoh","Danum Valley","Australia",
#                                               "SGW","LAM","DAN","BUL") ~ "Australasia",
#                                   site %in% c("Sand-F","Semi-F","Atla-F","Loundoungou",Afritron.sites,"Tanzania","OKU") ~ "Africa",
#                                   site %in% c("Gigante","BCI",
#                                               "group_North",
#                                               "Casa_Roubik",
#                                               "Sherman",
#                                               "group_Metro",
#                                               "Canal") ~ "Panama",
#                                   site == c("Total") ~ "Total",
#                                   TRUE ~ "Amazon")) %>%
#     mutate(liana.cat = factor(liana.cat,
#                               levels = c("low","high"))) %>%
#     mutate(site.group = factor(site.group,
#                                levels = c("Total","Panama",
#                                           "Amazon","Africa","Australasia"))) %>%
#     group_by(site,liana.cat) %>%
#     mutate(signif_rel = case_when(quantile(diff_h/no*100,probs = 1-alpha/2,na.rm = TRUE) < 0 ~ 0.3,
#                                   quantile(diff_h/no*100,probs = alpha/2,na.rm = TRUE) > 0 ~ 0.3,
#                                   TRUE ~ 0.2),
#            signif_rel2 = case_when(quantile(diff_h/no*100,probs = 1-alpha/2,na.rm = TRUE) < 0 ~ 1,
#                                    quantile(diff_h/no*100,probs = alpha/2,na.rm = TRUE) > 0 ~ 1,
#                                    TRUE ~ 0.4))
#
#   saveRDS(
#     temp3.title,paste0("./outputs/Main.OP.",DBH2test,".RDS"))
# }
#
#

system2("rsync",c("-avz",
                  "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/Main.OP.*",
                  "./outputs/"))
temp3.title <- readRDS("./outputs/Main.OP.50.RDS")

mean.cat <- temp3.title %>%
  group_by(liana.cat) %>%
  summarise(diff_h_m = mean(diff_h,na.rm = TRUE),
            diff_h_m_rel = 100*mean(diff_h/no,na.rm = TRUE),
            .groups = "keep")

ggplot(data = temp3.title %>%
         filter(site %in% sites),
       aes(x = diff_h/no*100,
           y = site.tot,
           color = liana.cat,
           fill = liana.cat,
           alpha = signif_rel)) +
  # geom_text(data = temp3.title %>%
  #              group_by(site.tot,liana.cat) %>%
  #              summarise(N.cat = unique(N.cat),
  #                        signif_rel = 1,
  #                        .groups = "keep"),
  #            aes(x = 25, label = paste("N = ",as.character(N.cat))),
  #            color = "black", fill = NA,hjust = 0) +
  geom_vline(xintercept = 0,linetype = 1) +
  geom_vline(data = mean.cat %>%
               mutate(signif_rel = 1),
             aes(xintercept = diff_h_m_rel,
                 color = liana.cat),
             linetype = 2) +
  # stat_interval(.width = c(.5, .8, .95)) +
  stat_halfeye(color = NA) +

  stat_pointinterval(aes(alpha = signif_rel2),
                     .width = c(1-alpha),
                     position = position_dodge(width = 0)) +
  facet_wrap(~ liana.cat) +
  # scale_x_continuous(limits = c(-100,60)) +
  labs(y = "", color = "", fill = "") +
  theme_bw() +
  guides(alpha = "none")

temp3.title %>%
  group_by(liana.cat) %>%
  summarise(m = median(diff_h/no*100,na.rm = TRUE))

ggplot(data = temp3.title %>%
         filter(site %in% sites),
       aes(x = diff_h/no*100,
           y = site.tot,
           color = liana.cat,
           fill = liana.cat,
           alpha = signif_rel)) +
  geom_vline(xintercept = 0,linetype = 1) +
  stat_halfeye(color = NA) +

  stat_pointinterval(aes(alpha = signif_rel2),
                     .width = c(1-alpha),
                     position = position_dodge(width = 0)) +
  facet_wrap(~ liana.cat) +
  scale_x_continuous(limits = c(-45,25)) +
  labs(y = "", color = "", fill = "") +
  theme_bw() +
  facet_wrap(~ site.group, scales = "free_y") +
  guides(alpha = "none") +
  theme(legend.position = c(0.1,0.2))

ggplot(data = temp3.title %>%
         filter(site %in% sites),
       aes(x = diff_h,
           y = site,
           color = liana.cat,
           fill = liana.cat)) +
  geom_vline(xintercept = 0,linetype = 1) +
  geom_vline(data = mean.cat,
             aes(xintercept = diff_h_m,
                 color = liana.cat),
             linetype = 2) +
  # stat_interval(.width = c(.5, .8, .95)) +
  stat_halfeye(alpha = 0.2, color = NA) +

  stat_pointinterval( .width = c(1-alpha),
                      position = position_dodge(width = 0.1)) +
  facet_wrap(~ liana.cat) +
  # scale_x_continuous(limits = c(-20,10)) +
  theme_bw()

