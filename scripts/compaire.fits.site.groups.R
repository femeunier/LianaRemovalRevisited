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
all.df <- readRDS("./outputs/All.COI.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10)


Afritron.sites <- readRDS("./data/Afritron/Afritron.metadata.RDS") %>%
  pull(site)

all.df.title <- all.df %>%
  mutate(site.group = case_when(site == "Australia" ~ "Australia" ,
                                site %in% c("Pasoh","Danum Valley") ~ "SEA",
                                site %in% c("Sand-F","Semi-F","Atla-F","Loundoungou",Afritron.sites) ~ "Africa",
                                site %in% c("Gigante","BCI") ~ "Panama",
                                TRUE ~ "Amazon")) %>%
  mutate(site.group = factor(site.group,
                             levels = c("Panama","Amazon","Africa","SEA","Australasia"))) %>%
  group_by(site.group) %>%
  mutate(site.group.N = paste0(site.group,", N = ", length(site.group)," (",length(site.group[which(liana.cat == "no")]), "-",
                         length(site.group[which(liana.cat == "low")]), "-",
                         length(site.group[which(liana.cat == "high")]), ")"),
         N.low = length(site.group[which(liana.cat == "low")]),
         N.high = length(site.group[which(liana.cat == "high")]),
         N.tot = length(site.group))

ggplot(data = all.df.title,
       aes(x = dbh, y = h, color = as.factor(liana.cat))) +
  geom_point(size = 0.2,
             alpha = 0.4) +
  stat_smooth(method = "lm",
              se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ site.group.N, nrow = 1) +
  theme_bw()

ggplot(data = all.df.title %>%
         filter(site.group == "Amazon"),
       aes(x = dbh, y = h, color = as.factor(liana.cat))) +
  geom_point(size = 0.2,
             alpha = 0.4) +
  stat_smooth(method = "lm",
              se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ site) +
  theme_bw()

stop()

site.groups <- unique(site.group)

models <- c("weibull")
model.forms <- c("all","none","a")

# Compile the outputs
fit.all.site.groups <- list()

print("Reading")

sites2keep <- c()
for (isite.group in seq(1,length(site.groups))){

  csite.group <- site.groups[isite.group]
  csite.group.corrected <- gsub(" ", "",csite.group, fixed = TRUE)

  print(paste("-",csite.group))
  fit.all.site.groups[[isite.group]] <- list()

  all.possible.files <- crossing(site.groups[isite.group], models,model.forms) %>%
    mutate(n = paste0("Fit.",
                      as.character(csite.group.corrected),
                      ".",
                      as.character(models),
                      "_",
                      as.character(model.forms),
                      ".RDS")) %>%
    pull(n)

  # Transfer files
  dir.create(file.path("./outputs/",csite.group.corrected),showWarnings = FALSE)
  transfer.files("*.RDS",
                 base = "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/",
                 source = file.path("data",csite.group.corrected),
                 destination = file.path("./outputs/",csite.group.corrected),
    show.progress = TRUE)

  cfiles <- list.files(file.path("./outputs/",csite.group.corrected),full.names = TRUE,pattern = "*.RDS")

  tokeep <- basename(cfiles) %in% all.possible.files

  cfiles.filtered <- cfiles[tokeep]
  cnames.filtered <- tools::file_path_sans_ext(cfiles.filtered)

  if (length(cfiles.filtered) == 0) next()

  for (ifile in seq(1,length(cfiles.filtered))){

    print(paste("--",ifile/length(cfiles.filtered)))
    fit.all.site.groups[[isite.group]][[cnames.filtered[ifile]]] <-  readRDS(paste0(cfiles.filtered[ifile]))
  }
}

# Select best model

comparison <- best.model <- pp.check.best <- pp.check.wb <- list()
best.model.names <- c()

actual.df <- data.frame()

print("Processing")

for (isite.group in seq(1,length(site.groups))){

  print(isite.group/length(site.groups))

  csite.group <- site.groups[isite.group]
  actual.df <- bind_rows(list(actual.df,
                              all.df %>% filter(site.group == csite.group)))


  if (length(fit.all.site.groups[[isite.group]]) > 1){
    X <- (lapply(fit.all.site.groups[[isite.group]], LOO,r_eff = NA))
    comparison[[isite.group]] <- brms::loo_compare(X)
    best.model.names[isite.group] <- rownames(comparison[[isite.group]])[1]
  } else {
    comparison[[isite.group]] <- NULL
    best.model.names[isite.group] <- names(fit.all.site.groups[[isite.group]])
  }

  # best.model.names[isite] <-  names(fit.all.sites[[isite]])[
  #   grepl("weibull_k",names(fit.all.sites[[isite]]))]

  best.model[[isite.group]] <- fit.all.site.groups[[isite.group]][[best.model.names[isite.group]]]
  pp.check.best[[isite.group]] <- pp_check(best.model[[isite.group]], ndraws = 500)

}

null.models <- lapply(fit.all.sites, function(x){
  names.x = names(x)
  return(x[[which(grepl("weibull_none",names.x))]])})

DBH2test <- 50

temp3 <- bind_rows((lapply(1:length(site.groups),
                           function(isite.group){

                             print(isite.group)
                             cdf <- all.df %>%
                               filter(site.group == site.groups[isite.group])


                             levels <- as.character(unique(cdf$liana.cat))

                             newdata <- bind_rows(list(data.frame(
                               dbh = rep(DBH2test,length(levels)),
                               liana.cat = levels)))

                             newdata <- newdata %>%
                               mutate(id = 1:length(dbh),
                                      site.group = site.groups[isite.group])

                             cmodel <- best.model[[isite.group]]
                             null.model <- null.models[[isite.group]]

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
                                      mutate(site.group = site.groups[isite.group]))
                           })))


Afritron.sites <- readRDS("./data/Afritron/Afritron.metadata.RDS") %>%
  pull(site)

temp3.title <- temp3 %>%
  left_join(all.df.title %>% dplyr::select(site.group,
                                           N.low,N.high,N.tot) %>% distinct(),
            by = "site.group") %>%
  mutate(site.group.tot = paste0(site.group," (N = ",N.tot,")")) %>%
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
  mutate(site.group = factor(site.group,
                             levels = c("Panama","Amazon","Africa","Australasia"))) %>%
  group_by(site,liana.cat) %>%
  mutate(signif_rel = case_when(quantile(diff_h/no*100,probs = 0.975,na.rm = TRUE) < 0 ~ 0.6,
                                quantile(diff_h/no*100,probs = 0.025,na.rm = TRUE) > 0 ~ 0.6,
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
         filter(site.group %in% site.groups),
       aes(x = diff_h/no*100,
           y = site.group.tot,
           color = liana.cat,
           fill = liana.cat,
           alpha = signif_rel)) +
  geom_text(data = temp3.title %>%
              group_by(site.group.tot,liana.cat) %>%
              summarise(N.cat = unique(N.cat),
                        signif_rel = 1,
                        .groups = "keep"),
            aes(x = 25, label = paste("N = ",as.character(N.cat))),
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
  scale_x_continuous(limits = c(-45,30)) +
  labs(y = "", color = "", fill = "") +
  theme_bw() +
  guides(alpha = "none")
#
#
# ggplot(data = temp3.title %>%
#          filter(site %in% sites),
#        aes(x = diff_h/no*100,
#            y = site.tot,
#            color = liana.cat,
#            fill = liana.cat,
#            alpha = signif_rel)) +
#   # geom_text(data = temp3.title %>%
#   #             group_by(site.tot,liana.cat) %>%
#   #             summarise(N.cat = unique(N.cat),
#   #                       signif_rel = 1,
#   #                       .groups = "keep"),
#   #           aes(x = 25, label = paste("N = ",as.character(N.cat))),
#   #           color = "black", fill = NA,hjust = 0) +
#   geom_vline(xintercept = 0,linetype = 1) +
#   # geom_vline(data = mean.cat %>%
#   #              mutate(signif_rel = 1),
#   #            aes(xintercept = diff_h_m_rel,
#   #                color = liana.cat),
#   #            linetype = 2) +
#   # stat_interval(.width = c(.5, .8, .95)) +
#   stat_halfeye(color = NA) +
#
#   stat_pointinterval(aes(alpha = signif_rel2),
#                      .width = c(.66, .95),
#                      position = position_dodge(width = 0)) +
#   facet_wrap(~ liana.cat) +
#   scale_x_continuous(limits = c(-45,30)) +
#   labs(y = "", color = "", fill = "") +
#   theme_bw() +
#   facet_wrap(~ site.group, scales = "free_y") +
#   guides(alpha = "none")
#
# ggplot(data = temp3.title %>%
#          filter(site %in% sites),
#        aes(x = diff_h,
#            y = site,
#            color = liana.cat,
#            fill = liana.cat)) +
#   geom_vline(xintercept = 0,linetype = 1) +
#   geom_vline(data = mean.cat,
#              aes(xintercept = diff_h_m,
#                  color = liana.cat),
#              linetype = 2) +
#   # stat_interval(.width = c(.5, .8, .95)) +
#   stat_halfeye(alpha = 0.2, color = NA) +
#
#   stat_pointinterval( .width = c(.66, .95),
#                       position = position_dodge(width = 0.1)) +
#   facet_wrap(~ liana.cat) +
#   scale_x_continuous(limits = c(-20,10)) +
#   theme_bw()

