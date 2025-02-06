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
all.df <- bind_rows(readRDS("./outputs/BCI.COI.data.RDS") %>%
                      mutate(sp = str_squish(sp)) %>%
                      filter(dbh >= 10)) %>%
  filter(year == 2019)

all.df.title <- all.df %>%
  group_by(year) %>%
  mutate(year.N = paste0(year,", N = ", length(year)," (",length(year[which(liana.cat == "no")]), "-",
                         length(year[which(liana.cat == "low")]), "-",
                         length(year[which(liana.cat == "high")]), ")"),
         N.low = length(year[which(liana.cat == "low")]),
         N.high = length(year[which(liana.cat == "high")]),
         N.tot = length(year))


years <- unique(all.df.title$year)

# Compile the outputs
fit.all.years <- list()

print("Reading")

years2keep <- best.model.names <- c()
for (iyear in seq(1,length(years))){

  cyear <- years[iyear]
  cyear.corrected <- gsub(" ", "",cyear, fixed = TRUE)

  print(paste("-",cyear))
  fit.all.years[[iyear]] <- list()

  dir.create(file.path("./outputs/",paste0("BCI.",cyear.corrected)),
             showWarnings = FALSE)

  # Check which files
  transfer.files("Diagnostics.RDS",
                 base = "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/",
                 source = file.path("data",paste0("BCI.",cyear.corrected)),
                 destination = file.path("./outputs/",paste0("BCI.",cyear.corrected)),
                 show.progress = FALSE)


  diagnosis.file <- file.path("./outputs/",paste0("BCI.",cyear.corrected),"Diagnostics.RDS")
  if (!file.exists(diagnosis.file)) next()

  Diagnstocis.Bayesian.year <- readRDS(diagnosis.file) %>%
    arrange((waic)) %>%
    filter(rhat.max < 1.05)

  Diagnstocis.Bayesian.year.best <-
    bind_rows(
      Diagnstocis.Bayesian.year %>%
        group_by(year) %>%
        filter(fe == "none") %>%
        filter(waic == min(waic)),
      Diagnstocis.Bayesian.year %>%
        group_by(year) %>%
        filter(waic == min(waic))) %>%
    distinct()

  all.possible.files <- Diagnstocis.Bayesian.year.best %>%
    mutate(file = paste0("Fit.",cyear.corrected,".",model.name,".RDS")) %>%
    pull(file)

  # Transfer files
  dir.create(file.path("./outputs/",cyear.corrected),showWarnings = FALSE)
  transfer.files(all.possible.files,
                 base = "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/",
                 source = file.path("data",paste0("BCI.",cyear.corrected)),
                 destination = file.path("./outputs/",paste0("BCI.",cyear.corrected)),
                 show.progress = FALSE)


  cfiles <- file.path("./outputs/",paste0("BCI.",cyear.corrected),all.possible.files)

  tokeep <- basename(cfiles) %in% all.possible.files

  cfiles.filtered <- cfiles[tokeep]
  cnames.filtered <- tools::file_path_sans_ext(cfiles.filtered)

  if (length(cfiles.filtered) == 0) next()

  for (ifile in seq(1,length(cfiles.filtered))){

    print(paste("--",ifile/length(cfiles.filtered)))
    fit.all.years[[iyear]][[cnames.filtered[ifile]]] <-  readRDS(paste0(cfiles.filtered[ifile]))
  }

  best.model.names[iyear] <- file.path("./outputs/",paste0("BCI.",cyear.corrected),paste0("Fit.",cyear.corrected,".",Diagnstocis.Bayesian.year %>%
                                                                                            group_by(year) %>%
                                                                                            filter(waic == min(waic)) %>% pull(model.name)))
}

# Select best model

actual.df <- data.frame()

print("Processing")
best.model <- list()
for (iyear in seq(1,length(years))){

  cyear <- years[iyear]
  print(paste(cyear, "-",iyear/length(years)))

  actual.df <- bind_rows(list(actual.df,
                              all.df %>% filter(year == cyear)))

  # if (length(fit.all.years[[iyear]]) > 1){
  #
  #   X <- lapply(fit.all.years[[iyear]],waic)
  #   comparison[[iyear]] <- brms::loo_compare(X)
  #   best.model.names[iyear] <- rownames(comparison[[iyear]])[1]
  #
  # } else {
  #   comparison[[iyear]] <- NULL
  #   best.model.names[iyear] <- names(fit.all.years[[iyear]])
  # }

  best.model[[iyear]] <- fit.all.years[[iyear]][[best.model.names[iyear]]]

}

null.models <- lapply(fit.all.years, function(x){
  names.x = names(x)
  return(x[[which(grepl("_none",names.x))]])})

alpha = 0.05

temp <- bind_rows((lapply(1:length(years),
                          function(iyear){

                            print(iyear)
                            cyear <- years[iyear]
                            cdf <- all.df %>%
                              filter(year == cyear)

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
                              geom_line(aes(x = dbh, y = h.pred.m, color = liana.cat)) +
                              geom_line(aes(x = dbh, y = h.null.pred.m), color = "black",linetype = 2) +
                              geom_point(data = cdf,
                                         aes(x = dbh, y = h, color = liana.cat)) +
                              # scale_x_log10() +
                              # scale_y_log10() +
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
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  geom_line(aes(x = dbh,y = h.pred.m, color = as.factor(liana.cat))) +geom_line(aes(x = dbh,y = h.null.pred.m), color = "black") +
  scale_x_continuous(limits = c(0,250)) +
  scale_y_continuous(limits = c(10,55)) +
  labs(x = "", y = '',color = "Liana infestation", fill = "Liana infestation") +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none")

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


alpha <- 0.05

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
  mutate(signif_rel = case_when(quantile(diff_h/no*100,probs = 1-alpha/2,na.rm = TRUE) < 0 ~ 0.3,
                                quantile(diff_h/no*100,probs = alpha/2,na.rm = TRUE) > 0 ~ 0.3,
                                TRUE ~ 0.2),
         signif_rel2 = case_when(quantile(diff_h/no*100,probs = 1-alpha/2,na.rm = TRUE) < 0 ~ 1,
                                 quantile(diff_h/no*100,probs = alpha/2,na.rm = TRUE) > 0 ~ 1,
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
  # geom_text(data = temp3.title %>%
  #              group_by(year.tot,liana.cat) %>%
  #              summarise(N.cat = unique(N.cat),
  #                        signif_rel = 1,
  #                        .groups = "keep"),
  #            aes(x = 25, label = paste("N = ",as.character(N.cat))),
  #            color = "black", fill = NA,hjust = 0) +
  geom_vline(xintercept = 0,linetype = 1) +
  # geom_vline(data = mean.cat %>%
  #              mutate(signif_rel = 1),
  #            aes(xintercept = diff_h_m_rel,
  #                color = liana.cat),
  #            linetype = 2) +
  # stat_interval(.width = c(.5, .8, .95)) +
  stat_halfeye(color = NA) +

  stat_pointinterval(aes(alpha = signif_rel2),
                     .width = c(.66, 1-alpha),
                     position = position_dodge(width = 0)) +
  # facet_wrap(~ liana.cat) +
  # scale_x_continuous(limits = c(-100,60)) +
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
                     .width = c(1-alpha),
                     position = position_dodge(width = 0)) +
  scale_x_continuous(limits = c(-15,2.5)) +
  labs(y = "", color = "", fill = "") +
  theme_minimal() +
  guides(alpha = "none") +
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  scale_fill_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  theme(legend.position = "none",
        text = element_text(size = 20)) +
  scale_y_discrete(labels = '') +
  labs(x = "", y = "")


temp3.title %>%
  filter(year %in% years) %>%
  dplyr::select(year,liana.cat,signif_rel2,diff_h,no) %>%
  group_by(liana.cat,year,signif_rel2) %>%
  summarise(Delta = mean(diff_h/no,na.rm = TRUE),
            Delta_low = quantile(diff_h/no,0.055,na.rm = TRUE),
            Delta_high = quantile(diff_h/no,1-0.055,na.rm = TRUE),
            Delta.h = mean(diff_h,na.rm = TRUE),
            Delta.h_low = quantile(diff_h,0.055,na.rm = TRUE),
            Delta.h_high = quantile(diff_h,1-0.055,na.rm = TRUE)) %>%
  filter(signif_rel2 == 1) %>%
  arrange((Delta.h))




df.residuals <- all.df %>%
  mutate(dbh = round(dbh)) %>%
  left_join(temp.title %>%
              dplyr::select(dbh,liana.cat,
                            h.pred.m,h.null.pred.m,year),
            by = c("dbh","liana.cat","year")) %>%
  mutate(res_null = (h.null.pred.m-h),
         res_best = (h.pred.m-h)) %>%
  mutate(delta.res = abs(res_best) - abs(res_null)) %>%
  # dplyr::select(liana.cat,res_null,res_best) %>%
  pivot_longer(cols = c(res_null,res_best)) %>%
  mutate(model = sub(".*\\_", "", name)) %>%
  mutate(liana.cat = case_when(model == "null" ~ "null",
                               TRUE ~ liana.cat))

df.residuals %>%
  group_by(year,name) %>%
  summarise(med = median(value),
            RSE = sqrt(1/(length(value - 3))*sum((value)**2)),
            RSE2 = sqrt(1/(length(value - 6))*sum((value)**2)),
            m =  mean(value),
            m.abs = mean(abs(value)))


ggplot(data = all.df %>%
         mutate(S = h/dbh),
       aes(x = liana.cat,
           y = S,
           fill = liana.cat)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred")) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = "none") +
  scale_x_discrete(labels = c("No",
                              "Moderate",
                              "Heavy")) +
  scale_y_continuous(limits = c(0.2,0.9)) +
  labs(x = "",y = "")



temp3 %>%
  mutate(diff_low = low - no,
         diff_high = high - no) %>%
  dplyr::select(-c(low,high)) %>%
  pivot_longer(cols = c(diff_low,diff_high),
               names_to = "liana.cat",
               values_to = "diff_h") %>%
  group_by(liana.cat) %>%
  summarise(m = 100*median(diff_h/no,na.rm = TRUE),
            m.low = 100*quantile(diff_h/no,alpha/2,na.rm = TRUE),
            m.high = 100*quantile(diff_h/no,1-alpha/2,na.rm = TRUE),

            m.abs = median(diff_h,na.rm = TRUE),
            m.abs.low = quantile(diff_h,alpha/2,na.rm = TRUE),
            m.abs.high = quantile(diff_h,1-alpha/2,na.rm = TRUE))

