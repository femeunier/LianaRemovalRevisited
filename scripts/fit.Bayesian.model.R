rm(list = ls())

library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(brms)
library(dplyr)
library(ggplot2)
library(BayesianTools)
library(bayesplot)
library(bayestestR)
library(tidyr)
library(minpack.lm)
library(tidybayes)
library(rstan)

all.df <- readRDS("./outputs/All.COI.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10)

sites <- unique(all.df$site)

priors.list <- form.list <- list()

# Weibull
priors.list <- default.priors(names = c("Weibull"))
form.list <- default.forms(names = c("Weibull"))

fit.all.sites <- fit.sum.all.sites <-
  fit.waic.all.sites <- list()

for (csite in sites){

  cdf <- all.df %>%
    filter(site == csite)

  fit <- fit.sum <-
    fit.waic <- list()

  for (model in names(priors.list)){

    print("================")
    print(paste(model,"-",(length(fit)+1)/length(names(priors.list)),"-",csite))

    fit[[model]] <- brm(form.list[[model]],
                        data=cdf,
                        prior=priors.list[[model]],
                        chains = 2, iter = 5000)

    fit.sum[[model]] <- summary(fit[[model]],waic = TRUE)

    pp_check(fit[[model]])
    fit.waic[[model]] <- waic(fit[[model]])

  }

  fit.all.sites[[csite]] <- fit
  fit.sum.all.sites[[csite]] <-fit.sum
  fit.waic.all.sites[[csite]] <- fit.waic
}

saveRDS(fit.sum.all.sites,"./outputs/fit.sum.all.sites.RDS")
saveRDS(fit.all.sites,"./outputs/fit.all.sites.RDS")
saveRDS(fit.waic.all.sites,"./outputs/fit.waic.all.sites.RDS")




# system2("scp",paste("./outputs/All.COI.data.RDS",
#                     "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/"))
# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/fit.Bayesian.model.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
