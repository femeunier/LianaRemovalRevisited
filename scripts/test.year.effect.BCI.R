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

library(LianaRemovalRevisited)


# system2("scp",paste("./outputs/BCI.COI.data.RDS",
#                     "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/"))

all.df <- readRDS("./outputs/BCI.COI.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10)

all.df %>%
  group_by(site) %>%
  summarise(n())

years = unique(all.df$year)

Names <- c("power","weibull","gmm")
Nchains <- 2
Niter <- 1000


fixed.effect.2.test <- list(power = list("a","b","k",
                                         c("all")),
                            weibull = list("a","b","k",
                                         c("a","b"),c("a","k"),c("b","k"),
                                         c("all")),
                            gmm = list("a","b","k",
                                         c("a","b"),c("a","k"),c("b","k"),
                                         c("all")))

fixed.effect.2.test <- list("a")

fit.all.years <- fit.sum.all.years <-
  fit.waic.all.years <- list()

for (iyear in seq(1,length(years))){

  print("======================================")

  cyear <- years[iyear]

  cdf <- all.df %>%
    filter(year == cyear)

  fit <- fit.sum <-
    fit.waic <- list()

  for (model in Names){

    cfixed.effect.2.test <- fixed.effect.2.test[[model]]

    for (model.form in seq(1,length(fixed.effect.2.test))){

      print(paste("Model Form:",paste0(model.form,"/",length(cfixed.effect.2.test)),
                  paste0("- Model (",model,"):"),paste0((length(fit)+1),"/",length(Names)), "-", cyear))

      cname <- paste(model,paste(cfixed.effect.2.test[[model.form]],collapse = ""),sep = "_")

      form.list <- default.forms(names = Names,
                                 fixed.effect = cfixed.effect.2.test[[model.form]],
                                 random.effect = "all")
      priors.list <- default.priors(names = Names,
                                    params = cfixed.effect.2.test[[model.form]])

      fit[[cname]] <- brm(form.list[[model]],
                          data=cdf,
                          cores = min(4,getOption("mc.cores", Nchains)),
                          prior=priors.list[[model]],
                          chains = Nchains, iter = Niter,
                          silent = 2)

      fit.sum[[cname]] <- summary(fit[[model]],waic = TRUE)

      fit.waic[[cname]] <- waic(fit[[model]])


    }
  }

  fit.all.years[[iyear]] <- fit
  fit.sum.all.years[[iyear]] <- fit.sum
  fit.waic.all.years[[iyear]] <- fit.waic

}


saveRDS(fit.sum.all.years,"./outputs/fit.sum.all.years")
saveRDS(fit.all.years,"./outputs/fit.all.years.RDS")
saveRDS(fit.waic.all.years,"./outputs/fit.waic.all.years.RDS")

# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/test.year.effect.BCI.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
