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

all.df <- readRDS("./outputs/All.COI.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10) %>%
  left_join(readRDS("./outputs/BCI.COI.data.RDS"))

all.df <- all.df %>%
  group_by(site,sp) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  mutate(sp = case_when(N <= 5 | sp == "" | tolower(sp) == "other" ~ "OTHER",
                        TRUE ~ sp)) %>%
  dplyr::select(-N)

all.df %>% group_by(site) %>%
  summarise(Ndata = n(),
            Nspecies = length(unique(sp))) %>%
  arrange(Ndata)

sites <- all.df %>% group_by(site) %>%
  summarise(Ndata = n(),
            Nspecies = length(unique(sp))) %>%
  arrange(Ndata) %>% pull(site)

# sites <- c("Rio Grande")

Names <- c("weibull")

Nchains <- 4
Niter <- 5000
control.list <- list(adapt_delta = 0.99,
                     max_treedepth = 20)

overwrite <- TRUE

fixed.effect.2.test <- list(power = list("a","none","b",
                                         "all"),
                            weibull = list("a",
                                           "none",
                                           "all"),
                            gmm = list("none",
                                       "a","b","k",
                                       c("a","b"),c("a","k"),c("b","k"),
                                       "all"))
re <- "all"

for (isite in seq(1,length(sites))){

  print("======================================")

  csite <- sites[isite]

  cdf <- all.df %>%
    filter(site == csite)

  for (model in Names){

    cfixed.effect.2.test <- fixed.effect.2.test[[model]]

    for (model.form in seq(1,length(cfixed.effect.2.test))){

      print(paste(csite,"-",
                  paste0("Model (",model,"):"),paste0(which(Names == model),"/",length(Names)),"-",
                  paste0("Model Form (",paste(cfixed.effect.2.test[[model.form]],collapse = ""),"):"),paste0(model.form,"/",length(cfixed.effect.2.test))
      )
      )

      cname <- paste(model,paste(cfixed.effect.2.test[[model.form]],collapse = ""),sep = "_")
      op.file <- file.path(".","outputs",paste0("Fit.",csite,".",cname,".RDS"))

      if (!overwrite & file.exists(op.file)){
        next()
      }


      form.list <- default.forms(names = Names,
                                 fixed.effect = cfixed.effect.2.test[[model.form]],
                                 random.effect = re)

      priors.list <- default.priors(names = model,
                       fixed.effect = cfixed.effect.2.test[[model.form]],
                       random.effect = "none")

      # priors.list[[model]] <- rbind(priors.list[[model]],
      #                          c(set_prior("cauchy(0,2.5)", class = "sd",nlpar = "a", lb = 0),
      #                            set_prior("cauchy(0,2.5)",class = "sd", nlpar = "b", lb = 0),
      #                            set_prior("cauchy(0,2.5)",class = "sd", nlpar = "k", lb = 0)))


      existing.cat <- paste0("liana.cat",unique(cdf %>%
                                                  filter(liana.cat != "no") %>%
                                                  pull(liana.cat)))

      for (i in seq(1,length(priors.list))){
        priors.list[[i]] <- priors.list[[i]]  %>% filter(coef %in% c("","Intercept",existing.cat))
      }

      cfit <- brm(form.list[[model]],
                  data=cdf %>%
                    mutate(logh = log(h),
                           sp = as.factor(sp)),
                  cores = min(Nchains,
                              parallel::detectCores() - 1),
                  prior = priors.list[[model]],
                  control = control.list,
                  chains = Nchains,
                  iter = Niter,
                  silent = 2)


      saveRDS(cfit,op.file)

    }
  }
}
# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/test.site.effect.fulldataset.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
