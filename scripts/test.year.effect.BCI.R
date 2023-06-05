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
  filter(dbh >= 10) %>%
  group_by(year,sp) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  mutate(sp = case_when(N <= 5 | sp == "" | tolower(sp) == "other" ~ "OTHER",
                        TRUE ~ sp)) %>%
  dplyr::select(-N)

all.df %>%
  group_by(year) %>%
  summarise(N = n(),
            Nsp = length(unique(sp)))

years = unique(all.df$year)
years = c(2011,2015)

Names <- c("power","weibull","gmm")
Names <- c("weibull")

Nchains <- 4
Niter <- 5000
control.list <- list(adapt_delta = 0.8,
                     max_treedepth = 20)

overwrite <- TRUE

fixed.effect.2.test <- list(power = list("a","none","b","all"),
                            weibull = list("a","none","all"),
                            gmm = list("a","b","k",
                                       c("a","b"),c("a","k"),c("b","k"),
                                       "all","none"))

re <- "all"

# fit.all.years <- fit.sum.all.years <-
#   fit.waic.all.years <- list()

for (iyear in seq(1,length(years))){

  print("======================================")

  cyear <- years[iyear]

  cdf <- all.df %>%
    filter(year == cyear)

  # fit <- fit.sum <-
  #   fit.waic <- list()

  for (model in Names){

    cfixed.effect.2.test <- fixed.effect.2.test[[model]]

    for (model.form in seq(1,length(cfixed.effect.2.test))){

      print(paste(cyear,"-",
                  paste0("Model (",model,"):"),paste0(which(Names == model),"/",length(Names)),"-",
                  paste0("Model Form (",paste(cfixed.effect.2.test[[model.form]],collapse = ""),"):"),paste0(model.form,"/",length(cfixed.effect.2.test))
                  )
            )

      cname <- paste(model,paste(cfixed.effect.2.test[[model.form]],collapse = ""),sep = "_")
      op.file <- file.path(".","outputs",paste0("Fit.BCI.",cyear,".",cname,".RDS"))

      if (!overwrite & file.exists(op.file)){
        next()
      }

      form.list <- default.forms(names = Names,
                                 fixed.effect = cfixed.effect.2.test[[model.form]],
                                 random.effect = re)
      priors.list <- default.priors(names = model,
                                    fixed.effect = cfixed.effect.2.test[[model.form]],
                                    random.effect = "none")

      # stop()

      cfit <- brm(form.list[[model]],
                  data=cdf %>%

                    mutate(logh = log(h)),
                  cores = min(Nchains,
                              parallel::detectCores() - 1),
                  prior = priors.list[[model]],
                  control = c(control.list),
                  chains = Nchains,
                  iter = Niter,
                  warmup = floor(Niter*2/3),
                  silent = 2)


      # posteriors <- bind_rows(lapply(list(cfit),function(x){
      #   temp <- as.array(x)
      #   CN <- colnames(temp[,1,])
      #   return(
      #     melt(temp[,,grepl(paste(c("_Intercept","liana.catno","liana.catlow","liana.cathigh"),collapse="|"),
      #                       CN)]))
      # }),.id = "year") %>%
      #   mutate(year = as.factor(years[as.numeric(year)])) %>%
      #   rename(sample = iteration,
      #          parname = variable)
      #
      # cols = c("b_a_Intercept","b_le_liana.catno","b_le_liana.catlow","b_le_liana.cathigh")
      # A <- posteriors %>% filter(parname %in%
      #                         cols) %>%
      #   pivot_wider(names_from = parname,
      #               values_from = value) %>%
      #   mutate(no = b_a_Intercept + b_le_liana.catno,
      #          low = b_a_Intercept + b_le_liana.catlow,
      #          high = b_a_Intercept + b_le_liana.cathigh) %>%
      #   dplyr::select(- c(b_le_liana.catno,b_le_liana.catlow,b_le_liana.cathigh,b_a_Intercept)) %>%
      #   pivot_longer(cols = c(no,low,high))
#
#       ggplot(data = A) +
#         geom_density(aes(x = value, fill = name), alpha = 0.5) +
#         theme_bw()

      saveRDS(cfit,op.file)

    }
  }
}


# saveRDS(fit.sum.all.years,"./outputs/fit.sum.all.years")
# saveRDS(fit.all.years,"./outputs/fit.all.years.RDS")
# saveRDS(fit.waic.all.years,"./outputs/fit.waic.all.years.RDS")

# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/test.year.effect.BCI.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
