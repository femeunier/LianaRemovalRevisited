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
library(ED2scenarios)

all.df <- readRDS("./outputs/BCI.COI.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10)

all.df <- all.df %>%
  group_by(year,sp) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  dplyr::select(-N)

all.df %>% group_by(year) %>%
  summarise(Ndata = n(),
            Nspecies = length(unique(sp))) %>%
  arrange(Ndata)

years <- all.df %>% group_by(year) %>%
  summarise(Ndata = n(),
            Nspecies = length(unique(sp))) %>%
  arrange(Ndata) %>% pull(year)

years <- 2015

Names <- c("weibull","gmm","power")

Nchains <- 4
Niter <- 15000
control.list <- list(adapt_delta = 0.99,
                     max_treedepth = 10)

overwrite <- TRUE

fixed.effect.2.test <- list(power = list("a","none","b",
                                         "all"),
                            weibull = list("all","none","a","b","k",
                                           c("a","b"),c("b","k"),c("a","k")),
                            gmm = list("none",
                                       "a","b","k",
                                       c("a","b"),c("a","k"),c("b","k"),
                                       "all"))
re <- "all"
dir.name <- "/data/gent/vo/000/gvo00074/felicien/R/data"

settings <- list(Names  = Names,
                 fixed.effect.2.test = fixed.effect.2.test,
                 overwrite = overwrite,
                 re = re,
                 Nchains = Nchains,
                 Niter = Niter,
                 control.list = control.list)
settings.location <- file.path(dir.name,"current.settings.RDS")
saveRDS(settings,settings.location)

list_dir <- list() ; jobname <- "job.sh"

for (iyear in seq(1,length(years))){

  cyear <- years[iyear]
  cyear.corrected <- gsub(" ", "",cyear, fixed = TRUE)
  cdir <- file.path(dir.name,paste0("BCI.",cyear.corrected))

  cdf <- all.df %>%
    filter(year == cyear)

  # Create data file
  dir.create(cdir,
             showWarnings = FALSE)
  saveRDS(cdf,
          file.path(cdir,
                    paste0("data_",cyear.corrected,".RDS")))

  # Create script file
  Rscript.name <- file.path(cdir,script.name <- "Rscript.R")
  write.script(file.name = script.name,
               dir.name = cdir,
               site.name = cyear.corrected,
               settings.location = settings.location)

  # Create job file
  ED2scenarios::write_jobR(file = file.path(cdir,jobname),
                           nodes = 1,ppn = 4,mem = 25,walltime = 24,
                           prerun = "ml purge ; ml R-bundle-Bioconductor/3.15-foss-2021b-R-4.2.0",
                           CD = "/data/gent/vo/000/gvo00074/felicien/R/",
                           Rscript = Rscript.name)
  list_dir[[as.character(cyear)]] = cdir


}

dumb <- write_bash_submission(file = file.path(getwd(),"BCI_jobs_Bayesian.sh"),
                              list_files = list_dir,
                              job_name = jobname)

# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/submit.years.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
