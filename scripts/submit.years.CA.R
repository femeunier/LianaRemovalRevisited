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

all.df <- readRDS("./outputs/BCI.CA.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10) %>%
  mutate(area = CA)

# all.df <- all.df %>%
#   group_by(year,sp) %>%
#   mutate(N = n()) %>%
#   ungroup() %>%
#   mutate(sp = case_when(N <= 5 | sp == "" | tolower(sp) == "other" ~ "OTHER",
#                         TRUE ~ sp)) %>%
#   dplyr::select(-N)

all.df %>% group_by(year) %>%
  summarise(Ndata = n(),
            Nspecies = length(unique(sp))) %>%
  arrange(Ndata)

years <- all.df %>% group_by(year) %>%
  summarise(Ndata = n(),
            Nspecies = length(unique(sp))) %>%
  arrange(Ndata) %>% pull(year)


Names <- c("power")

Nchains <- 4
Niter <- 15000
control.list <- list(adapt_delta = 0.99,
                     max_treedepth = 10)

overwrite <- TRUE

fixed.effect.2.test <- list(power = list("a","none","all","b"),
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

settings.location <- file.path(dir.name,"current.settings.CA.RDS")
saveRDS(settings,settings.location)

list_dir <- list() ; jobname <- "jobCA.sh"

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
                    paste0("data_",cyear.corrected,"_CA.RDS")))

  # Create script file
  Rscript.name <- file.path(cdir,script.name <- "Rscript.CA.R")
  write.script.CA(file.name = script.name,
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

dumb <- write_bash_submission(file = file.path(getwd(),"BCI_jobs_Bayesian_CA.sh"),
                              list_files = list_dir,
                              job_name = jobname)

# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/submit.years.CA.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
