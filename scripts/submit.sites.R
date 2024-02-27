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

# system2("scp",paste("./outputs/BCI.COI.data.RDS",
#                     "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/"))

all.df <- readRDS("./outputs/All.COI.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10)

all.df <- all.df %>%
  group_by(site,sp) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  # mutate(sp = case_when(N <= 5 | sp == "" | tolower(sp) == "other" ~ "OTHER",
  #                       TRUE ~ sp)) %>%
  dplyr::select(-N)

all.df %>% group_by(site) %>%
  summarise(Ndata = n(),
            Nspecies = length(unique(sp))) %>%
  arrange(Ndata)

sites <- all.df %>% group_by(site) %>%
  summarise(Ndata = n(),
            Nspecies = length(unique(sp))) %>%
  arrange(Ndata) %>% pull(site)

# sites <- c("Pasoh","Asenayo","Rio Grande")
sites <- c("Tapajos")

Names <- c("weibull","power","gmm")

Nchains <- 4
Niter <- 15000
control.list <- list(adapt_delta = 0.8,
                     max_treedepth = 10)

overwrite <- TRUE

fixed.effect.2.test <- list(power = list("a","none","b","all"),
                            weibull = list("none",
                                           "a","b","k",
                                           c("a","b"),c("a","k"),c("b","k"),
                                           "all"),
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

for (isite in seq(1,length(sites))){

  csite <- sites[isite]
  csite.corrected <- gsub(" ", "",csite, fixed = TRUE)
  cdir <- file.path(dir.name,csite.corrected)

  cdf <- all.df %>%
    filter(site == csite)

  # Create data file
  dir.create(cdir,
             showWarnings = FALSE)
  saveRDS(cdf,
          file.path(cdir,
                    paste0("data_",csite.corrected,".RDS")))

  # Create script file
  Rscript.name <- file.path(cdir,script.name <- "Rscript.R")
  write.script(file.name = script.name,
               dir.name = cdir,
               site.name = csite.corrected,
               settings.location = settings.location)

  # Create job file
  ED2scenarios::write_jobR(file = file.path(cdir,jobname),
                           nodes = 1,ppn = 4,mem = 25,walltime = 72,
                           prerun = "ml purge ; ml R-bundle-Bioconductor/3.15-foss-2021b-R-4.2.0",
                           CD = "/data/gent/vo/000/gvo00074/felicien/R/",
                           Rscript = Rscript.name)
  list_dir[[csite]] = cdir


}

# # Last run with all data together
# csite.corrected <- csite <- "Total"
# cdir <- file.path(dir.name,csite.corrected)
# cdf <- all.df
#
# # Create data file
# dir.create(cdir,
#            showWarnings = FALSE)
# saveRDS(cdf,
#         file.path(cdir,
#                   paste0("data_",csite.corrected,".RDS")))
#
#
# # Create script file
# Rscript.name <- file.path(cdir,script.name <- "Rscript.R")
# write.script(file.name = script.name,
#              dir.name = cdir,
#              site.name = csite.corrected,
#              settings.location = settings.location)
#
# # Create job file
# ED2scenarios::write_jobR(file = file.path(cdir,jobname),
#                          nodes = 1,ppn = 4,mem = 25,walltime = 24,
#                          prerun = "ml purge ; ml R-bundle-Bioconductor/3.15-foss-2021b-R-4.2.0",
#                          CD = "/data/gent/vo/000/gvo00074/felicien/R/",
#                          Rscript = Rscript.name)
# list_dir[[csite]] = cdir

dumb <- write_bash_submission(file = file.path(getwd(),"sites_jobs_Bayesian.sh"),
                              list_files = list_dir,
                              job_name = jobname)

# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/submit.sites.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
