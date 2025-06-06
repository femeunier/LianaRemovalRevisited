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

all.df <- readRDS("./outputs/All.COI.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10)

site.groups <- readRDS("./outputs/site.loc.RDS")

all.df <- all.df %>%
  left_join(site.groups %>%
              rename(site = site.common) %>%
              dplyr::select(site,site.group),
            by = "site") %>%
  group_by(site.group,sp) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  # mutate(sp = case_when(N <= 100 | sp == "" | tolower(sp) == "other" ~ "OTHER",
  #                       TRUE ~ sp)) %>%
  dplyr::select(-N)

all.df %>%
  group_by(site.group) %>%
  summarise(Ndata = n(),
            Nspecies = length(unique(sp))) %>%
  arrange(Ndata)

site.groups <- all.df %>%
  group_by(site.group) %>%
  summarise(Ndata = n(),
            Nspecies = length(unique(sp))) %>%
  arrange(Ndata) %>%
  pull(site.group)

Names <- c("gmm","weibull","power")

Nchains <- 6
Niter <- 5000
control.list <- list(adapt_delta = 0.8,
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

for (isite.group in seq(1,length(site.groups))){

  csite.group <- site.groups[isite.group]
  csite.group.corrected <- gsub(" ", "",csite.group, fixed = TRUE)
  cdir <- file.path(dir.name,csite.group.corrected)

  cdf <- all.df %>%
    filter(site.group == csite.group)

  # Create data file
  dir.create(cdir,
             showWarnings = FALSE)
  saveRDS(cdf,
          file.path(cdir,
                    paste0("data_",csite.group.corrected,".RDS")))

  # Create script file
  Rscript.name <- file.path(cdir,script.name <- "Rscript.R")
  write.script(file.name = script.name,
               dir.name = cdir,
               site.name = csite.group.corrected,
               settings.location = settings.location,
               site.re = TRUE,
               threads = TRUE)

  # Create job file
  ED2scenarios::write_jobR(file = file.path(cdir,jobname),
                           nodes = 1,ppn = 24,mem = 25,walltime = 72,
                           prerun = "ml purge ; ml R-bundle-Bioconductor/3.20-foss-2024a-R-4.4.2",
                           CD = "/data/gent/vo/000/gvo00074/felicien/R/",
                           Rscript = Rscript.name)
  list_dir[[csite.group]] = cdir


}

dumb <- write_bash_submission(file = file.path(getwd(),"all_jobs_group_Bayesian.sh"),
                              list_files = list_dir,
                              job_name = jobname)

# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/submit.site.groups.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/outputs/site.loc.RDS hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/
