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


all.df <- readRDS("./outputs/All.COI.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10) %>%
  group_by(sp) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  # mutate(sp = case_when(N <= 100 | sp == "" | tolower(sp) == "other" ~ "OTHER",
  #                       TRUE ~ sp)) %>%
  dplyr::select(-N)

dir.name <- "/data/gent/vo/000/gvo00074/felicien/R/data/"
csite.corrected <- csite <- "Total"
cdir <- file.path(dir.name,
                  paste0(csite.corrected,".re"))

# Create data file
dir.create(cdir,
           showWarnings = FALSE)

saveRDS(all.df,
        file.path(cdir,
                  paste0("data_",csite.corrected,".RDS")))

Names <- c("gmm","weibull","power")

Nchains <- 5
Niter <- 5000
control.list <- list(adapt_delta = 0.95,
                     max_treedepth = 10)

overwrite <- TRUE

fixed.effect.2.test <- list(power = list("a","none","b",
                                         "all"),
                            weibull = list("none","a","all","b","k",
                                           c("a","b"),c("b","k"),c("a","k")),
                            gmm = list("none",
                                       "a","b","k",
                                       c("a","b"),c("a","k"),c("b","k"),
                                       "all"))
re <- "all"

list_dir <- list() ; job.names <- c()

for (iname in seq(1,length(Names))){
  cname <- Names[[iname]]
  for (i in seq(1,length(fixed.effect.2.test[[cname]]))){

    ceffect <- fixed.effect.2.test[[cname]][[i]]
    clist <- list(list(ceffect))
    names(clist) <- cname

    csettings <- list(Names = cname,
                     fixed.effect.2.test = clist,
                     overwrite = overwrite,
                     re = re,
                     Nchains = Nchains,
                     Niter = Niter,
                     backend = "rstan",           # rstan
                     control.list = control.list)

    settings.location <- file.path(dir.name,paste0("current.settings.",
                                                   cname,".",paste0(ceffect,collapse = ""),
                                                   ".RDS"))
    saveRDS(csettings,settings.location)

    # Create script file
    Rscript.name <- file.path(cdir,script.name <- paste0("Rscript.",
                                                         cname,".",paste0(ceffect,collapse = ""),
                                                         ".R"))

    write.script(file.name = script.name,
                 dir.name = cdir,
                 site.name = csite.corrected,
                 settings.location = settings.location,
                 site.re = TRUE,
                 threads = TRUE)


    # Create job file

    cjobname <- paste0("job.",
                       cname,".",paste0(ceffect,collapse = ""),
                       ".sh")
    job.names <- c(job.names,
                   cjobname)

    ED2scenarios::write_jobR(file = file.path(cdir,cjobname),
                             nodes = 1,ppn = 24,mem = 25,walltime = 72,
                             prerun = "ml purge ; ml R-bundle-Bioconductor/3.20-foss-2024a-R-4.4.2",
                             CD = "/data/gent/vo/000/gvo00074/felicien/R/",
                             Rscript = Rscript.name)

    list_dir[[paste0(cname,"_",paste0(ceffect,collapse = ""))]] = cdir

  }
}

dumb <- write_bash_submission(file = file.path(getwd(),
                                               "all_jobs_Bayesian.sh"),
                              list_files = list_dir,
                              job_name = job.names)

# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/submit.total.effects.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
