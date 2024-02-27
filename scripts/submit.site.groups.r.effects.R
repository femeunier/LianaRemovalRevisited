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

Afritron.sites <- readRDS("./data/Afritron.metadata.RDS") %>%
  pull(site)

all.df <- all.df %>%
  mutate(site.group = case_when(site %in% c("Pasoh","Danum Valley","Australia") ~ "Australasia",
                                site %in% c("Sand-F","Semi-F","Atla-F","Loundoungou",Afritron.sites,"OKU","Tanzania") ~ "Africa",
                                site %in% c("Gigante","BCI") ~ "Panama",
                                TRUE ~ "Amazon")) %>%
  group_by(site.group,sp) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  # mutate(sp = case_when(N <= 100 | sp == "" | tolower(sp) == "other" ~ "OTHER",
  #                       TRUE ~ sp)) %>%
  dplyr::select(-N)

dir.name <- "/data/gent/vo/000/gvo00074/felicien/R/data"
Names <- c("gmm","weibull","power")

Nchains <- 4
Niter <- 5000
control.list <- list(adapt_delta = 0.8,
                     max_treedepth = 10)

overwrite <- FALSE

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

site.groups <- all.df %>%
  group_by(site.group) %>%
  summarise(Ndata = n(),
            Nspecies = length(unique(sp))) %>%
  arrange(Ndata) %>%
  pull(site.group)

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
                   site.name = csite.group.corrected,
                   settings.location = settings.location,
                   site.re = TRUE)


      # Create job file

      cjobname <- paste0("job.",
                         cname,".",paste0(ceffect,collapse = ""),
                         ".sh")
      job.names <- c(job.names,
                     cjobname)

      ED2scenarios::write_jobR(file = file.path(cdir,cjobname),
                               nodes = 1,ppn = 4,mem = 25,walltime = 72,
                               prerun = "ml purge ; ml R-bundle-Bioconductor/3.15-foss-2021b-R-4.2.0",
                               CD = "/data/gent/vo/000/gvo00074/felicien/R/",
                               Rscript = Rscript.name)

      list_dir[[paste0(csite.group.corrected,"_",cname,"_",paste0(ceffect,collapse = ""))]] = cdir

    }
  }
}


dumb <- write_bash_submission(file = file.path(getwd(),"all_jobs_Bayesian.sh"),
                              list_files = list_dir,
                              job_name = job.names)

# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/submit.site.groups.r.effects.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
