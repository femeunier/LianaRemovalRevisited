rm(list = ls())

library(brms)
library(gridExtra)
library(abind)
library(reshape2)
library(ggridges)
library(stringr)
library(scales)
library(tidyr)
library(ggforce)
library(lubridate)
library(dplyr)
library(LianaRemovalRevisited)
library(cowplot)
library(ggdist)
library(ggplot2)
library(ED2scenarios)
library(purrr)

source("check.site.groups.R")

dir.name <- "/data/gent/vo/000/gvo00074/felicien/R/data"

site.groups <- readRDS("./outputs/site.loc.RDS")
# Load the data
all.df <- bind_rows(readRDS("./outputs/All.COI.data.RDS") %>%
                      mutate(sp = str_squish(sp)) %>%
                      filter(dbh >= 10)) %>%
  left_join(site.groups %>%
              rename(site = site.common) %>%
              dplyr::select(site,site.group),
            by = "site")

all.df.title <- all.df %>%
  group_by(site) %>%
  mutate(site.N = paste0(site,", N = ", length(site)," (",length(site[which(liana.cat == "no")]), "-",
                         length(site[which(liana.cat == "low")]), "-",
                         length(site[which(liana.cat == "high")]), ")"),
         N.low = length(site[which(liana.cat == "low")]),
         N.high = length(site[which(liana.cat == "high")]),
         N.tot = length(site))

site.groups <- unique(all.df.title$site.group)

Check.Bayesian.sites <- readRDS("/data/gent/vo/000/gvo00074/felicien/R/Check.Bayesian.site.groups.RDS") %>%
  mutate(time = case_when(is.na(time) ~ as.Date('01/01/2000'),
                          TRUE ~ time))

site.groups <- all.df %>% group_by(site.group) %>%
  summarise(Ndata = n(),
            Nspecies = length(unique(sp))) %>%
  arrange(Ndata) %>% pull(site.group)

Names <- c("gmm","power","weibull")

fixed.effect.2.test <- list(power = list("a","none","b","all"),
                            weibull = list("none","a","all","b","k",
                                           c("a","b"),c("b","k"),c("a","k")),
                            gmm = list("none",
                                       "a","b","k",
                                       c("a","b"),c("a","k"),c("b","k"),
                                       "all"))


Nchains <- 4
Niter <- 15000
control.list <- list(adapt_delta = 0.9,
                     max_treedepth = 10)

overwrite <- FALSE
re <- "all"

list_dir <- list() ; job.names <- c()

for (isite in seq(1,length(site.groups))){

  csite.group <- site.groups[isite]
  csite.group.corrected <- gsub(" ", "",csite.group, fixed = TRUE)
  cdir <- file.path(dir.name,csite.group.corrected)

  dir.create(cdir,
             showWarnings = FALSE)

  cdf <- all.df %>%
    filter(site.group == csite.group)

  saveRDS(cdf,
          file.path(cdir,
                    paste0("data_",csite.group.corrected,".RDS")))

  for (iform in seq(1,length(Names))){

    cform <- Names[iform]
    cfixed.effect.2.test <- fixed.effect.2.test[[cform]]

    for (imodel in seq(1,length(cfixed.effect.2.test))){

      ceffect <- cfixed.effect.2.test[[imodel]]
      op.check <- Check.Bayesian.sites %>%
        filter(site.group == csite.group,
               model.form == cform,
               fe == paste0(ceffect,
                            collapse = ""))

      if (nrow(op.check) > 0){
        if ((op.check$time) > as.Date("15/02/25")){
          next()
        }
      }

      clist <- list(list(ceffect))
      names(clist) <- cform

      csettings <- list(Names = cform,
                        fixed.effect.2.test = clist,
                        overwrite = overwrite,
                        re = re,
                        Nchains = Nchains,
                        Niter = Niter,
                        control.list = control.list)

      settings.location <- file.path(cdir,
                                     paste0("current.settings.",
                                            cform,".",paste0(ceffect,collapse = ""),
                                            ".RDS"))
      saveRDS(csettings,settings.location)

      # Create script file
      Rscript.name <- file.path(cdir,
                                script.name <- paste0("Rscript.",
                                                      cform,".",paste0(ceffect,collapse = ""),
                                                      ".R")
      )

      write.script(file.name = script.name,
                   dir.name = cdir,
                   site.name = csite.group.corrected,
                   settings.location = settings.location,
                   threads = TRUE)


      # Create job file

      cjobname <- paste0("job.",
                         cform,".",paste0(ceffect,collapse = ""),
                         ".sh")
      job.names <- c(job.names,
                     cjobname)

      ED2scenarios::write_jobR(file = file.path(cdir,cjobname),
                               nodes = 1,ppn = 24,mem = 25,walltime = 72,
                               prerun = "ml purge ; ml R-bundle-Bioconductor/3.15-foss-2021b-R-4.2.0",
                               CD = "/data/gent/vo/000/gvo00074/felicien/R/",
                               Rscript = Rscript.name)

      list_dir[[paste0(csite.group,"_",cform,"_",paste0(ceffect,collapse = ""))]] = cdir



    }
  }
}

dumb <- write_bash_submission(file = file.path(getwd(),"unfinished_jobs.sh"),
                              list_files = list_dir,
                              job_name = job.names)

# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/resubmit.unfinished.jobs.site.groups.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
