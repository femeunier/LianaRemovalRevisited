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
library(dplyr)
library(LianaRemovalRevisited)
library(cowplot)
library(ggdist)
library(ggplot2)
library(ED2scenarios)
library(purrr)

dir.name <- "/data/gent/vo/000/gvo00074/felicien/R/data"

# Load the data
# all.df <- bind_rows(readRDS("./outputs/All.COI.data.RDS") %>%
#                       mutate(sp = str_squish(sp)) %>%
#                       filter(dbh >= 10),
#                     readRDS("./outputs/All.COI.data.RDS") %>%
#                       mutate(sp = str_squish(sp)) %>%
#                       filter(dbh >= 10) %>%
#                       mutate(site = "Total.re"))

all.df <-  readRDS("./outputs/All.COI.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10) %>%
  mutate(site = "Total.re")

# all.df <- bind_rows(readRDS("./outputs/All.COI.data.RDS") %>%
#                       mutate(sp = str_squish(sp)) %>%
#                       filter(dbh >= 10) %>%
#                       mutate(site = "Total.re"))

all.df.title <- all.df %>%
  group_by(site) %>%
  mutate(site.N = paste0(site,", N = ", length(site)," (",length(site[which(liana.cat == "no")]), "-",
                         length(site[which(liana.cat == "low")]), "-",
                         length(site[which(liana.cat == "high")]), ")"),
         N.low = length(site[which(liana.cat == "low")]),
         N.high = length(site[which(liana.cat == "high")]),
         N.tot = length(site))

# transfer.files("Check.Bayesian.sites.RDS",
#                base = "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/",
#                source = "",
#                destination = file.path("./outputs/"),
#                show.progress = TRUE)

sites <- all.df %>% group_by(site) %>%
  summarise(Ndata = n(),
            Nspecies = length(unique(sp))) %>%
  arrange(Ndata) %>% pull(site)
# sites <- "Pasoh"

Names <- c("gmm","power","weibull")

fixed.effect.2.test <- list(power = list("a","none","b","all"),
                            weibull = list("none","a","all","b","k",
                                           c("a","b"),c("b","k"),c("a","k")),
                            gmm = list("none",
                                       "a","b","k",
                                       c("a","b"),c("a","k"),c("b","k"),
                                       "all"))

Strong <- TRUE
Nchains <- 4
Niter <- 15000
control.list <- list(adapt_delta = 0.95,
                     max_treedepth = 10)

overwrite <- TRUE
re <- "all"

list_dir <- list() ; job.names <- c()
all.Bayesian.sites <- data.frame()

for (isite in seq(1,length(sites))){

  csite <- sites[isite]
  csite.corrected <- gsub(" ", "",csite, fixed = TRUE)
  cdir <- file.path(dir.name,csite.corrected)

  diagn.file <- file.path("/data/gent/vo/000/gvo00074/felicien/R/data",
                          csite.corrected,
                          "Diagnostics.RDS")

  if (!file.exists(diagn.file)) next()

  Check.Bayesian.site <- readRDS(diagn.file)

  all.Bayesian.sites <- bind_rows(all.Bayesian.sites,
                                  Check.Bayesian.site)

  cdf <- all.df %>%
    filter(site == csite)

  saveRDS(cdf,
          file.path(cdir,
                    paste0("data_",csite.corrected,".RDS")))

  for (iform in seq(1,length(Names))){

    cform <- Names[iform]
    cfixed.effect.2.test <- fixed.effect.2.test[[cform]]

    for (imodel in seq(1,length(cfixed.effect.2.test))){

      ceffect <- cfixed.effect.2.test[[imodel]]
      op.check <- Check.Bayesian.site %>%
        filter(site == csite,
               model.form == cform,
               fe == paste0(ceffect,collapse = ""))

      if (nrow(op.check) == 0){

      } else if (op.check[["rhat.max"]] < 1.05) {
        next()
      }

      print(paste(csite,'-',cform,"-", paste0(ceffect,collapse = ""),"-",op.check[["rhat.max"]]))

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
                   site.name = csite.corrected,
                   settings.location = settings.location,
                   strong = Strong,
                   site.re = TRUE,
                   threads = TRUE)

      # Create job file

      cjobname <- paste0("job.",
                         cform,".",paste0(ceffect,collapse = ""),
                         ".sh")
      job.names <- c(job.names,
                     cjobname)

      ED2scenarios::write_jobR(file = file.path(cdir,cjobname),
                               nodes = 1,ppn = 24,mem = 25,walltime = 72,
                               prerun = "ml purge ; ml R-bundle-Bioconductor/3.20-foss-2024a-R-4.4.2",
                               CD = "/data/gent/vo/000/gvo00074/felicien/R/",
                               Rscript = Rscript.name)

      list_dir[[paste0(csite,"_",cform,"_",paste0(ceffect,collapse = ""))]] = cdir

    }
  }
}

dumb <- write_bash_submission(file = file.path(getwd(),"largeRhat_jobs_total.sh"),
                              list_files = list_dir,
                              job_name = job.names)

# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/resubmit.largeRHat.jobs_total.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
