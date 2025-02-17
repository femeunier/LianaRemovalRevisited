rm(list = ls())

library(brms)
library(gridExtra)
library(abind)
library(reshape2)
library(ggridges)
library(stringr)
library(lubridate)
library(scales)
library(tidyr)
library(ggforce)
library(dplyr)
library(LianaRemovalRevisited)
library(cowplot)
library(ggdist)
library(ggplot2)

# Load the data
all.df <- bind_rows(readRDS("./outputs/All.COI.data.RDS") %>%
                      mutate(sp = str_squish(sp)) %>%
                      filter(dbh >= 10))


site.groups <- readRDS("./outputs/site.loc.RDS")

all.df.title <- all.df %>%
  left_join(site.groups %>%
              rename(site = site.common) %>%
              dplyr::select(site,site.group),
            by = "site") %>%
  mutate(site.N = paste0(site,", N = ", length(site)," (",length(site[which(liana.cat == "no")]), "-",
                         length(site[which(liana.cat == "low")]), "-",
                         length(site[which(liana.cat == "high")]), ")"),
         N.low = length(site[which(liana.cat == "low")]),
         N.high = length(site[which(liana.cat == "high")]),
         N.tot = length(site))

site.groups <- unique(all.df.title$site.group)

models <- c("weibull","power","gmm")
model.forms <- c("all","none","a","b","ab","bk","ak","k")

# Compile the outputs
# fit.site <- list()

# df.all <- data.frame()
for (isite in seq(1,length(site.groups))){

  csite.group <- site.groups[isite]
  csite.group.corrected <- gsub(" ", "",csite.group, fixed = TRUE)

  print(paste(csite.group))

  df.site <- data.frame()

  all.possible.files <- crossing(site.groups[isite], models,model.forms) %>%
    mutate(n = paste0("Fit.",
                      ifelse(csite.group == "Total.re",
                             "Total",
                             as.character(csite.group.corrected)),
                      ".",
                      as.character(models),
                      "_",
                      as.character(model.forms),
                      ".RDS")) %>%
    pull(n)

  cfiles <- list.files(file.path("./data/",csite.group.corrected),
                       full.names = TRUE,
                       pattern = "*.RDS")

  tokeep <- (basename(cfiles) %in% all.possible.files) &
    (year(file.info(cfiles)$ctime) >= 2025)

  cfiles.filtered <- cfiles[tokeep]
  cnames.filtered <- tools::file_path_sans_ext(cfiles.filtered)

  if (length(cfiles.filtered) == 0) next()

  for (ifile in seq(1,length(cfiles.filtered))){

    print("- Reading")
    print(paste("-",ifile/length(cfiles.filtered)))
    fit.site <-  readRDS(paste0(cfiles.filtered[ifile]))

    print("- Processing")

    # Check estimate errors

    cfile <- cnames.filtered[ifile]

    cmodel <- basename(cfile)
    cmodel.name <- strsplit(cmodel,"\\.")[[1]][3]
    cmodelform <- sub("\\_.*", "",cmodel.name)
    fixed.effects <- sub(".*\\_", "",cmodel.name)

    # cwaic <- loo(fit.site)
    cwaic <- tryCatch(waic(fit.site),
                      error = function(e) NULL)
    if (is.null(cwaic)){
      next()
    }

    all.rhat <- rhat(fit.site)
    max.error <- max(predict(fit.site)[,2])

    # df.all <- bind_rows(list(df.all,
    #                          data.frame(site = csite,
    #                                     time = file.info(paste0(cfile,".RDS"))[["mtime"]],
    #                                     model.name = cmodel.name,
    #                                     model.form = cmodelform,
    #                                     fe = fixed.effects,
    #                                     waic = cwaic$estimates[3,1],
    #                                     rhat.m = mean(all.rhat),
    #                                     rhat.max = max(all.rhat))))

    df.site <-  bind_rows(list(
      df.site,
      data.frame(site.group = csite.group,
                 time = file.info(paste0(cfile,".RDS"))[["mtime"]],
                 model.name = cmodel.name,
                 model.form = cmodelform,
                 fe = fixed.effects,
                 waic = cwaic$estimates[3,1],
                 error.max = max.error,
                 rhat.m = mean(all.rhat),
                 rhat.max = max(all.rhat))
    ))

  }
  saveRDS(df.site,
          file.path("./data/",csite.group.corrected,"Diagnostics.RDS"))
}

# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/process.site.groups.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
