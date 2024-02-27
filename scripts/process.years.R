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

# Load the data
all.df <- bind_rows(readRDS("./outputs/BCI.COI.data.RDS") %>%
                      mutate(sp = str_squish(sp)) %>%
                      filter(dbh >= 10))

years <- unique(all.df$year)

models <- c("weibull","power","gmm")
model.forms <- c("all","none","a","b","ab","bk","ak","k")

# Compile the outputs
# fit.year <- list()

# df.all <- data.frame()
for (iyear in seq(1,length(years))){

  cyear <- years[iyear]
  cyear.corrected <- gsub(" ", "",cyear, fixed = TRUE)

  print(paste(cyear))

  df.year <- data.frame()

  all.possible.files <- crossing(years[iyear], models,model.forms) %>%
    mutate(n = paste0("Fit.",
                      as.character(cyear.corrected),
                      ".",
                      as.character(models),
                      "_",
                      as.character(model.forms),
                      ".RDS")) %>%
    pull(n)

  cfiles <- list.files(file.path("./data/",paste0("BCI.",cyear.corrected)),full.names = TRUE,pattern = "*.RDS")

  tokeep <- basename(cfiles) %in% all.possible.files

  cfiles.filtered <- cfiles[tokeep]
  cnames.filtered <- tools::file_path_sans_ext(cfiles.filtered)

  if (length(cfiles.filtered) == 0) next()

  for (ifile in seq(1,length(cfiles.filtered))){

    print("- Reading")
    print(paste("-",ifile/length(cfiles.filtered)))
    fit.year <-  readRDS(paste0(cfiles.filtered[ifile]))

    print("- Processing")

    cfile <- cnames.filtered[ifile]

    cmodel <- basename(cfile)
    cmodel.name <- strsplit(cmodel,"\\.")[[1]][3]
    cmodelform <- sub("\\_.*", "",cmodel.name)
    fixed.effects <- sub(".*\\_", "",cmodel.name)

    # cwaic <- loo(fit.year)
    cwaic <- waic(fit.year)
    all.rhat <- rhat(fit.year)

    # df.all <- bind_rows(list(df.all,
    #                          data.frame(year = cyear,
    #                                     time = file.info(paste0(cfile,".RDS"))[["mtime"]],
    #                                     model.name = cmodel.name,
    #                                     model.form = cmodelform,
    #                                     fe = fixed.effects,
    #                                     waic = cwaic$estimates[3,1],
    #                                     rhat.m = mean(all.rhat),
    #                                     rhat.max = max(all.rhat))))

    df.year <-  bind_rows(list(
      df.year,
      data.frame(year = cyear,
                 time = file.info(paste0(cfile,".RDS"))[["mtime"]],
                 model.name = cmodel.name,
                 model.form = cmodelform,
                 fe = fixed.effects,
                 waic = cwaic$estimates[3,1],
                 rhat.m = mean(all.rhat),
                 rhat.max = max(all.rhat))
    ))

  }
  saveRDS(df.year,
          file.path("./data/",paste0("BCI.",cyear.corrected),"Diagnostics.RDS"))
}

# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/process.years.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
