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

all.df.title <- all.df %>%
  group_by(year) %>%
  mutate(year.N = paste0(year,", N = ", length(year)," (",length(year[which(liana.cat == "no")]), "-",
                         length(year[which(liana.cat == "low")]), "-",
                         length(year[which(liana.cat == "high")]), ")"),
         N.low = length(year[which(liana.cat == "low")]),
         N.high = length(year[which(liana.cat == "high")]),
         N.tot = length(year))

years <- unique(all.df.title$year)
# years <- c("BCI")

models <- c("weibull","power","gmm")
model.forms <- c("all","none","a","b","ab","bk","ak","k")

# Compile the outputs
fit.all.years <- list()

df.all <- data.frame()
for (iyear in seq(1,length(years))){

  cyear <- years[iyear]
  cyear.corrected <- gsub(" ", "",cyear, fixed = TRUE)

  print(paste(cyear))

  print("- Reading")
  fit.all.years[[iyear]] <- list()

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

    print(paste("--",ifile/length(cfiles.filtered)))
    fit.all.years[[iyear]][[cnames.filtered[ifile]]] <-  file.exists((paste0(cfiles.filtered[ifile])))
  }

  print("- Processing")

  for (imodel in seq(1,length(fit.all.years[[iyear]]))){

    cfile <- names(fit.all.years[[iyear]])[imodel]

    print(paste("--",imodel/length(fit.all.years[[iyear]])))

    cmodel <- basename(cfile)
    cmodel.name <- strsplit(cmodel,"\\.")[[1]][3]
    cmodelform <- sub("\\_.*", "",cmodel.name)
    fixed.effects <- sub(".*\\_", "",cmodel.name)

    df.all <- bind_rows(list(df.all,
                             data.frame(year = cyear,
                                        time = file.info(paste0(cfile,".RDS"))[["mtime"]],
                                        model.name = cmodel.name,
                                        model.form = cmodelform,
                                        fe = fixed.effects)))
  }
}

saveRDS(df.all,
        "Check.Bayesian.years.RDS")

# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/check.years.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
