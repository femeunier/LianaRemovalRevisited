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
all.df <- bind_rows(readRDS("./outputs/All.COI.data.RDS") %>%
                      mutate(sp = str_squish(sp)) %>%
                      filter(dbh >= 10),
                    readRDS("./outputs/All.COI.data.RDS") %>%
                      mutate(sp = str_squish(sp)) %>%
                      filter(dbh >= 10) %>% mutate(site = "Total"))

all.df.title <- all.df %>%
  group_by(site) %>%
  mutate(site.N = paste0(site,", N = ", length(site)," (",length(site[which(liana.cat == "no")]), "-",
                         length(site[which(liana.cat == "low")]), "-",
                         length(site[which(liana.cat == "high")]), ")"),
         N.low = length(site[which(liana.cat == "low")]),
         N.high = length(site[which(liana.cat == "high")]),
         N.tot = length(site))

sites <- unique(all.df.title$site)
# sites <- c("BCI")

models <- c("weibull","power","gmm")
model.forms <- c("all","none","a","b","ab","bk","ak","k")

# Compile the outputs
fit.all.sites <- list()

df.all <- data.frame()
for (isite in seq(1,length(sites))){

  csite <- sites[isite]
  csite.corrected <- gsub(" ", "",csite, fixed = TRUE)

  print(paste(csite))

  print("- Reading")
  fit.all.sites[[isite]] <- list()

  all.possible.files <- crossing(sites[isite], models,model.forms) %>%
    mutate(n = paste0("Fit.",
                      as.character(csite.corrected),
                      ".",
                      as.character(models),
                      "_",
                      as.character(model.forms),
                      ".RDS")) %>%
    pull(n)

  cfiles <- list.files(file.path("./data/",csite.corrected),full.names = TRUE,pattern = "*.RDS")

  tokeep <- basename(cfiles) %in% all.possible.files

  cfiles.filtered <- cfiles[tokeep]
  cnames.filtered <- tools::file_path_sans_ext(cfiles.filtered)

  if (length(cfiles.filtered) == 0) next()

  for (ifile in seq(1,length(cfiles.filtered))){

    print(paste("--",ifile/length(cfiles.filtered)))
    fit.all.sites[[isite]][[cnames.filtered[ifile]]] <-  file.exists((paste0(cfiles.filtered[ifile])))
  }

  print("- Processing")

  for (imodel in seq(1,length(fit.all.sites[[isite]]))){

    cfile <- names(fit.all.sites[[isite]])[imodel]

    print(paste("--",imodel/length(fit.all.sites[[isite]])))

    cmodel <- basename(cfile)
    cmodel.name <- strsplit(cmodel,"\\.")[[1]][3]
    cmodelform <- sub("\\_.*", "",cmodel.name)
    fixed.effects <- sub(".*\\_", "",cmodel.name)

    df.all <- bind_rows(list(df.all,
                             data.frame(site = csite,
                                        time = file.info(paste0(cfile,".RDS"))[["mtime"]],
                                        model.name = cmodel.name,
                                        model.form = cmodelform,
                                        fe = fixed.effects)))
  }
}

saveRDS(df.all,
        "Check.Bayesian.sites.RDS")

# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/check.sites.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
