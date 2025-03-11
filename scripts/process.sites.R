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
                      filter(dbh >= 10) %>%
                      mutate(site = "Total.re"))

# all.df <- bind_rows(readRDS("./outputs/All.COI.data.RDS") %>%
#                       mutate(sp = str_squish(sp)) %>%
#                       filter(dbh >= 10))


all.df.title <- all.df %>%
  group_by(site) %>%
  mutate(site.N = paste0(site,", N = ", length(site)," (",length(site[which(liana.cat == "no")]), "-",
                         length(site[which(liana.cat == "low")]), "-",
                         length(site[which(liana.cat == "high")]), ")"),
         N.low = length(site[which(liana.cat == "low")]),
         N.high = length(site[which(liana.cat == "high")]),
         N.tot = length(site))

sites <- unique(all.df.title$site)
# sites <- c(readRDS("./data/rainfor.sites.RDS"),
#            c("Dja North","Kisangani_all"),
#            "Total.re")

# sites <- c("BUL","DAN","LAM","SGW")
# sites <- c("Loundoungou")
# sites <- c("Sherman","Canal")
# sites <- c("129","357")
# sites <- readRDS("./data/rainfor2.md.RDS") %>%
#   pull(group) %>% unique()

models <- c("weibull","power","gmm")
model.forms <- c("all","none","a","b","ab","bk","ak","k")

overwite <- TRUE
# Compile the outputs
# fit.site <- list()

# df.all <- data.frame()
for (isite in seq(1,length(sites))){

  csite <- sites[isite]
  csite.corrected <- gsub(" ", "",csite, fixed = TRUE)

  print(paste0(csite," - ", isite,"/",length(sites)))

  OP.file <- file.path("./data/",csite.corrected,"Diagnostics.RDS")
  if (!overwite & file.exists(OP.file)){
    next()
  }

  df.site <- data.frame()

  all.possible.files <- crossing(sites[isite], models,model.forms) %>%
    mutate(n = paste0("Fit.",
                      ifelse(csite == "Total.re",
                             "Total",
                             as.character(csite.corrected)),
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
    cwaic <- waic(fit.site)
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
      data.frame(site = csite,
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
          OP.file)
}

# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/process.sites.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
