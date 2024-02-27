rm(list = ls())

library("R.matlab")
library("ITSMe")
library("LianaRemovalRevisited")
library(data.table)

ref.dir <- "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/pointclouds/"
raw.files <- list.files(path = ref.dir,pattern = "*.txt",
                        include.dirs = FALSE,
           full.names = FALSE)

files <- unique(sub("_wood.txt","",sub("_leaf.txt", "", raw.files)))

for (i in seq(91,length(files))){
  print(i)
  ctree <- files[i]

  leaf.file <- paste0("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/pointclouds/",
                               ctree,"_leaf.txt")
  wood.file <- paste0("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/pointclouds/",
                      ctree,"_wood.txt")

  lsize <- file.info(leaf.file)$size
  wsize <- file.info(wood.file)$size

  if (lsize > 0 & wsize > 0){
    pc <- bind_rows(read.csv(leaf.file,  sep = " ", header = FALSE),
                    read.csv(wood.file,  sep = " ", header = FALSE))
  } else if (wsize > 0){
    pc <- read.csv(wood.file,  sep = " ", header = FALSE)
  } else if (lsize > 0){
    pc <- read.csv(leaf.file,  sep = " ", header = FALSE)
  } else{
    next()
  }

  fwrite(pc,
         file = paste0("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/pointclouds/QSM2/pc/",
                       ctree,".mat_pc.txt"),
         sep = " ", row.names = FALSE, col.names = FALSE)

}

A <- LianaRemovalRevisited::summary_qsm_metrics(
  QSMs_path = "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/pointclouds/QSM2/",
  PCs_path = "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/pointclouds/QSM2/pc/",
  OUT_path = "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/pointclouds/summary")
#

