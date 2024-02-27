rm(list = ls())

# Group

library(dplyr)
library(rgl)
library(viewshed3d)
library(plot3D)
library(ggplot2)
library(lidR)

raw.data <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/2019_TLS_data.csv",stringsAsFactors = FALSE) %>%
  rename(dbh = Final.DBH..cm.,
         CA = CPA..m2.) %>%
  mutate(liana.cat = case_when(Liana == 0 ~ "no",
                               Liana == 1 ~ "low",
                               Liana == 2 ~ "high"))

colors <- c("no" = "darkgreen",
            "low" = "orange",
            "high"= "darkred")

raw.data.selected <- raw.data %>%
  mutate(dbh.cat = round(dbh/20,0)) %>%
  group_by(Species,dbh.cat) %>%
  mutate(Ncat = length(unique(liana.cat)),
         N = n(),
         Nmin = min(table(liana.cat))) %>%
  filter(Ncat >= 3) %>%
  arrange(Species,dbh.cat,Liana) %>%
  mutate(sp.cat = paste(Species,dbh.cat)) %>%
  ungroup() %>%
  dplyr::select(Tag,Species,dbh,sp.cat,liana.cat,CA,Height..m.,N,Nmin) %>%
  filter(sp.cat == unique(sp.cat)[1]) %>%
  arrange(liana.cat,dbh) %>%
  group_by(liana.cat) %>%
  filter(dbh >= 40 & dbh <= 45)


Ntrees <- nrow(raw.data.selected)
thr.hold = 1
BoxSize = 0.1
Select = 1

Dir <- "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/pointclouds/"
all.pcs <- list.files(Dir)

options(rgl.printRglwidget = TRUE)

all.trees <- data.frame()
crs_tls<-sp::CRS("+init=epsg:26918")

ch.all <- data.frame()


for (itree in seq(1,Ntrees)){

  print(itree/Ntrees)

  cspecies <- raw.data.selected$sp[itree] ; cliana <- raw.data.selected$liana.cat[itree]
  cdbh <- raw.data.selected$dbh[itree] ; ch <- raw.data.selected$h[itree] ; corder <- raw.data.selected$order[itree]

  files <- file.path(Dir,
                     all.pcs[grepl(paste0("_",raw.data.selected$Tag[itree],"_"),all.pcs)])

  ctree <- data.frame()

  for (ifile in seq(1,length(files))){

    cfile <- files[ifile]
    cdata <- tryCatch(read.table(files[ifile]),
                      error = function(e) NULL)

    if (is.null(cdata)) next()

    ctree <- bind_rows(ctree,
                       as.data.frame(cdata %>%
                                       rename(X = V1, Y = V2, Z = V3) %>%
                                       mutate(organ = case_when(grepl("leaf",cfile) ~ "Leaf",
                                                                grepl("wood",cfile) ~ "Wood",
                                                                TRUE ~ "Other"))))
  }


  las <-LAS(ctree, proj4string = crs_tls)
  las.dw <- downsample_scene(downsample_scene(las, method = "space", BoxSize),
                             method = "random",Select)

  ctree.dw <- as.data.frame(payload(las.dw))

  ctree.rescaled <- ctree.dw %>%
    mutate(base = Z <= (min(Z) + thr.hold)) %>%
    mutate(X = X- min(X[base]),
           Y = Y - min(Y[base]),
           Z = Z - min(Z[base]))

  # with(ctree.rescaled,plot3d(X, Y, Z, type = "p", size = 1,xlab = "",ylab = "",zlab="",
  #                      aspect = TRUE))

  ctree.position <- ctree.rescaled %>%
    dplyr::mutate(species = cspecies,
                  h = ch,
                  dbh = cdbh,
                  liana.cat = cliana,
                  id = itree) %>%
    mutate(Delta_X = (itree - 1)*10,
           Delta_Y = 0) %>%
    # mutate(Delta_X = runif(n = 1,-1,1)*20,
    #        # Delta_Y = (corder-1)*5 + runif(n = 1,-1,1)*2.5) %>%
    #        Delta_Y = runif(n = 1,-1,1)*20) %>%
    # Delta_Y = (-as.numeric(cliana)+1)*5 + runif(n = 1,-1,1)*2.5) %>%
    mutate(X = X + Delta_X,
           Y = Y + Delta_Y)

  all.trees <- bind_rows(all.trees,
                         ctree.position %>%
                           ungroup())


}

all.trees.colored <- all.trees %>%
  mutate(liana.cat.num = as.numeric(factor(liana.cat,
                                           levels = c("no","low","high")))) %>%
  mutate(color = case_when(organ == "Leaf" ~ colors[liana.cat.num],
                           organ == "Wood" ~ colors[liana.cat.num],
                           TRUE ~ "black"))


bg3d(color = "white")
view3d(theta = 0, phi = -90)

with(all.trees.colored, plot3d(X, Y, Z, col = color, alpha = 0.5,
                               type = "p", size = 1,xlab = "",ylab = "",zlab="",
                               aspect = TRUE,main = "", sub = "", ann = FALSE, axes = FALSE))

saveRDS(raw.data.selected,
        "./outputs/plot3d.group.RDS")
