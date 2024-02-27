rm(list = ls())

# AGB

library(dplyr)
library(rgl)
library(viewshed3d)
library(ggplot2)

raw.data <- readRDS("./outputs/BCI.COI.data.RDS") %>%
  filter(year == 2019)

ggplot(data = raw.data) +
  geom_point(aes(x = dbh, y = h, color = liana.cat)) +
  theme_bw()

colors <- c("no" = "darkgreen",
            "low" = "orange",
            "high"= "darkred")

raw.data.cat <- raw.data %>%
  mutate(dbh.cat = round(dbh/10,0)) %>%
  group_by(liana.cat,dbh.cat) %>%
  mutate(h.m = mean(h)) %>%
  mutate(diff = round(abs(h.m - h),digits = 2))

raw.data.selected <- raw.data.cat %>%
  arrange(diff,h) %>%
  slice_head(n = 1) %>%
  group_by(dbh.cat) %>%
  arrange(h) %>%
  mutate(order = 1:length(site)) %>%
  ungroup() %>%
  mutate(id = 1:n())
# filter(dbh > 200)
# slice_sample(n = 50)

dbh.sum <- summary(raw.data.selected$dbh)

Ntrees <- nrow(raw.data.selected)
thr.hold = 1
BoxSize = 0.05
Select = 1

Dir <- "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/pointclouds/"
all.pcs <- list.files(Dir)

options(rgl.printRglwidget = TRUE)

all.trees <- data.frame()
crs_tls<-sp::CRS("+init=epsg:26918")

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


  las <-LAS(ctree %>% filter(organ != "Leaf"), proj4string = crs_tls)
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
    mutate(Delta_X = dbh - max(0,(dbh-100))/2,
           # Delta_Y = (corder-1)*5 + runif(n = 1,-1,1)*2.5) %>%
    Delta_Y = 0) %>%
    # Delta_Y = (-as.numeric(cliana)+1)*5 + runif(n = 1,-1,1)*2.5) %>%
    mutate(X = X + Delta_X,
           Y = Y + Delta_Y)

  all.trees <- bind_rows(all.trees,
                         ctree.position %>%
                           ungroup())


}

all.trees.colored <- all.trees %>%
  mutate(liana.cat.num = as.numeric(as.factor(liana.cat))) %>%
  mutate(color = colors[liana.cat.num])


bg3d(color = "white")
view3d(theta = 0, phi = -90)

with(all.trees.colored, plot3d(X, Y, Z, col = color, alpha = 0.5,
                               type = "p", size = 1,xlab = "",ylab = "",zlab="",
                               aspect = TRUE,main = "", sub = "", ann = FALSE, axes = FALSE))

saveRDS(raw.data.selected,
        "./outputs/plot3d.AGB.RDS")
