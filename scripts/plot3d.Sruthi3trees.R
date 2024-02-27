rm(list = ls())

# Height

library(dplyr)
library(rgl)
library(viewshed3d)
library(ggplot2)
library(lidR)

raw.data <- readRDS("./outputs/BCI.COI.data.RDS") %>%
  filter(year == 2019)

ggplot(data = raw.data) +
  geom_point(aes(x = dbh, y = h, color = liana.cat)) +
  theme_bw()

colors <- c("no" = "darkgreen",
            "low" = "orange",
            "high"= "darkred")


raw.data.selected <- raw.data %>%
  ungroup() %>%
  filter(sp == "Alseis blackiana",
         dbh >= 40,
         dbh <= 50) %>%
  arrange(desc(dbh)) %>%
  filter(Tag %in% c(1718,5891,3145))

dbh.sum <- summary(raw.data.selected$dbh)

Ntrees <- nrow(raw.data.selected)
thr.hold = 1
BoxSize = 0.15
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
    mutate(Delta_X = dbh - max(0,(dbh-100))/2,
           Delta_Y = (3-as.numeric(cliana))*5) %>% # + runif(n = 1,-1,1)*2.5) %>%
    # Delta_Y = 0) %>%
    # Delta_Y = (-as.numeric(cliana)+1)*5 + runif(n = 1,-1,1)*2.5) %>%
    mutate(X = X + Delta_X,
           Y = Y + Delta_Y)

  all.trees <- bind_rows(all.trees,
                         ctree.position %>%
                           ungroup())


}

all.trees.colored <- all.trees %>%
  mutate(liana.cat.num = as.numeric(as.factor(liana.cat))) %>%
  mutate(color = case_when(organ == "Leaf" ~ colors[liana.cat.num],
                           organ == "Wood" ~ colors[liana.cat.num],
                           TRUE ~ "black"))


bg3d(color = "white")
view3d(theta = 0, phi = -90)

with(all.trees.colored, plot3d(X, Y, Z, col = color, alpha = 0.5,
                               type = "p", size = 1,xlab = "",ylab = "",zlab="",
                               aspect = TRUE,main = "", sub = "", ann = FALSE, axes = FALSE))


rgl.postscript('./Figures/Height.png', fmt = 'pdf')

saveRDS(raw.data.selected,
        "./outputs/plot3d.height.RDS")


# with(all.trees.colored,plot3d(X, Y, Z, col = color,
#                               type = "p", size = 1,xlab = "",ylab = "",zlab="",
#                               aspect = TRUE, main = "", sub = ""))

# las<-LAS(wood)
# crs_tls<-sp::CRS("+init=epsg:26918")
# las<-LAS(wood, proj4string = crs_tls)
#
# plot(las,bg = "white", size = 3)


#
# showPointCloud(myOne(:,1);
#
# display.point.cloud(pc, col.palette = NA, col.var = "Z", col.lim = c(0,
#                                                                      50), size = 1)
#
# leaf <- readLAS('~/Downloads/ALBLAC_359_leaf.txt')
