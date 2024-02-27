rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(purrr)
library(BCI.AGB)
library(quantreg)
library(sf)
library(rgdal)
library(raster)
library(lidR)
library(rgl)
library(viewshed3d)
library(dplyr)

################################################################################
# Danum Valley plot
# corners <- data.frame(
#   Y = -133 + c(548500,548500,547500,547500),
#   X = -702 + c(588500,589000,588500,589000))
#
# # 587798    588298   547367 548367
# # 587795.4  588317.1  547361 548364.
#
# plot <- st_as_sf(corners, coords=c("X","Y")) %>%
#   dplyr::summarise() %>%
#   st_cast("POLYGON") %>%
#   st_convex_hull()


plot <-  shapefile("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/Danum/50ha_Boundary.shp")
plot <- as_Spatial(data.frame(x = c(117.7966,117.7921,117.7919,117.7964),
                              y = c(4.951621,4.95153,4.960582,4.960666)) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON"))

# lons <- c(117.792194,117.796611,117.7964,117.791972)
# lats <- c(4.951444,4.951583,4.957194,4.957194)
#
# plot <- as_Spatial(data.frame(x = lons,
#                               y = lats) %>%
#                      st_as_sf(coords = c("x", "y"), crs = 4326) %>%
#                      summarise(geometry = st_combine(geometry)) %>%
#                      st_cast("POLYGON"))
#
# plot(plot)


################################################################################
# Crowns

trees <-  shapefile("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/Danum/All_trees1.shp")
sftrees <- st_as_sf(trees)

sfplot <- st_as_sf(mod.plot <- spTransform(plot,crs(trees)))

corners <- (as.data.frame(mod.plot@polygons[[1]]@Polygons[[1]]@coords) %>%
  rename(X = V1, Y = V2))



sqrt( sum((corners[2,] - corners[1,])**2))
sqrt( sum((corners[3,] - corners[2,])**2))
sqrt( sum((corners[4,] - corners[1,])**2))
sqrt( sum((corners[4,] - corners[3,])**2))

################################################################################
# Inventory
# dir <- "/home/femeunier/Documents/projects/BCI.AGB/data/Danum/"
dir <- "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/Danum/"

files2read <- file.path(dir,c("PlotDataReport06-26-2023_1342305683.txt"))
census <-  read.table(files2read,header = TRUE,sep = "\t",stringsAsFactors = FALSE) %>%
  dplyr::select(Latin,TreeID,Tag,StemID,PX,PY,DBH,Census,Status) %>%
  mutate(x = min(corners$X) + 500 - PY,
         y = min(corners$Y) + PX) %>%
  mutate(xbis = x + (1000-PX)/1000* diff(sort(corners$X)[c(1,2)]),
         ybis = y + (500 - PY)/500* diff(sort(corners$Y)[c(1,2)])) %>%
  mutate(x = xbis,
         y = ybis)

inters <- (st_intersects(sfplot, sftrees))[[1]]
ratio <- (as.vector(st_area(st_intersection(sfplot,sftrees)))/
       as.vector(st_area(sftrees[inters,])))

inters <- inters[ratio >= 0.95]

# cplot.sd <- st_as_sf(st_transform(cplot,crs(trees)))


str_name<- "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/Danum/chm.tif"
chm=raster(str_name)

ggplot(data = census %>%
         # filter(PX == min(PX)) %>%
         filter(DBH >= 10*10)) +
  geom_point(aes(y = ybis, x = xbis)) +
  geom_point(data = corners,
             aes(x = X, y = Y), color = "red") +
  coord_equal() +
  theme_bw()

# plot(chm, alpha = 0.5,
#      xlim = c(min(corners$X),max(corners$X)),
#      ylim = c(min(corners$Y),max(corners$Y)))
# # plot(polys, add = TRUE, border = "black",col = adjustcolor("white", alpha.f=0.))
# plot(trees, add = TRUE)
# plot(plot, add = TRUE, color = "red", size = 10)


r.df <- as.data.frame(cropped.chm <- mask(crop(chm,mod.plot),mod.plot),xy = TRUE)
r.df$CHM <- as.vector(cropped.chm)
r.df <- r.df %>%
  filter(!is.na(CHM))

ggplot(r.df) +
  geom_polygon(data=mod.plot, aes(x=long, y=lat, group=group),
               fill=NA,color="black")+
  geom_point(data = census %>%
               filter(DBH >= 20*10),
             aes(y = y, x = x), size = 0.1) +
  geom_tile(aes(x =x, y = y,
                fill=CHM),
            alpha = 0.5) +
  geom_polygon(data=trees[inters,], aes(x=long, y=lat, group=group),
               fill=NA,color="black")+
  coord_equal() +
  # scale_x_continuous(limits = c(min(corners$X),max(corners$X))) +
  # scale_y_continuous(limits = c(min(corners$Y),max(corners$Y))) +
  theme_bw()

Ntrees <- length(inters)

buffer <- 1
df.all <- data.frame()

for (itree in seq(1,Ntrees)){

  print(itree/Ntrees)
  ctree.num <- inters[itree]
  ctree <- ((trees@polygons[[ctree.num]])@Polygons[[1]])@coords

  tree.vec <- as.data.frame(ctree) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")

  cx = as.vector(ctree[,1]); cy = as.vector(ctree[,2])
  xmin <- min(cx) - buffer ; xmax <- max(cx) + buffer
  ymin <- min(cy) - buffer ; ymax <- max(cy) + buffer
  e <- extent(xmin,xmax,ymin,ymax)

  chm.crop <- mask(crop(chm,e),tree.vec)

  pot.trees <- census %>%
    filter(x >= xmin, x <= xmax,
           y >= ymin, y <= ymax,
           DBH >= 0*10)

#   pot.trees.pts <- as.data.frame(pot.trees) %>%
#     st_as_sf(coords = c("x", "y"), crs = 4326) %>%
#     summarise(geometry = st_combine(geometry)) %>%
#     st_cast("POINT")
#   cinters <- as.numeric((st_intersects(pot.trees.pts,tree.vec)))
#   cinters[is.na(cinters)] <- 0
#   pot.trees <- pot.trees[which(cinters == 1),]

  # plot(chm.crop)
  # plot(tree.vec,border = "black",col = adjustcolor("white", alpha.f=0.),
  #      add = TRUE)
  # lines(pot.trees$x,pot.trees$y,type = "p",add = TRUE)

  if (nrow(pot.trees)){

    pot.trees <- pot.trees %>%
    #   # mutate(dist = sqrt(( mean(cx) - x)**2 +  ( mean(cy) - y)**2)) %>%
    #   # arrange(dist) %>%
      arrange(desc(DBH))

    # lines(pot.trees[["x"]],pot.trees[["y"]],type = "p")

    ch.max <- max(as.vector(
      mask(chm.crop,tree.vec)),na.rm = TRUE)
    cdbh <- (pot.trees[["DBH"]])[1]
    csp <- pot.trees[["Latin"]][1]
    cTag <- pot.trees[["Tag"]][1]
    cStatus <- pot.trees[["Status"]][1]
    cArea <- ((trees@polygons[[ctree.num]])@Polygons[[1]])@area
    cLiana <- (trees$Id)[ctree.num]
    cCOI <- ifelse(cLiana == 0,
                   0,
                   ifelse(cLiana >= 75,4,
                          ifelse(cLiana < 25,1,
                                 ifelse(cLiana < 50,2,3)
                          )))
    cliana.cat <- ifelse(cCOI == 0,
                         "no",
                         ifelse(cCOI > 2, "high","low"))

    df.all <- bind_rows(list(df.all,
                             data.frame(ID = itree,
                                        dbh = cdbh,
                                        h = ch.max,
                                        sp = csp,
                                        tag = cTag,
                                        area = cArea,
                                        status = cStatus,
                                        Liana = cLiana,
                                        COI = cCOI,
                                        liana.cat = cliana.cat)))


  }
}

table(df.all$status)
hist(df.all$Liana)

# df.all.cat <- df.all %>%
#   mutate(dbh = dbh) %>%
#   mutate(dbh.cat = round(dbh/50,digits = 0)) %>%
#   group_by(liana.cat,dbh.cat) %>%
#   summarise(h = mean(h,na.rm = TRUE),
#             dbh = mean(dbh,na.rm = TRUE),
#             .groups = "keep")

df.selected <- df.all %>%
  mutate(dbh = dbh/10) %>%
  filter(!(dbh < 50 & h > 60)) # too big trees

ggplot(data = df.selected  %>%
         filter(dbh > 30),
       aes(x = dbh, y = h, color = liana.cat,
           fill = liana.cat)) +
  geom_point() +
  # scale_x_log10() +
  # scale_y_log10() +
  stat_smooth(method = "lm",
              se = TRUE) +
  theme_bw()

ggplot(data = df.selected %>%
         filter(dbh > 30),
       aes(x = dbh, y = area, color = liana.cat)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  stat_smooth(method = "lm",
              se = FALSE) +
  theme_bw()

# plot(df.all$Liana,df.all$COI)
c(table(df.selected$liana.cat),nrow(df.selected),Ntrees)
# sort(table(df.all$sp))

saveRDS(df.selected,
        "./data/Danum/DV.processed.RDS")


# a <- ((trees@polygons[[2]])@Polygons[[1]])@coords
# plot(a[,1],a[,2],col = "red", type = "l")
