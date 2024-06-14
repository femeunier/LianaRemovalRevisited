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
# Crowns

trees <-  shapefile("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/Danum/All_trees1.shp")
sftrees <- st_as_sf(trees)


corners <- data.frame(
  Y = -133 + c(548500,548500,547500,547500),
  X = -702 + c(588500,589000,588500,589000))
plot <- st_as_sf(corners, coords=c("X","Y")) %>%
  dplyr::summarise() %>%
  st_cast("POLYGON") %>%
  st_convex_hull()
E <- extent(plot)

e <- as(extent(trees), 'SpatialPolygons')

str_name<- "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/Danum/chm.tif"
chm=raster(str_name)

r.df <- as.data.frame(cropped.chm <- mask(crop(chm,e),e),xy = TRUE)
r.df$CHM <- as.vector(cropped.chm)
r.df <- r.df %>%
  filter(!is.na(CHM))

ggplot(r.df) +
  geom_polygon(data=e, aes(x=long, y=lat, group=group),
               fill=NA,color="black")+
  geom_tile(aes(x =x, y = y,
                fill=CHM),
            alpha = 0.5) +
  geom_polygon(data=trees[,], aes(x=long, y=lat, group=group),
               fill=NA,color="black")+
  coord_equal() +
  theme_bw()

Ntrees <- nrow(trees)
df.all <- data.frame()

for (itree in seq(1,Ntrees)){

  print(itree/Ntrees)
  ctree.num <- itree
  ctree <- ((trees@polygons[[ctree.num]])@Polygons[[1]])@coords

  tree.vec <- as.data.frame(ctree) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")

  cx = as.vector(ctree[,1]); cy = as.vector(ctree[,2])

  chm.crop <- mask(crop(chm,extent(tree.vec)),tree.vec)

  ch.max <- max(as.vector(
    chm.crop),na.rm = TRUE)
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
                                      h = ch.max,
                                      area = cArea,
                                      Liana = cLiana,
                                      COI = cCOI,
                                      liana.cat = cliana.cat)))
}

hist(df.all$Liana)

# df.all.cat <- df.all %>%
#   mutate(dbh = dbh) %>%
#   mutate(dbh.cat = round(dbh/50,digits = 0)) %>%
#   group_by(liana.cat,dbh.cat) %>%
#   summarise(h = mean(h,na.rm = TRUE),
#             dbh = mean(dbh,na.rm = TRUE),
#             .groups = "keep")

df.selected <- df.all

ggplot(data = df.selected ,
       aes(x = h, y = area, color = liana.cat,
           fill = liana.cat)) +
  geom_point() +
  # scale_x_log10() +
  # scale_y_log10() +
  stat_smooth(method = "lm",
              se = TRUE) +
  theme_bw()

# plot(df.all$Liana,df.all$COI)
c(table(df.selected$liana.cat),nrow(df.selected),Ntrees)
# sort(table(df.all$sp))


ggplot(data = df.all) +
  geom_boxplot(aes(x = liana.cat,
                   y = area,
                   fill = liana.cat)) +
  scale_y_log10() +
  theme_bw()


saveRDS(df.all %>%
          dplyr::select(ID,h,area,Liana,COI,liana.cat),
        "./data/Danum/h.CA.DV.RDS")



# a <- ((trees@polygons[[2]])@Polygons[[1]])@coords
# plot(a[,1],a[,2],col = "red", type = "l")
