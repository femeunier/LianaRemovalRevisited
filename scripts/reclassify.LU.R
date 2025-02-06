rm(list = ls())

r <- raster("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1_aggr_reclassified.tif")
df.r <- as.data.frame(r,xy = TRUE) %>%
  rename(lon =  x,
         lat = y,
         LU = layer) %>%
  mutate(lat = round(lat,digits = 3),
         lon = round(lon,digits = 3)) %>%
  filter(!is.na(LU))


r2 <- raster("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1_global_aggr.tif")
df.r2 <- as.data.frame(r2,xy = TRUE) %>%
  rename(lon =  x,
         lat = y,
         LU = layer) %>%
  mutate(lat = round(lat,digits = 3),
         lon = round(lon,digits = 3)) %>%
  filter(abs(lat) <= 23.125) %>%
  filter(!is.na(LU))

trans <- df.r %>%
  left_join(df.r2,
            by = c("lat","lon")) %>%
  dplyr::select(LU.x,LU.y) %>%
  distinct()

mod_raster <- crop(reclassify(r2,
                         matrix(c(trans$LU.y,trans$LU.x),ncol = 2)),
                   extent(c(-180,180,-30,30)))

mod_raster[mod_raster>200] <- NA
writeRaster(mod_raster,
            "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1_pantropical_aggr.tif",
            options=c('TFW=YES'))
