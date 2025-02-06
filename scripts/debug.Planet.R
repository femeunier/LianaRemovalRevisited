rm(list = ls())
r <- stack("/home/femeunier/Downloads/Planet/data/20180715_154844_104d_3B_udm2_clip.tif")

M <- read_sf("/home/femeunier/Downloads/GigantePlots/Liana_Plot_Fixed_60m.shp")

m <- M %>%
        filter(Plot == 3)
m.transformed <- st_transform(m,crs = proj4string(r))

r.masked <- mask(r,m.transformed)

plot(r.masked[[1]])

hist(as.vector(r.masked[[7]]))


rm(list = ls())

library(lidR)
data_path <- "~/Downloads/ALS/"
files <- list.files(data_path, pattern = ".las$", full.names =  TRUE)

#LASfile <- system.file("extdata", "plot13_veg_squ.las", package ="lidR")
las <- lapply(files, FUN = readLAS)

#calculate canopy height model
chm <- sapply(las, FUN = rasterize_canopy, res = 1, algorithm = p2r())
# the canopy height model

library(terra)
mos <- do.call(mosaic, chm)
saveRDS(mos, "~/Downloads/ALS/mosaic_chm.RDS")
