rm(list = ls())

library(rgdal)
library(raster)

location.file <- "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/location_plots.csv"
data = read.csv(location.file)

Xmin <- min(data$X) - 0.001 ; Xmax <- max(data$X) + 0.001
Ymin <- min(data$Y) - 0.001 ; Ymax <- max(data$Y) + 0.001

out = extent(c(Xmin,Xmax,Ymin,Ymax))
crs(out) <- "+proj=longlat +ellps=WGS84 +no_defs "
out = as(out, 'SpatialPolygons')
out = as(out, 'SpatialPolygonsDataFrame')

writeOGR(out, dsn = '.', layer = 'LianaRemovalExtent', driver = "ESRI Shapefile")

delta_lon = 111.32*(Xmax - Xmin)
latitude = mean(Ymin,Ymax)/180*(pi)
delta_lat = 40075*cos(latitude)/360*(Ymax-Ymin)

total_area = delta_lat*delta_lon*10*10


########################################################################################################
# Exact data

library(sf)
plots <- read_sf(dsn = "./data/Stefan/Liana_Plot_Fixed_60m.shp")

# plots[[5]]:
# xmin: -79.85874 ymin: 9.106186 xmax: -79.84958 ymax: 9.120621

Xmin <- -79.85874 ; Xmax <- -79.84958
Ymin <- 9.106186 ; Ymax <- 9.120621

out = raster::extent(c(Xmin,Xmax,Ymin,Ymax))
out = as(out, 'SpatialPolygons')
out = as(out, 'SpatialPolygonsDataFrame')
crs(out) <- "+proj=longlat +ellps=WGS84 +no_defs "

writeOGR(out, dsn = '.', layer = 'LianaRemovalExtent', driver = "ESRI Shapefile",overwrite_layer = TRUE)

delta_lon = 111.32*(Xmax - Xmin)
latitude = mean(Ymin,Ymax)/180*(pi)
delta_lat = 40075*cos(latitude)/360*(Ymax-Ymin)

total_area = delta_lat*delta_lon*10*10
plot(out)
plot(plots, add = T)


