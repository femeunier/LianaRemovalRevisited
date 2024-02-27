rm(list = ls())

library(ncdf4)
library(raster)
library(reshape2)
#
# ncfile <- "/data/gent/vo/000/gvo00074/felicien/ESA_Biomass/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2018-fv3.0.nc"
# A <- raster(ncfile)
# e <- extent(c(-180,180,-32,25))
# Acrop <- crop(A,e)
#
# #
# # AGB.df <- melt(AGB) %>%
# #   mutate(lon = lons[Var1],
# #          lat = lats[Var2]) %>%
# #   dplyr::select(-c(Var1,Var2)) %>%
# #   rename(agb = value) %>%
# #   filter(abs(lat) <= 25)
# # nc_close(nc)
# #
# # grid <- rasterFromXYZ((AGB.df %>%
# #                          ungroup() %>%
# #                          dplyr::select(c("lat","lon","agb")))[,c("lon","lat","agb")])
#
# ESA.repojected <- resample(Acrop,
#                            raster("/data/gent/vo/000/gvo00074/felicien/R/data/Avitabile_AGB_Map_reproj.tif"),
#                            "bilinear")
#
# writeRaster(ESA.repojected,
#             "./data/ESA.reprojected.tif",
#             overwrite=T)


# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/reproject.ESA.R hpc:/data/gent/vo/000/gvo00074/felicien/R

###################################################################################


rm(list = ls())

library(ncdf4)
library(dplyr)
library(raster)
library(reshape2)

file <- "/data/gent/vo/000/gvo00074/felicien/R/data/GEDI04_B_MW019MW223_02_002_02_R01000M_MU.tif"
r <- raster(file)
rbis <- projectRaster(r,
                      crs = crs( raster("/data/gent/vo/000/gvo00074/felicien/R/data/Avitabile_AGB_Map_reproj.tif")))
df <- as.data.frame(rbis,
                    xy = TRUE) %>%
  filter(!is.na(GEDI04_B_MW019MW223_02_002_02_R01000M_MU),
         abs(y) <= 32) %>%
  rename(agb = GEDI04_B_MW019MW223_02_002_02_R01000M_MU)

r2 <- rasterFromXYZ((df %>%
                         ungroup() %>%
                         dplyr::select(c("x","y","agb")))[,c("x","y","agb")])

GEDI.repojected <- resample(r2,
                            raster("/data/gent/vo/000/gvo00074/felicien/R/data/Avitabile_AGB_Map_reproj.tif"),
                           "bilinear",na.rm = TRUE)

writeRaster(GEDI.repojected,
            "./data/GEDI.repojected.tif",
            overwrite=T)

system2("scp",c("hpc:/data/gent/vo/000/gvo00074/felicien/R/data/GEDI.repojected.tif",
                "./data/"))

system2("scp",c("hpc:/data/gent/vo/000/gvo00074/felicien/R/data/ESA.reprojected.tif",
                "./data/"))
r <- raster("./data/ESA.reprojected.tif")
r <- raster("/home/femeunier/Documents/projects/YGB/data/Avitabile_AGB_Map/Avitabile_AGB_Map_reproj.tif")


#####################################################################################################
Biomass.Trendy <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/Trendy.pantropical.S2.cAGB.v11.recent.RDS") %>%
  pivot_wider(names_from = "variable",
              values_from = "value") %>%
  mutate(cAGB = case_when(!is.na(cRoot) ~ cVeg - cRoot,
                          TRUE ~ cVeg*0.7)) %>%                   # Assuming 70% of the vegetation is AGB
  dplyr::filter(!is.na(cAGB))

models <- unique(Biomass.Trendy$model)
all.df <- data.frame()
for (cmodel in models){

  print(cmodel)
  cdf <- Biomass.Trendy %>%
    filter(model == cmodel) %>%
    filter(year == max(year)) %>%
    mutate(cAGB = pmax(cAGB,0))

  lat <- cdf$lat ; lon <- cdf$lon
  grid <- tryCatch(rasterFromXYZ(cdf[,c("lon","lat","cAGB")]),
                   error = function(err){NA})

  if (all(is.na(as.vector(grid)))){
    res <- max(c(diff(sort(unique(lat))),diff(sort(unique(lon)))))
    grid <- raster(SpatialPixelsDataFrame(points = cdf[c("lon","lat")],
                                             data = cdf["cAGB"],
                                             tolerance = res/10))
  }


  cgrid.repojected <- resample(grid,
                               raster("/home/femeunier/Documents/projects/YGB/data/Avitabile_AGB_Map/Avitabile_AGB_Map_reproj.tif"),
                               "bilinear")
  cdf.rep <- as.data.frame(cgrid.repojected,
                                 xy = TRUE) %>%
    rename(lon = x,
           lat = y)

  all.df <- bind_rows(all.df,
                      cdf.rep %>%
                        ungroup() %>%
                        filter(abs(lat) < 32) %>%
                        mutate(model = cmodel) )

  writeRaster(cgrid.repojected,
              paste0("./data/",cmodel,".grid.tif"),
              overwrite=T)

}

all.df.sum <- all.df %>%
  mutate(cAGB = pmax(cAGB,0)) %>%
  group_by(lat,lon) %>%
  summarise(cAGB = mean(cAGB),
            .groups = "keep")

all.grid <- rasterFromXYZ((all.df.sum %>%
                         ungroup() %>%
                         dplyr::select(c("lat","lon","cAGB")))[,c("lon","lat","cAGB")])

writeRaster(all.grid,
            "./data/all.grid.tif",
            overwrite=T)
