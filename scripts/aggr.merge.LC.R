rm(list = ls())

library(raster)
library(terra)

dir <- "/data/gent/vo/000/gvo00074/felicien/ELIE"

files <- c("C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.tif")
Names <- c("2020")
r_res <- list()

# e <- extent(-10,10,-5,5)
e <- ext(-100,150,-90,90)

for (ifile in seq(1,length(files))){

  file <- file.path(dir,files[ifile])
  print(file)
  r <- rast(file)
  rc <- terra::crop(r, e)
  # r_res[[ifile]] <- terra::aggregate(rc, fact=0.05/res(rc),fun = modal,na.rm = TRUE)

  r_res[[ifile]] <- raster::aggregate(raster(rc),
                                     fact=0.05/res(rc),
                                     fun = modal)

  writeRaster(r_res[[ifile]] ,
              file.path(paste0(tools::file_path_sans_ext(file),"_global_aggr.tif")),overwrite = TRUE)


}

# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/aggr.merge.LC.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R

# names(r_res) <- Names
#
# rsrc <- terra::sprc(r_res)
# r.all <- merge(rsrc)
#
# writeRaster(r.all, '/data/gent/vo/000/gvo00074/felicien/Tyukavina/pantropics.tif',overwrite = TRUE)
