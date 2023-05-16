rm(list = ls())

library(reticulate)

setwd("/data/gent/vo/000/gvo00074/felicien/ELIE/")

cdsapi <-import("cdsapi")
c <- cdsapi$Client()

c$retrieve(
  'satellite-land-cover',
  list(
    'variable' = 'all',
    'format' = 'zip',
    'year' = '2020',
    'version' = 'v2.1.1'),
  paste0('LC.zip'))

# ml GDAL
# gdalwarp -of Gtiff -co COMPRESS=LZW -co TILED=YES -ot Byte -te -180.0000000 -90.0000000 180.0000000 90.0000000 -tr 0.002777777777778 0.002777777777778 -t_srs EPSG:4326 NETCDF:C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc:lccs_class C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.tif

