rm(list = ls())
# Assuming the files have been downloaded
# loading rGEDI package
library(rGEDI)
library(dplyr)
library(sp)
library(leaflet)
library(leafsync)
library(tidyr)
library(ggplot2)

# Study area boundary box coordinates
# https://datadryad.org/stash/dataset/doi%253A10.15146%252FR3FH61
# site.name <- "BCI"
# ul_lat <- 9.156129
# lr_lat <- 9.152909
# ul_lon <- -79.852753
# lr_lon <- -79.846058
#ul_lat<- 9.1
#lr_lat<- 9.3
#ul_lon<- -79.8
#lr_lon<- -80.0

# Harvard
#site.name <- "Harvard"
#ul_lat<- 42.5
#lr_lat<- 42.55
#ul_lon<- -72.15
#lr_lon<- -72.2

#site.name <- "Paracou"
#ul_lat<- 5.2
#lr_lat<- 5.4
#ul_lon<- -52.8
#lr_lon<- -53

# Figure 1 https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2745.13155
site.name <- "Gigante"
ul_lat <- 9.125
lr_lat <- 9.105
ul_lon <- -79.86
lr_lon <- -79.845

# Get path to GEDI data
gLevel1B<-gedifinder(product="GEDI01_B",ul_lat, ul_lon, lr_lat, lr_lon,version="002")
gLevel2A<-gedifinder(product="GEDI02_A",ul_lat, ul_lon, lr_lat, lr_lon,version="002")
gLevel2B<-gedifinder(product="GEDI02_B",ul_lat, ul_lon, lr_lat, lr_lon,version="002")

# Set output dir for downloading the files
outdir=file.path(getwd(),"data","GEDI",site.name)

# Downloading GEDI data
# gediDownload(filepath=gLevel1B,outdir=outdir)
# LianaGEDI::gediDownload(filepath=gLevel2A,outdir=outdir)
# LianaGEDI::gediDownload(filepath=gLevel2B,outdir=outdir)
# Reading GEDI data and merge

ifile=4
gedilevel1b <- readLevel1B(level1Bpath = file.path(outdir,paste0(sub('\\.h5$','',basename(gLevel1B[ifile])),"_sub.h5")))
level1bGeo <- getLevel1BGeo(level1b=gedilevel1b,select=c("elevation_bin0","elevation_lastbin","beam"))
head(level1bGeo)

# plot(level1bGeo %>% filter(beam == 5) %>% pull(elevation_bin0),type='l',ylim=c(-50,250))
# lines(level1bGeo %>% filter(beam == 5) %>% pull(elevation_lastbin),type='l',col='red')
# plot(level1bGeo %>% filter(beam == 5) %>% pull(elevation_bin0)-
#        level1bGeo %>% filter(beam == 5) %>% pull(elevation_lastbin),type='l',ylim=c(-50,250))

level1bGeo$shot_number <- paste0(level1bGeo$shot_number)

# Converting level1bGeo as data.table to SpatialPointsDataFrame
level1bGeo_spdf<-SpatialPointsDataFrame(cbind(level1bGeo$longitude_bin0, level1bGeo$latitude_bin0),
data=level1bGeo)

level1bGeo_filtered <- level1bGeo %>% filter(beam %in% c(2,5,6,8,11)) %>% mutate(shot_number_index = as.numeric(substr(shot_number,10, nchar(shot_number)))) %>%
  group_by(beam) %>% mutate(index = shot_number_index + 1 - min(shot_number_index))
# %>% filter(latitude_lastbin>9.15 & latitude_lastbin<9.16) %>%
# filter(longitude_lastbin>-79.95 & longitude_lastbin < -79.83)

# Exporting level1bGeo as ESRI Shapefile
shot.number <- level1bGeo_filtered$shot_number[runif(1,1,nrow(level1bGeo_filtered))]
tmp <- level1bGeo_filtered %>% filter(shot_number == shot.number)
N=10
level1bGeo_filtered_SN <- level1bGeo_filtered %>% filter(index %in% seq(tmp$index-N,tmp$index+N),
                                                         beam == tmp$beam)
pal <- colorNumeric(
  palette = colorRampPalette(c('red', 'green'))(length(level1bGeo_filtered$elevation_bin0)),
  domain = level1bGeo_filtered$elevation_bin0)

data.loc.plots <- read.csv(file.path(getwd(),"data","location_plots.csv")) %>%
  mutate(Treatment = case_when(Plot %in% c(1,4,6,10,8,12,13,16) ~ "Control",
                               TRUE ~ "Removal")) %>%
  mutate(Col = case_when(Treatment == "Control" ~ "#000000",
                         TRUE ~ "#FFFFFF"))

leaflet(level1bGeo_filtered) %>%
  addCircleMarkers(~longitude_bin0,
                   ~latitude_bin0,
                   radius = 1,
                   opacity = 1,
                   fillOpacity = 1,
                   color =  ~pal(elevation_bin0))  %>%
  addCircleMarkers(level1bGeo_filtered_SN$longitude_bin0,
                   level1bGeo_filtered_SN$latitude_bin0,
                   radius = 1,
                   opacity = 1,
                   fillOpacity = 1,
                   color = "red")  %>%
  addRectangles(
    lng1 = data.loc.plots %>% filter(Corner == "ll") %>% pull(X),
    lat1 = data.loc.plots %>% filter(Corner == "ll") %>% pull(Y),
    lng2 = data.loc.plots %>% filter(Corner == "ur") %>% pull(X),
    lat2 = data.loc.plots %>% filter(Corner == "ur") %>% pull(Y),
    fillColor = "transparent",
    color = data.loc.plots %>% filter(Corner == "ur") %>% pull(Col)
  ) %>%
  addScaleBar(options = list(imperial = FALSE)) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addLegend(colors = "red",
            labels = "Samples",
            title = "GEDI Level1B")

