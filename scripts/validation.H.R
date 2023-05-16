rm(list = ls())

library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(ggplot2)

tif.file <- "./data/Forest_height_2019_Panama.tif"

r <- raster(tif.file)
r[r>= 100] <- NA
plot(r)

data.loc.plots <- read.csv(file.path(getwd(),"data","location_plots.csv")) %>%
  mutate(Treatment = case_when(Plot %in% c(1,4,6,10,8,12,13,16) ~ "Control",
                               TRUE ~ "Removal"))

df.height <- data.frame()
for (iplot in seq(1,max(data.loc.plots$Plot))){

  cplot <- data.loc.plots %>% filter(Plot == iplot)

  xmin <- cplot %>% filter(Corner == "ll") %>% pull(X)
  xmax <- cplot %>% filter(Corner == "ur") %>% pull(X)
  ymin <- cplot %>% filter(Corner == "ll") %>% pull(Y)
  ymax <- cplot %>% filter(Corner == "ur") %>% pull(Y)

  c_extent <- as(extent(c(xmin + (xmax -xmin)/4,
                          xmax - (xmax -xmin)/4,
                          ymin + (ymax -ymin)/4,
                          ymax - (ymax -ymin)/4)), 'SpatialPolygons')
  c_r <- crop(r,c_extent)
  df.height <- bind_rows(list(df.height,
                              data.frame(plot = iplot,
                                         Treatment = cplot %>% filter(Corner == "ll") %>% pull(Treatment),
                                         h = max(as.vector(c_r)))))

}

ggplot(data = df.height) +
  geom_boxplot(aes(x = Treatment,
                   y = h)) +
  theme_bw()


summary(lm(data = df.height,
           formula = h ~ Treatment))

# tif.file <- "/home/femeunier/Downloads/Forest_height_2019_SAM.tif"
# e <- as(extent(c(-80.2,-79.6,8.9,9.3)), 'SpatialPolygons')
# crs(e) <- crs(r)
#
# r_cropped <- crop(r,e)
#
# plot(r_cropped)
#
# writeRaster(r_cropped,'./data/Forest_height_2019_Panama.tif')
