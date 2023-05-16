rm(list = ls())

location.file <- "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/location_plots.csv"
locations <- read.csv(location.file)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

library(ggmap)
library(gridExtra)

maptype <- c("terrain-labels", "terrain-lines", "toner", "toner-2011",
             "toner-background", "toner-hybrid", "toner-lines",
             "toner-lite", "watercolor")

map <- get_stamenmap( bbox = c(left = -79.9, bottom = 9.05, right = -79.8,
                               top = 9.2), zoom = 12, maptype = maptype[9])

ggmap(map) +
  scale_y_continuous(limits = c(9.1,9.2)) +
  scale_x_continuous(limits = c(-79.9,-79.8))  +
  geom_point(aes(x = X, y = Y),
             data = locations %>% filter(Corner == "ll"),
             shape = 16)  +
  theme_minimal()




