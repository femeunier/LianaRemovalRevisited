library(lwgeom)
library(osmdata)
library(osmplotr)
library(sf)
library(tidyverse)
library(ggmap)
library(ggplot2)
library(raster)
library(maptools)
library(ggthemes)
library(ggsn)
library(maps)
library(mapdata)
library(geodata)

### define example bbox
lon_min <- -80 # xmin
lon_max <- -79 # xmax
lat_min <- 9 # ymin
lat_max <- 10 # ymax


bb <- get_bbox(c(lon_min, lat_min, lon_max, lat_max))

### get "water" that is not sea as polygons
water <- opq(bb) %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf()

### get sea & land as polygons
# 1. get coastline (as line)
coast <- opq(bb) %>%
  add_osm_feature(key = "natural", value = "coastline") %>%
  osmdata_sf()

# 2. get overall rectangle for bbox
bb_rect <- data.frame(
  lat = c(lat_min, lat_max),
  lon = c(lon_min, lon_max)
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_bbox() %>%
  st_as_sfc()

# 3. split overall rectangle for bbox via coastline
bb_rect_split <- bb_rect %>%
  st_split(coast$osm_lines) %>%
  st_collection_extract("POLYGON")

# 4. extract splitted rectangle parts
land <- bb_rect_split[1]
sea  <- bb_rect_split[2]

### ggplot
ggplot() +
  geom_sf(
    data = land,
    fill = "lightgrey",
    color = NA
  ) +
  geom_sf(
    data = sea,
    fill = "navy",
    color = NA
  ) +
  geom_sf(
    data = water$osm_multipolygons,
    fill = scales::muted("navy"),
    color = NA
  ) +
  scale_x_continuous(limits = c(-79.9,-79.8), expand = c(0, 0)) +
  scale_y_continuous(limits = c(9.1, 9.2), expand = c(0, 0)) +
  theme_map() +
  ggsn::scalebar(location = "bottomleft", dist = 2,
                 transform = TRUE, dist_unit = "km",
                 model = 'WGS84',
                 x.min = -79.896, x.max = -79.8,
                 y.min = 9.109, y.max = 9.2) +
  ggsn::north(x.min = -79.89, x.max = -79.807,
              y.min = 9.07, y.max = 9.195,
              location = "topright", scale = 0.1) +
  theme(axis.text = element_text(),
        axis.ticks = element_line(),
        axis.title = element_text()) +
  xlab("Longitude") +
  ylab("Latitude")

#################################################################################

library(ggplot2)
library(rnaturalearth)

world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")


ggplot(data = world) +

  geom_sf(fill = NA) +

  scale_x_continuous(limits = c(-80,-78)) +
  scale_y_continuous(limits = c(7,10)) +

  theme_bw()

library(sf)

plots <- read_sf(dsn = "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/Stefan/Liana_Plot_Fixed_60m.shp")

plot(plots)

############################################################################################################################

library(ggmap)
library(ggplot2)
library(raster)
library(maptools)
library(ggthemes)
library(ggsn)
library(maps)
library(mapdata)
library(geodata)

# library(raster)
# library(ggplot2)
# library(ggsn)
# library(ggmap)
# library(maps)
# library(mapdata)

mymap <- getData("GADM", country = "panama", level = 2)
plot(mymap,
     xlim=c(-79.9,-79.8), ylim=c(9.05, 9.2))


mymap2 <- getData("GADM", country = 'panama',level = 1) %>% fortify()
mypoint <- data.frame(long=c(-79.743, -79.696, -79.645, -79.595),
                      lat=c(9.160, 9.117, 9.058, 9.015),
                      group=c("L", "GW", "OGR", "LC"))

mypoint2 <- data.frame(long=c(-79.846, -79.707, -79.665, -79.610),
                       lat=c(9.181, 9.112, 9.057, 9.014),
                       group=c("BCI", "G", "EH", "MF"))

ggplot() +
  geom_blank(data = mymap,
             aes(x=long, y=lat)) +
  geom_map(data = mymap, map = mymap %>% fortify(),
           aes(group = group, map_id = id,fill = id),
               color = "black", linewidth = 0.1, show.legend = FALSE) +
  coord_sf(xlim=c(-79.9,-79.8), ylim=c(9.05, 9.2), expand = FALSE) +
  # geom_point(data = mypoint, aes(x = long, y = lat),
  #color = "black", size = 3) +
  # geom_label(data = mypoint, aes(label = group, x = long, y = lat),
  #size = 3, fontface = "bold", nudge_x = c(0.015, 0.02, 0.022, 0.018)) +
  # geom_point(data = mypoint2, aes(x = long, y = lat),
  #color = "blue", size = 3) +
  # geom_label(data = mypoint2, aes(label = group, x = long, y = lat),
  #size = 3, fontface = "bold", nudge_x = c(-0.02, -0.018, -0.02, -0.02)) +
  scale_x_continuous(limits = c(-79.9,-79.8), expand = c(0, 0)) +
  scale_y_continuous(limits = c(9.05, 9.2), expand = c(0, 0)) +
  theme_map() +
  ggsn::scalebar(location = "bottomleft", dist = 1,
                 transform = TRUE, dist_unit = "km", model = 'WGS84',
                 x.min = -79.896, x.max = -79.8,
                 y.min = 9.06, y.max = 9.2) +
  ggsn::north(x.min = -79.89, x.max = -79.807,
              y.min = 9.07, y.max = 9.195,
              location = "topright", scale = 0.1) +
  theme(axis.text = element_text(),
        axis.ticks = element_line(),
        axis.title = element_text()) +
  xlab("Longitude") +
  ylab("Latitude")

###########################################################################

library(ggplot2)
library(maps)

# Load world map data
world_map <- map_data("world")

# Filter data to show Central America
panama_map <- world_map[world_map$region == "Panama", ]

# Plot the map of Central America
ggplot(panama_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightblue", color = "black") +
  coord_fixed(1.3) +
  labs(title = "Location of the Panama Canal",
       x = "Longitude",
       y = "Latitude") +
  theme_classic()

###########################################################################

library(ggplot2) # for creating the plot
library(maps)    # for the maps data
library(mapdata)

# Load the map data of Panama
panama_map <- map_data("worldHires", "Panama")

bci_latitude <- 9.14
bci_longitude <- -79.78

# Create a ggplot object
gg <- ggplot() +
  geom_polygon(data = panama_map, aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "gray") +
  # theme(panel.background = element_rect(fill = scales::muted("lightblue"))) +
  # coord_quickmap() +
  coord_cartesian(xlim = c(bci_longitude - 0.5, bci_longitude + 0.5),
                  ylim = c(bci_latitude - 0.5, bci_latitude + 0.5)) + # for zooming in on BCI
  theme_bw()# for an equal aspect ratio

# Add labels for the x and y axes
gg <- gg + xlab("Longitude") + ylab("Latitude")

# Add a title for the plot
gg <- gg + ggtitle("Map of Central Panama")

# Finally, print the plot
print(gg)

##################################################################################################

library(raster)
library(ggplot2)
library(ggthemes)
library(ggsn)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)

mymap <- getData("GADM", country = "panama", level = 3) %>% fortify()
mymap2 <- getData("GADM", country = 'panama',level = 1) %>% fortify()

mypoint <- data.frame(long=c(-79.743, -79.696, -79.645, -79.595),
                      lat=c(9.160, 9.117, 9.058, 9.015),
                      group=c("L", "GW", "OGR", "LC"))
mypoint2 <- data.frame(long=c(-79.846, -79.707, -79.665, -79.610),
                       lat=c(9.181, 9.112, 9.057, 9.014),
                       group=c("BCI", "G", "EH", "MF"))

g1 <- ggplot() +
  geom_blank(data = mymap, aes(x=long, y=lat)) +
  geom_map(data = mymap, map = mymap,
           aes(group = group, map_id = id), color = "darkgrey") +
  # coord_sf(xlim=c(-79.9,-79.8), ylim=c(9.05, 9.2), expand = FALSE) +
  # geom_point(data = mypoint, aes(x = long, y = lat),
  #color = "black", size = 3) +
  # geom_label(data = mypoint, aes(label = group, x = long, y = lat),
  #size = 3, fontface = "bold", nudge_x = c(0.015, 0.02, 0.022, 0.018)) +
  # geom_point(data = mypoint2, aes(x = long, y = lat),
  #color = "blue", size = 3) +
  # geom_label(data = mypoint2, aes(label = group, x = long, y = lat),
  #size = 3, fontface = "bold", nudge_x = c(-0.02, -0.018, -0.02, -0.02)) +
  scale_x_continuous(limits = c(-79.9,-79.8), expand = c(0, 0)) +
  scale_y_continuous(limits = c(9.05, 9.2), expand = c(0, 0)) +
  theme_map() +
  ggsn::scalebar(location = "bottomleft", dist = 1,
                 transform = TRUE, dist_unit = "km", model = 'WGS84',
                 x.min = -79.896, x.max = -79.8,
                 y.min = 9.06, y.max = 9.2) +
  ggsn::north(x.min = -79.89, x.max = -79.807,
              y.min = 9.07, y.max = 9.195,
              location = "topright", scale = 0.1) +
  geom_polygon(color="black",fill="white")+
  theme(axis.text = element_text(),
        axis.ticks = element_line(),
        axis.title = element_text()) +
  xlab("Longitude") +
  ylab("Latitude")

print(g1)
