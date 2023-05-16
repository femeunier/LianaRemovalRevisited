rm(list = ls())

# Meta-data
# Add other meta-data

library(dplyr)
library(ggplot2)
library(ggrepel)
library(raster)

sites.Congo <- read.csv("./data/metadata_Congo.csv") %>%
  rename(lat = Lat,
         lon = Lon)

# 2+27/60
# 17+02/60

site.Gigante <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/location_plots.csv") %>%
  summarise(lat = mean(Y),
            lon = mean(X)) %>%
  mutate(Site = "Gigante-Us")

site.Gigante2 <- data.frame(lat = 9+6/60+31/3600,
                            lon = -79-50/60-37/3600,
                            Site = "Gigante-Joe")

site.BCI <- data.frame(lat = (9.156129 + 9.152909)/2,
                       lon = (-79.852753 -79.846058)/2,
                       Site = "BCI")

sites.Australia <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/COI_metadata_FoRCE_Australia.csv",
                           stringsAsFactors = FALSE) %>%
  summarise(lat = -mean(centre_s),
            lon = mean(centre_e)) %>%
  mutate(Site = "Australia")

all.df.md <- bind_rows(list(
  site.BCI %>% mutate(site.common = "BCNM"),
  sites.Congo %>% mutate(site.common = Site),
  site.Gigante %>% mutate(site.common = "BCNM"),
  site.Gigante2 %>% mutate(site.common = "BCNM"),
  sites.Australia %>% mutate(site.common = "Australia"))) %>%
  left_join(readRDS("./outputs/All.COI.data.RDS") %>%
              group_by(site) %>%
              dplyr::summarise(N = length(dbh),
                        .groups = "keep") %>%
              rename(Site = site) %>% ungroup(),
            by = "Site") %>%
  group_by(site.common) %>%
  summarise(N = sum(N),
            lat = mean(lat),
            lon = mean(lon),
            .groups = "keep")


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


Australia.md.ds <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/COI_metadata_FoRCE_Australia.csv",
                            stringsAsFactors = FALSE)

ggplot() +
  geom_sf(data = world,
          fill = NA,
          color = "darkgrey") +
  geom_point(aes(x = centre_e, y = -centre_s),
             data = Australia.md.ds, shape = 1) +
  scale_y_continuous(limits = c(-1,-0.5)*23.5) +
  scale_x_continuous(limits = c(140,150)) +
  scale_size_continuous(range = c(0.1, 2)) +
  theme_bw()

all.df.md2plot <- all.df.md %>%
  filter(!is.na(N))

# load PFT map
r <- raster("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1_aggr_reclassified.tif")
df.r <- as.data.frame(r,xy = TRUE) %>%
  rename(lon =  x,
         lat = y,
         LU = layer) %>%
  filter(!is.na(LU))

ggplot() +
  geom_raster(data = df.r,
              aes(x = lon, y = lat, fill = as.factor(LU)),
              alpha = 0.5,show.legend = FALSE) +
  geom_sf(data = world,
          fill = NA,
          color = "black",
          alpha = 0.5) +
  geom_point(aes(x = lon, y = lat,size = sqrt(N)),
             data = all.df.md2plot, shape = 1) +
  geom_label_repel(aes(x = lon, y = lat, label = site.common),
                   size=2,
                   data = all.df.md2plot,
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(1, "lines"),
                   segment.color = 'grey50') +
  scale_fill_manual(values = c("white",c("#72a83d"),"darkgreen")) +
  scale_y_continuous(limits = c(-1,1)*23.5) +
  scale_x_continuous(limits = c(-100,150)) +
  scale_size_continuous(range = c(0.1, 2)) +
  labs(x = "", y = "") +
  theme_bw() +
  guides(size = "none")

sum(all.df.md2plot$N)
nrow(all.df.md2plot)
