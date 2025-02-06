rm(list = ls())

# Meta-data
# Add other meta-data

library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyr)
library(raster)
library(readxl)
library(ggthemes)
library(stringr)

# Forestplot Asia

FP.Asia <- readRDS("./data/Asia/Plot.locations.RDS") %>%
  rename(lat = Latitude,
         lon = Longitude) %>%
  group_by(site.common) %>%
  summarise(lat = mean(lat),
            lon = mean(lon),
            .groups = "keep") %>%
  mutate(Site = site.common)

sites.Erika <- data.frame(Site = c("129","357"),
                          lat = c(-2.71,-3.28),
                          lon = c(-54.75,-54.85))

sites.Congo <- read.csv("./data/metadata_Congo.csv") %>%
  rename(lat = Lat,
         lon = Lon) %>%
  filter(Site != "Semi-F") %>%
  mutate(Site = case_when(Site == "Atla-F" ~ "Luki",
                          Site == "Sand-F" ~ "Mokabi"))

site.tapajos = data.frame(
  lat = -2.755150,
  lon = -54.836777,
  Site = "Tapajos"
)

site.Gigante <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/location_plots.csv") %>%
  summarise(lat = mean(Y),
            lon = mean(X)) %>%
  mutate(Site = "Gigante-Us")

site.Alain <- read.csv("./data/Tree_COI_Data_Cameroon_MissingHeights.csv") %>%
  dplyr::select(plotID, plotLat ,plotLon ) %>%
  mutate(Site = sub("\\_.*", "", plotID)) %>%
  filter(Site == "OKU") %>%
  group_by(Site) %>%
  summarise(lat = mean(plotLat),
            lon = mean(plotLon))


site.Panama <- readRDS("./outputs/plots.panama.RDS") %>%
  group_by(plot.group) %>%
  summarise(lat = mean(lat),
            lon = mean(lon)) %>%
  rename(Site = plot.group) %>%
  filter(Site != "BCI")

site.Tan <- read.csv("./data/TanzaniaData_FoRCE_COI.csv") %>%
  mutate(site = substr(plotID,1,4)) %>%
  dplyr::select(c(site,forest,latitude,longitude)) %>%
  distinct() %>%
  # group_by(site) %>%
  summarise(lat = mean(latitude),
            lon = mean(longitude)) %>%
  mutate(Site = "Tanzania")

site.Gigante2 <- data.frame(lat = 9+6/60+31/3600,
                            lon = -79-50/60-37/3600,
                            Site = "Gigante")

site.Afritron <- readRDS( "./data/Afritron/Afritron.metadata.RDS") %>%
  rename(Site = site)

site.BCI <- data.frame(lat = (9.156129 + 9.152909)/2,
                       lon = (-79.852753 -79.846058)/2,
                       Site = "BCI")

sites.Australia <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/COI_metadata_FoRCE_Australia.csv",
                           stringsAsFactors = FALSE) %>%
  summarise(lat = -mean(centre_s),
            lon = mean(centre_e)) %>%
  mutate(Site = "Australia")

site.begum <- data.frame(lat = 2+27/60 + 11.87/3600,
                         lon = 17 +2/60 + 32.17/3600,
                         Site = "Loundoungou")

site.Jocker <- data.frame(lat = -29-40/60-23/3600,
                          lon = -(53+37/60+56/3600),
                          Site = "Rio Grande")

site.Arildo <- data.frame(lat = mean(c(-22-45/60, -23)),
                          lon = -mean(c(47,47+12/60)),
                          Site = "Sao Paulo")

# Karin
raw.data.Karin <- read_xlsx("./data/Karin/Karin data_Felicien.xlsx")

coords <- raw.data.Karin %>%
  dplyr::select(Coordinates) %>%
  mutate(lat.string = sub("\"S .*", "", Coordinates),
         lon.string = str_sub(str_match(Coordinates, " 4\\s*(.*?)\\s*W")[,1],
                              end = -3)) %>%
  mutate(lat =   as.numeric(substr(lat.string,1,2)) +
           as.numeric(substr(lat.string,4,5))/60 +
           as.numeric(sub(".*’", "", lat.string))/3600,
         lon =   as.numeric(substr(lon.string,2,3)) +
           as.numeric(substr(lon.string,5,6))/60 +
           as.numeric(sub(".*’", "", lon.string))/3600)

site.Karin <- coords %>%
  summarise(lat = -mean(lat),
            lon = -mean(lon)) %>%
  mutate(Site = "Campinas")

site.pasoh <- data.frame(lat = 2 + 58/60,
                         lon = 102+18/60,
                         Site = "Pasoh")

site.DV <- data.frame(lon = 117.7943,
                      lat = 4.9561,
                      Site = "Danum Valley")

# Rainfor request 1
site.rainfor <- readRDS("./data/ForestPlots/data/RainForMD.RDS") %>%
  rename(Site = group) %>%
  rename(lat = lat.m,
         lon = lon.m) %>%
  filter(!(Site %in% c("GAU","SAT","VCR","FRP","POA"))) # repeated with the second census

# Rainfor request 2
site.rainfor2 <- readRDS("./data/rainfor2.md.RDS") %>%
  rename(Site = group) %>%
  rename(lat = Latitude,
         lon = Longitude) %>%
  dplyr::select(-N)

all.COI <- readRDS("./outputs/All.COI.data.RDS")

all.sites <-  bind_rows(list(
  bind_rows(
    site.Alain %>% mutate(site.common = Site),
    site.Panama %>% mutate(site.common = Site),
    site.Tan %>% mutate(site.common = Site),
    # site.tapajos %>% mutate(site.common = Site),
    site.begum %>% mutate(site.common = Site),
    site.pasoh %>% mutate(site.common = Site),
    site.Arildo %>% mutate(site.common = Site),
    sites.Erika %>% mutate(site.common = Site),
    FP.Asia,
    site.BCI %>% mutate(site.common = Site),
    site.Afritron %>% mutate(site.common = Site),
    site.Karin %>% mutate(site.common = Site),
    sites.Congo %>% mutate(site.common = Site),
    # site.DV %>% mutate(site.common = Site),
    site.rainfor %>% mutate(site.common = Site),
    site.rainfor2 %>% mutate(site.common = Site),
    # site.Gigante %>% mutate(site.common = "BCNM"),
    site.Gigante2 %>% mutate(site.common = Site),
    site.Jocker %>% mutate(site.common = Site),
    sites.Australia %>% mutate(site.common = Site)))) %>%

  left_join(all.COI %>%
              group_by(site) %>%
              dplyr::summarise(N = length(dbh),
                               dbh.max = max(dbh,na.rm = TRUE),
                               Nlarge = length(dbh[dbh >= 10]),
                               .groups = "keep") %>%
              rename(Site = site) %>% ungroup(),
            by = "Site") %>%
  dplyr::select(-Country)



all.df.md <- all.sites %>%
  group_by(site.common) %>%
  summarise(N = sum(N),
            Nlarge = sum(Nlarge),
            dbh.max = max(dbh.max),
            lat = mean(lat),
            lon = mean(lon),
            .groups = "keep")


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


Australia.md.ds <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/COI_metadata_FoRCE_Australia.csv",
                            stringsAsFactors = FALSE)


# ggplot() +
#   geom_sf(data = world,
#           fill = NA,
#           color = "darkgrey") +
#   geom_point(aes(x = centre_e, y = -centre_s),
#              data = Australia.md.ds, shape = 1) +
#   scale_y_continuous(limits = c(-1,-0.5)*23.5) +
#   scale_x_continuous(limits = c(-10,45)) +
#   scale_size_continuous(range = c(0.1, 2)) +
#   theme_bw()

all.df.md2plot <- all.df.md %>%
  filter(!is.na(N))

# load PFT map
r <- raster("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1_pantropical_aggr.tif")
df.r <- as.data.frame(r,xy = TRUE) %>%
  rename(lon =  x,
         lat = y,
         LU = C3S.LC.L4.LCCS.Map.300m.P1Y.2020.v2.1.1_pantropical_aggr) %>%
  filter(!is.na(LU))

ggplot() +
  geom_raster(data = df.r,
              aes(x = lon, y = lat, fill = as.factor(LU)),
              alpha = 0.4,show.legend = FALSE) +
  geom_sf(data = world,
          fill = NA,
          color = "black",
          alpha = 0.5) +
  # geom_point(aes(x = lon, y = lat,size = sqrt(N)),
  #            data = all.df.md2plot, shape = 1) +
  geom_point(aes(x = lon, y = lat),
             data = all.df.md2plot, shape = 1,
             alpha = 1,
             size = 1, color = "red") +
  # geom_point(aes(x = lon, y = lat),
  #            data = all.df.md2plot %>%
  #              dplyr::filter(site.common %in% c("BCI")), shape = 1,
  #            alpha = 1, color = "red",
  #            size = 1.5) +
  scale_fill_manual(values = c("white",c("#72a83d"),"darkgreen")) +
  scale_y_continuous(limits = c(-30,10)) +
  scale_x_continuous(limits = c(-85,160),expand = c(0,0)) +
  scale_size_continuous(range = c(0.1, 2)) +
  labs(x = "", y = "") +
  theme_map() +
  guides(size = "none") +
  theme(text = element_text(size = 20))


# ggplot() +
#   geom_raster(data = df.r,
#               aes(x = lon, y = lat, fill = as.factor(LU)),
#               alpha = 0.3,show.legend = FALSE) +
#   geom_sf(data = world,
#           fill = NA,
#           color = "black",
#           alpha = 0.5) +
#   # geom_point(aes(x = lon, y = lat,size = sqrt(N)),
#   #            data = all.df.md2plot, shape = 1) +
#   geom_point(aes(x = lon, y = lat),
#              data = all.df.md2plot, shape = 16, size = 2) +
#   # geom_point(aes(x = lon, y = lat),
#   #            data = all.df.md2plot %>% filter(site.common == "OKU"),
#   #            color = "red",shape = 16) +
#   # geom_label_repel(aes(x = lon, y = lat, label = site.common),
#   #                  size=5,
#   #                  data = all.df.md2plot,
#   #                  segment.color = 'grey50') +
#   scale_fill_manual(values = c("white",c("#72a83d"),"darkgreen")) +
#   scale_y_continuous(limits = c(7,10)) +
#   scale_x_continuous(limits = c(-85,-75),expand = c(0,0)) +
#   scale_size_continuous(range = c(0.1, 2)) +
#   labs(x = "", y = "") +
#   theme_map() +
#   guides(size = "none") +
#   theme(text = element_text(size = 20))



saveRDS(all.df.md2plot,
        "./outputs/site.loc.RDS")


# Amazon <- ggplot() +
#   geom_raster(data = df.r,
#               aes(x = lon, y = lat, fill = as.factor(LU)),
#               alpha = 0.3,show.legend = FALSE) +
#   geom_sf(data = world,
#           fill = NA,
#           color = "black",
#           alpha = 0.5) +
#   # geom_point(aes(x = lon, y = lat,size = sqrt(N)),
#   #            data = all.df.md2plot, shape = 1) +
#   geom_point(aes(x = lon, y = lat),
#              data = all.df.md2plot, shape = 16) +
#   # geom_point(aes(x = lon, y = lat),
#   #            data = all.df.md2plot %>% filter(site.common == "OKU"),
#   #            color = "red",shape = 16) +
#   # geom_label_repel(aes(x = lon, y = lat, label = site.common),
#   #                  size=5,
#   #                  data = all.df.md2plot,
#   #                  segment.color = 'grey50') +
#   scale_fill_manual(values = c("white",c("#72a83d"),"darkgreen")) +
#   scale_y_continuous(limits = c(-30,10)) +
#   scale_x_continuous(limits = c(-85,-35),expand = c(0,0)) +
#   scale_size_continuous(range = c(0.1, 2)) +
#   labs(x = "", y = "") +
#   theme_bw() +
#   guides(size = "none") +
#   theme(text = element_text(size = 20))
#
# Panama <- ggplot() +
#   geom_raster(data = df.r,
#               aes(x = lon, y = lat, fill = as.factor(LU)),
#               alpha = 0.3,show.legend = FALSE) +
#   geom_sf(data = world,
#           fill = NA,
#           color = "black",
#           alpha = 0.5) +
#   # geom_point(aes(x = lon, y = lat,size = sqrt(N)),
#   #            data = all.df.md2plot, shape = 1) +
#   geom_point(aes(x = lon, y = lat),
#              data = all.df.md2plot, shape = 16) +
#   # geom_point(aes(x = lon, y = lat),
#   #            data = all.df.md2plot %>% filter(site.common == "OKU"),
#   #            color = "red",shape = 16) +
#   # geom_label_repel(aes(x = lon, y = lat, label = site.common),
#   #                  size=5,
#   #                  data = all.df.md2plot,
#   #                  segment.color = 'grey50') +
#   scale_fill_manual(values = c("white",c("#72a83d"),"darkgreen")) +
#   scale_y_continuous(limits = c(5,10)) +
#   scale_x_continuous(limits = c(-85,-75),expand = c(0,0)) +
#   scale_size_continuous(range = c(0.1, 2)) +
#   labs(x = "", y = "") +
#   theme_bw() +
#   guides(size = "none") +
#   theme(text = element_text(size = 20))

saveRDS(all.df.md2plot,
        "./outputs/all.df.md2plot.RDS")

levels.ordered <- all.sites %>%
  arrange(lon) %>%
  pull(Site)

summary.num <- readRDS("./outputs/All.COI.data.RDS") %>%
  group_by(site,liana.cat) %>%
  dplyr::summarise(N = length(dbh),
                   Nlarge = length(dbh[dbh >= 10]),
                   .groups = "keep") %>%
  rename(Site = site) %>%
  mutate(liana.cat = as.character(liana.cat)) %>%
  ungroup() %>%
  complete(liana.cat = c("no","low","high"),
           Site = unique(all.sites$Site),
           fill = list(N = NA,
                       Nlarge = NA)) %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high"))) %>%
  mutate(Site = factor(Site,level = levels.ordered))




ggplot(data = summary.num) +
  geom_bar(aes(x = Site, y = Nlarge, fill = liana.cat),
           stat = "identity",
           position = position_dodge(), alpha = 1) +
  # scale_y_log10() +
  labs(x = "", y = "Number of trees",
       fill = "Liana Infestation") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(size = 20),
        legend.position = c(0.15,0.85)) +
  scale_fill_brewer(palette = "Greens") +
  scale_fill_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred")) +
  guides(fill = FALSE)

length(unique(summary.num$Site))

bind_rows(summary.num %>%
  group_by(liana.cat) %>%
  summarise(N = sum(N,na.rm = TRUE),
            Nlarge = sum(Nlarge,na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(fraction = N/sum(N)*100,
         faction.large = Nlarge/sum(Nlarge)*100),
  summary.num %>%
    summarise(N = sum(N,na.rm = TRUE),
              Nlarge = sum(Nlarge,na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(liana.cat = "Total",
           fraction = N/sum(N)*100,
           faction.large = Nlarge/sum(Nlarge)*100))

summary.num %>%
  group_by(Site) %>%
  summarise(N = sum(N,na.rm = TRUE),
            Nlarge = sum(Nlarge,na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(liana.cat = "Total",
         fraction = N/sum(N)*100,
         faction.large = Nlarge/sum(Nlarge)*100)

frac <- summary.num %>%
  group_by(Site) %>%
  mutate(fraction = Nlarge/sum(Nlarge),
         liana.cat = factor(liana.cat,
                            levels = c("no","low","high")))

alpha = 0.11

f <- function(x) {
  r <- quantile(x, probs = c(alpha/2, 0.25, 0.5, 0.75, 1 - alpha/2))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

ggplot(data = frac,
       aes(x = liana.cat,
           y = fraction*100,
           fill = liana.cat)) +

  stat_summary(fun.data = f,
               geom="boxplot",
               width = .15,
               alpha = 0.5,
               outlier.shape = NA) +

  ggdist::stat_halfeye(
    adjust = .5,
    width = .6,
    .width = 0,
    justification = -0.2, alpha = 0.5,
    point_colour = NA
  ) +
  scale_fill_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred")) +
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  scale_y_continuous(limits = c(0,100)) +
  # scale_y_log10(limits = c(0.1,2)) +
  labs(x = "", y = "") +
  scale_x_discrete(labels = c("","","")) +
  theme_bw() +
  guides(fill = "none", color = "none") +
  theme(text = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))



frac %>%
  group_by(liana.cat) %>%
  summarise(min = min(fraction)*100,
            site.min = Site[which.min(fraction)],
            med = median(fraction)*100,
            mean = mean(fraction)*100,
            max = max(fraction)*100,
            site.max = Site[which.max(fraction)])

# frac %>%
#   filter(fraction > 0.2,
#          liana.cat == "high")
