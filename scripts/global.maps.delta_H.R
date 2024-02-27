rm(list = ls())

library(raster)
library(geodata)
library(dplyr)
library(tidyr)
library(rnaturalearth)
library(ggplot2)
library(ggthemes)
library(scales)
library(YGB)
library(reshape2)

days <- c(31,28,31,30,31,30,31,31,30,31,30,31)
tropics <- extent(-150,170,-31,23.15)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

################################################################################
# Avitabile map
Avi <- raster("/home/femeunier/Documents/projects/YGB/data/Avitabile_AGB_Map/Avitabile_AGB_Map.tif")/20

################################################################################
# Climate

r.tavg <- worldclim_global("tavg",
                         path = "./data/WorldClim/", version="2.1",
                         res = 10)

r.tavg.cropped <- crop(r.tavg,extent(Avi))
plot(r.tavg.cropped[[1]])
df.tavg <- as.data.frame(r.tavg.cropped, xy = TRUE) %>%
  pivot_longer(cols = starts_with("wc2.1_10m_tavg_")) %>%
  mutate(month = as.numeric(sub(".*\\_tavg_", "", name))) %>%
  group_by(x,y) %>%
  summarise(MAT = weighted.mean(value,days),
            t.sd = sd(value)/weighted.mean(value,days),
            .groups = "keep")

r.prec <- worldclim_global("prec",
                         path = "./data/WorldClim/", version="2.1",
                         res = 10)
r.prec.cropped <- crop(r.prec,extent(Avi))
plot(r.prec.cropped[[1]])

df.prec <- as.data.frame(r.prec.cropped,xy = TRUE) %>%
  pivot_longer(cols = starts_with("wc2.1_10m_prec_")) %>%
  mutate(month = as.numeric(sub(".*\\_prec_", "", name))) %>%
  group_by(x,y) %>%
  summarise(MAP = sum(value),
            MCWD = -calc.MCWD(value - days*3.33),
            .groups = "keep")

r.MAP <- raster(SpatialPixelsDataFrame(points = df.prec[c("x","y")],
                                       data = df.prec["MAP"]))
r.MCWD <- raster(SpatialPixelsDataFrame(points = df.prec[c("x","y")],
                                        data = df.prec["MCWD"]))

################################################################################
# Avi.repojected <- resample(Avi, raster(r.tavg.cropped), "bilinear")
# writeRaster(Avi.repojected,
#             '/home/femeunier/Documents/projects/YGB/data/Avitabile_AGB_Map/Avitabile_AGB_Map_reproj.tif',options=c('TFW=YES'))

Avi.repojected <- raster("/home/femeunier/Documents/projects/YGB/data/Avitabile_AGB_Map/Avitabile_AGB_Map_reproj.tif")
names(Avi.repojected) <- "Avitabile_AGB_Map"

df.Avi <- as.data.frame(Avi.repojected,xy = TRUE)

df.var <- df.prec %>%
  left_join(df.tavg,
            by = c("x","y")) %>%
  left_join(df.Avi,
            by = c("x","y"))

hist(df.var$Avitabile_AGB_Map)

# plot(raster(SpatialPixelsDataFrame(points = df.var[c("x","y")],
#                                    data = df.var["delta_AGB"])))

lats <- seq(min(df.var$y)-1/6,max(df.var$y)+1/6,1/6) ; lons <- seq(min(df.var$x)-1/6,max(df.var$x)+1/6,1/6)
Gridarea <- RCMIP5:::calcGridArea(lon = lons,
                                  lat = lats) %>%
  melt() %>% mutate(Var1 = lons[Var1],
                    Var2 = lats[Var2]) %>%
  rename(x = Var1,
         y = Var2,
         area = value)


ggplot() +

  geom_tile(data = Gridarea,
            aes(x = x, y = y,fill = log10(area)),
            alpha = 0.7) +
  geom_sf(data = world,
          fill = NA,
          color = "black",
          alpha = 0.5) +
  coord_sf(xlim = c(-120, 170), ylim = c(-30, 23), expand = FALSE) +
  theme_map() +
  guides(size = "none")

df.var.area <- df.var %>%
  mutate(x = round(x,digits = 3),
         y = round(y,digits = 3)) %>%
  left_join(Gridarea %>%
              mutate(x = round(x,digits = 3),
                     y = round(y,digits = 3)),
            by = c("x","y"))

DBHtargets <- c(25,50,100)
Weights <- c(20,30,50)

df.var.area.all <- data.frame()
df.area.loss <- data.frame()

Var1 = "t.sd"; Var2 = "MCWD"
lat.max = 12 ; lat.min = -31

for (iDBHtarget in seq(1,length(DBHtargets))){
  cDBHtarget <- DBHtargets[iDBHtarget]
  print(cDBHtarget)

  best.model <- readRDS(paste0("./outputs/best.model.DeltaH.",cDBHtarget,".RDS"))


  newdata = data.frame(MAT = df.var.area$MAT,
                       t.sd = df.var.area$t.sd,
                       MAP = df.var.area$MAP,
                       MCWD = df.var.area$MCWD)

  df.var.area$delta <- predict(best.model,
                               newdata = newdata)

  df.var.area <- df.var.area %>%
    mutate(delta_bound = case_when(MAT < 17 | MAT > 29 | MCWD > 450 | MCWD < 0 |
                                   t.sd > 0.2 ~ NA_real_,
                                   TRUE ~ delta)) %>%
    mutate(delta_AGB = delta_bound/100*Avitabile_AGB_Map)

  #################################################################################

  df.area.loss <- bind_rows(df.area.loss,
                            df.var.area %>%
    ungroup() %>%
    # filter(y <= lat.max, y >= lat.min) %>%
    mutate(Tot = area*delta_AGB) %>%   # kgC/m²*m² = kgC
    summarise(loss = sum(Tot,na.rm = TRUE)*1000/1e15,   # kgC --> gC --> PgC
              Tot.Biomass = sum(Avitabile_AGB_Map*area,na.rm = TRUE)*1000/1e15) %>%    # Mg
    mutate(target = cDBHtarget))

  df.var.area.all <- bind_rows(df.var.area.all,
                               df.var.area %>%
                                 mutate(target = cDBHtarget))

}

ggplot() +

  geom_tile(data = df.var.area.all %>%
              filter((y) <= lat.max, y >= lat.min),
            aes(x = x, y = y,fill = delta_bound),
            alpha = 1) +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "darkgreen",na.value = "lightgrey",
                       limits = c(-30,1),
                       oob=squish) +
  geom_point(data = readRDS("./outputs/site.loc.RDS"),
             aes(x = lon, y = lat, size = Nlarge),shape = 1) +
  geom_sf(data = world,
          fill = NA,
          color = "black",
          alpha = 0.5) +
  coord_sf(xlim = c(-120, 170), ylim = c(lat.min, lat.max), expand = FALSE) +
  facet_wrap(~ target, ncol = 1) +
  theme_map() +
  guides(size = "none")

ggplot(data = df.var.area.all %>%
         filter((y) <= lat.max, y >= lat.min)) +
  geom_density(aes(x = delta_bound,fill = as.factor(target)),
               alpha = 0.5) +
  theme_bw()


ggplot() +

  geom_tile(data = df.var.area.all %>%
              filter((y) <= lat.max, y >= lat.min),
            aes(x = x, y = y,fill = delta_AGB*20),
            alpha = 1) +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "darkgreen",na.value = "lightgrey",
                       limits = c(-5,0.1)*20,
                       oob=squish) +
  geom_point(data = readRDS("./outputs/site.loc.RDS"),
             aes(x = lon, y = lat, size = Nlarge),shape = 1) +
  geom_sf(data = world,
          fill = NA,
          color = "black",
          alpha = 0.5) +
  coord_sf(xlim = c(-120, 170), ylim = c(lat.min,lat.max), expand = FALSE) +
  facet_wrap(~ target, ncol = 1) +
  theme_map() +
  guides(size = "none")

ggplot(data = df.var.area.all %>%
         filter((y) <= lat.max, y >= lat.min)) +
  geom_density(aes(x = delta_AGB*20,fill = as.factor(target)),
               alpha = 0.5) +
  theme_bw()

################################################################################
# Weighted average

df.link <- data.frame(target = DBHtargets,
                      W = Weights/100)

df.average <- df.var.area.all %>%
  left_join(df.link,
            by = "target") %>%
  filter(y <= lat.max,
         y >= lat.min) %>%
  group_by(x,y) %>%
  summarise(delta_bound_av = sum(delta_bound/100*W),
            delta_AGB = sum(delta_bound/100*Avitabile_AGB_Map*W),
            AGB.av = Avitabile_AGB_Map[1],
            MAP = MAP[1],MCWD = MCWD[1],
            MAT = MAT[1],t.sd = t.sd[1],
            area = area[1],
            .groups = "keep")

ggplot(data = df.var.area.all %>%
         filter(target == 50,
                !is.na(delta_bound))) +
  geom_point(aes(x = t.sd, y = delta_bound)) +
  theme_bw()


ggplot() +

  geom_tile(data = df.average %>%
              filter((y) <= lat.max, y >= lat.min),
            aes(x = x, y = y,fill = delta_AGB*20),
            alpha = 1) +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "darkgreen",
                       na.value = "lightgrey",breaks = c(-50,-25,0),
                       limits = c(-2.5,0.1)*20,
                       oob=squish) +

  geom_sf(data = world,
          fill = NA,
          color = "black",
          alpha = 0.5, size = 0.5) +

  # geom_point(data = readRDS("./outputs/site.loc.RDS"),
  #            aes(x = lon, y = lat, size = Nlarge),shape = 1) +

  coord_sf(xlim = c(-120, 170), ylim = c(lat.min, lat.max), expand = FALSE) +
  theme_void() +
  labs(fill = "") +
  guides(size = "none") +
  theme(text = element_text(size = 14),
        legend.position = "bottom")

ggplot() +

  geom_tile(data = df.var.area.all %>%
              filter((y) <= lat.max, y >= lat.min,
                     target == 50),
            aes(x = x, y = y,fill = delta_bound),
            alpha = 1) +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "darkgreen",
                       na.value = "lightgrey",breaks = c(-50,-25,0),
                       limits = c(-2.5,0.1)*20,
                       oob=squish) +

  geom_sf(data = world,
          fill = NA,
          color = "black",
          alpha = 0.5, size = 0.5) +

  # geom_point(data = readRDS("./outputs/site.loc.RDS"),
  #            aes(x = lon, y = lat, size = Nlarge),shape = 1) +

  coord_sf(xlim = c(-120, 170), ylim = c(lat.min, lat.max), expand = FALSE) +
  theme_void() +
  labs(fill = "") +
  guides(size = "none") +
  theme(text = element_text(size = 14),
        legend.position = "bottom")

hist((df.average$delta_AGB*20))

sum(df.average$delta_AGB*df.average$area,na.rm = TRUE)*1000/1e15 /
  (sum(df.average$AGB.av*df.average$area,na.rm = TRUE)*1000/1e15)
# df.average %>%
#   filter(t.sd <= 0.15,
#          MCWD <= 400) %>%
#   ungroup() %>%
#   summarise(delta_AGB = sum(delta_AGB*area,na.rm = TRUE)*1000/1e15,
#             AGB = sum(AGB.av*area,na.rm = TRUE)*1000/1e15,
#             .groups = "keep") %>%
#   mutate(loss = delta_AGB/AGB)

ggplot() +

  geom_histogram(data = df.average %>%
              filter((y) <= lat.max, y >= lat.min,
                     delta_AGB != 0),
            aes(x = delta_AGB*20)) +
  theme_minimal() +
  scale_x_continuous(limits = c(-50,5)) +
  scale_y_continuous(breaks = seq(0,10000,5000),
                     labels = seq(0,10,5)) +
  labs(x = "",y = "") +
  theme(text = element_text(size = 24),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())


cols<-brewer.pal(3,"Dark2")

ggplot() +

  geom_density(data = df.average %>%
                 mutate(continent = case_when(x <= -35 ~ "America",
                                              x <= 50 ~ "Africa",
                                              TRUE ~ "Australasia")) %>%
                 mutate(continent = factor(continent,
                                           levels = c("America","Africa","Australasia"))) %>%
                   filter((y) <= lat.max, y >= lat.min,
                          delta_AGB != 0),
                 aes(x = delta_AGB*20,
                     fill = continent),alpha = 0.5, color = NA) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
  scale_x_continuous(limits = c(-50,5)) +
  scale_y_continuous(breaks = c(), labels = c("")) +
  labs(x = "",y = "", fill = "") +
  theme(text = element_text(size = 24)) +
  guides(fill = "none")


ggplot() +

  geom_density(data = df.var.area.all %>%
                 mutate(continent = case_when(x <= -35 ~ "America",
                                              x <= 50 ~ "Africa",
                                              TRUE ~ "Australasia")) %>%
                 mutate(continent = factor(continent,
                                           levels = c("America","Africa","Australasia"))) %>%
                 filter((y) <= lat.max, y >= lat.min,
                        target == 50,
                        delta_AGB != 0),
               aes(x = delta_bound,
                   fill = continent),alpha = 0.5, color = NA) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
  scale_x_continuous(limits = c(-50,5)) +
  scale_y_continuous(breaks = c(), labels = c("")) +
  labs(x = "",y = "", fill = "") +
  theme(text = element_text(size = 24)) +
  guides(fill = "none")


ggplot() +

  geom_density(data = df.average %>%
                 mutate(continent = case_when(x <= -35 ~ "America",
                                              x <= 50 ~ "Africa",
                                              TRUE ~ "Australasia")) %>%
                 mutate(continent = factor(continent,
                                           levels = c("America","Africa","Australasia"))) %>%
                 filter((y) <= lat.max, y >= lat.min,
                        delta_AGB != 0),
               aes(x = delta_bound_av*100,
                   fill = continent),alpha = 0.5, color = NA) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
  scale_x_continuous(limits = c(-20,5)) +
  scale_y_continuous(breaks = c(), labels = c("")) +
  labs(x = "",y = "", fill = "") +
  theme(text = element_text(size = 24)) +
  guides(fill = "none")


df.average %>%
  mutate(continent = case_when(x <= -35 ~ "America",
                               x <= 50 ~ "Africa",
                               TRUE ~ "Australasia")) %>%
  group_by(continent) %>%
  summarise(AGBloss = sum(area*delta_AGB*1000/1e15,na.rm = TRUE),
            forest = sum(area*AGB.av*1000/1e15,na.rm = TRUE),
            .groups = "keep") %>%
  mutate(frac = 100*AGBloss/forest)


df.average %>%
  mutate(continent = case_when(x <= -35 ~ "America",
                               x <= 50 ~ "Africa",
                               TRUE ~ "Australasia")) %>%
  group_by(continent) %>%
  filter(delta_AGB != 0) %>%
  summarise( m = 20*mean(delta_AGB,na.rm = TRUE),
            med = 20*median(delta_AGB,na.rm = TRUE),
            min = 20*min(delta_AGB,na.rm = TRUE),
            max = 20*max(delta_AGB,na.rm = TRUE),
            .groups = "keep")

df.var.area.all %>%
  mutate(continent = case_when(x <= -35 ~ "America",
                               x <= 50 ~ "Africa",
                               TRUE ~ "Australasia")) %>%
  mutate(continent = factor(continent,
                            levels = c("America","Africa","Australasia"))) %>%
  filter((y) <= lat.max, y >= lat.min,
         target == 50,
         delta_AGB != 0) %>%
  group_by(continent) %>%
  summarise(m = mean(delta_bound,na.rm = TRUE),
            med = median(delta_bound,na.rm = TRUE),
            Min = min(delta_bound,na.rm = TRUE),
            Max = max(delta_bound,na.rm = TRUE))


################################################################################

df.long <- df.var.area.all %>%
  dplyr::select(x,y,target,delta_bound) %>%
  mutate(target = paste0("DBH_",target)) %>%
  pivot_wider(names_from = target,
              values_from = delta_bound) %>%
  pivot_longer(cols = paste0("DBH_",DBHtargets[DBHtargets!=50]),
               names_to = "target",
               values_to = "delta_bound")

ggplot(data = df.long %>%
         filter((y) <= lat.max, y >= lat.min),
       aes(x = DBH_50, y = delta_bound, color = target, fill = target)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0,linetype = 2, color = "black") +
  theme_bw()

df.long.AGB <- df.var.area.all %>%
  dplyr::select(x,y,target,delta_AGB) %>%
  mutate(target = paste0("DBH_",target)) %>%
  pivot_wider(names_from = target,
              values_from = delta_AGB) %>%
  pivot_longer(cols = paste0("DBH_",DBHtargets[DBHtargets!=50]),
               names_to = "target",
               values_to = "delta_AGB")

ggplot(data = df.long.AGB %>%
         filter((y) <= lat.max, y >= lat.min),
       aes(x = DBH_50, y = delta_AGB*20, color = target, fill = target)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0,linetype = 2, color = "black") +
  theme_bw()


