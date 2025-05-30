rm(list = ls())

library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(geodata)
library(raster)
library(tidyr)

srad <- worldclim_global("srad",
                           path = "./data/WorldClim/", version="2.1",res = 10)


Panama <- extent(c(-80,-79,9,10))
Tropics <- extent(c(-180,180,-23.25,23.25))
CoreTropics <- extent(c(-180,180,-5,5))

df <- data.frame() ; df.lat <- data.frame()
for (i in seq(1,12)){
  crast <- srad[[i]]

  df <- bind_rows(df,
                  data.frame(Panama = mean(as.vector(crop(crast,Panama)),
                           na.rm = TRUE),
             Tropics = mean(as.vector(crop(crast,Tropics)),
                           na.rm = TRUE),
             CoreTropics = mean(as.vector(crop(crast,CoreTropics)),
                            na.rm = TRUE),
             month = i))

  cvar <- paste0("wc2.1_10m_srad_",sprintf("%02d",i))

  df.lat <- bind_rows(df.lat,
                      as.data.frame(crast,
                xy = TRUE) %>%
    mutate(lat = round(y)) %>%
    rename(value := cvar) %>%
    group_by(lat) %>%
    summarise(value.m = mean(value,na.rm = TRUE)) %>%
      mutate(month = i))

}

df.lat.m <- df.lat %>%
  group_by(lat) %>%
  summarise(value.m = mean(value.m),
            .groups = "keep")

ggplot() +
  geom_line(data = df.lat,
            aes(y = value.m, x = lat, group = month,
                color = (month))) +
  geom_line(data = df.lat.m,
            aes(y = value.m, x = lat), color = "red") +
  scale_x_continuous(limits = c(-1,1)*23.25) +
  coord_flip() +
  theme_bw()

df.long <- df %>%
  pivot_longer(cols = -c(month),
               names_to = "location",
               values_to = "value")

df.long.sum <-
  df.long %>%
  group_by(location) %>%
  summarise(value.m = mean(value),
            .groups = "keep")

ggplot() +
  geom_line(data = df.long,
            aes(x = month,
                y = value,
                color = location)) +
  geom_hline(data = df.long.sum,
             aes(yintercept = value.m,
                 color = location),
             linetype = 2) +
  theme_bw()

A <- readRDS("/home/femeunier/Documents/data/monthly.climate.pantropical.ERA5.RDS")

A.lat <- A %>%
  mutate(lat = round(lat)) %>%
  group_by(lat,month) %>%
  summarise(dswrf.m = mean(dswrf),
             .groups = "keep")

A.lat.m <- A.lat %>%
  group_by(lat) %>%
  summarise(dswrf.m = mean(dswrf.m,na.rm = TRUE),
            .groups = "keep")

ggplot() +
  geom_line(data = A.lat,
            aes(y = dswrf.m, x = lat, group = month,color = (month))) +
  geom_line(data = A.lat.m,
            aes(y = dswrf.m, x = lat),
            color = "red") +
  coord_flip() +
  theme_bw()

A.sum <- A %>%
  filter(year %in% c(1981:2010)) %>%
  group_by(lon,lat,month) %>%
  summarise(dswrf.m = mean(dswrf,na.rm = TRUE),
            .groups = "keep")

Panama <- extent(c(-80,-79,9,10))
Tropics <- extent(c(-180,180,-23.25,23.25))
CoreTropics <- extent(c(-180,180,-5,5))

df.wide <- bind_rows(A.sum %>%
                       filter(lon >= -80, lon <= -79,lat <= 10, lat >= 9) %>%
                       group_by(month) %>%
                       summarise(value.m = mean(dswrf.m,na.rm = TRUE),
                                 .groups = "keep") %>%
                       mutate(location = "Panama"),
                     A.sum %>%
                       filter(lon >= -180, lon <=180,lat <= 23.25, lat >= -23.25) %>%
                       group_by(month) %>%
                       summarise(value.m = mean(dswrf.m,na.rm = TRUE),
                                 .groups = "keep")%>%
                       mutate(location = "Tropics"),
                     A.sum %>%
                       filter(lon >= -180, lon <=180,lat <= 5, lat >= -5) %>%
                       group_by(month) %>%
                       summarise(value.m = mean(dswrf.m,na.rm = TRUE),
                                 .groups = "keep")%>%
                       mutate(location = "CoreTropics"))


ggplot() +
  geom_line(data = df.wide,
            aes(x = month,
                y = value.m,
                color = location)) +
  theme_bw()
