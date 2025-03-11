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

df <- data.frame()
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

}

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
