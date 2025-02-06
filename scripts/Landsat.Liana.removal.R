rm(list = ls())

library(sf)
library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)

my_sf <- read_sf("/home/femeunier/Downloads/GigantePlots/Liana_Plot_Fixed_60m.shp")
plot(my_sf)

corners <- as.data.frame(st_coordinates(my_sf)) %>%
  rename(lon = X,
         lat = Y,
         plot.num = L2) %>%
  dplyr::select(-L1) %>%
  group_by(plot.num) %>%
  slice_head(n = 4)

corners %>%
  group_by(plot.num) %>%
  summarise(lon = mean(lon)) %>%
  View()

ggplot(data = corners) +
  geom_point(aes(x = lon, y = lat, color = as.factor(plot.num))) +
  theme_bw()



write.csv(corners,"~/Downloads/GigantePlots/corners.csv")

A <- st_centroid(my_sf) %>%
  st_geometry()

centroids <- as.data.frame(st_coordinates(A)) %>%
  mutate(ID = 1:n(),
         treatment = my_sf$Type,
         plot.num = my_sf$Plot) %>%
  rename(lon = X,
         lat = Y)
write.csv(centroids,"~/Downloads/Plots.panama.csv")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
ggplot() +
  geom_sf(data = world,fill = NA,color = "grey") +
  geom_sf(data = my_sf,fill = NA,color = "red") +
  geom_point(data = centroids,
             aes(x = lon, y = lat), size = 0.1) +
  scale_x_continuous(limits = c(-79.9,-79.8),expand = c(0,0)) +
  scale_y_continuous(limits = c(9.1,9.15),expand = c(0,0)) +
  theme_bw()

# Johanna magics

data <- as.data.frame(read.csv("/home/femeunier/Downloads/monthlymeanevi_felicien_landsat_masked.csv")) %>%
  pivot_longer(cols = -c(X)) %>%
  mutate(ID = extract_numeric(name)) %>%
  rename(month = X) %>%
  dplyr::select(-name) %>%
  left_join(centroids %>%
              dplyr::select(ID, treatment),
            by = "ID")

data.sum <- data %>%
  group_by(treatment,month) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            value.sd = sd(value,na.rm = TRUE),
            .groups = "keep")

ggplot(data = data) +
  geom_line(aes(x = month,
                y = value, color = treatment, group = ID),size = 0.1) +
  geom_ribbon(data = data.sum,
            aes(x = month,
                y = value.m, ymin = value.m - value.sd, ymax = value.m + value.sd, fill = treatment),
            alpha = 0.5, color = NA) +
  geom_line(data = data.sum,
            aes(x = month,
                y = value.m, color = treatment)) +
  theme_bw()

# Tree case
NIR = 0.4; red = 0.05;
EVI <- 2.5*(NIR - red)/(NIR + 2.4*red + 1)
EVI

# Liana case
NIR = 0.5; red = 0.05;
EVI <- 2.5*(NIR - red)/(NIR + 2.4*red + 1)
EVI

################################################################################

df <- read.csv("~/Downloads/meanevi_2011_2024_felicien_landsat_nonmasked.csv") %>%
  # pivot_longer(cols = -any_of(c("X","ID","plot_num","treatment"))) %>%
  pivot_longer(cols = -any_of(c("X"))) %>%
  mutate(date = as.Date(gsub("\\.","/",substr(name,2,11)))) %>%
  mutate(year = year(date),
         month = month(date)) %>%
  rename(ID = X) %>%
  left_join(centroids %>%
              dplyr::select(ID, treatment),
            by = c("ID"))


df <- read.csv("~/Downloads/meanevi_2011_2024_felicien_landsat_masked_buffer.csv") %>%
  pivot_longer(cols = -any_of(c("X","ID","plot_num","treatment"))) %>%
  mutate(date = as.Date(gsub("\\.","/",substr(name,2,11)))) %>%
  mutate(year = year(date),
         month = month(date)) %>%
  left_join(centroids %>%
              dplyr::select(ID, treatment),
            by = c("ID","treatment"))

df.sum <- df %>%
  group_by(year,month,treatment) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            value.sd = sd(value,na.rm = TRUE),
            .groups = "keep")

ggplot() +
  geom_ribbon(data = df.sum %>%
                filter(year >= 2020),
              aes(x = year + (month-1/2)/12,
                  y = value.m, ymin = value.m - value.sd, ymax = value.m + value.sd, fill = treatment),
              alpha = 0.5, color = NA) +
  geom_line(data = df.sum %>%
              filter(year >= 2020) ,
            aes(x = year + (month-1/2)/12,
                y = value.m, color = treatment)) +
  geom_rect(aes(xmin = 2020:2025,
                xmax = 2020:2025 + (4/12),
                ymin = -Inf, ymax = +Inf), fill = "grey", alpha = 0.25) +
  theme_bw()


df.seasonal.sum <- df %>%
  filter(year >= 2015) %>%
  group_by(month,treatment) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            value.sd = sd(value,na.rm = TRUE),
            .groups = "keep")


ggplot(data = df) +
  geom_ribbon(data = df.seasonal.sum,
              aes(x = month,
                  y = value.m, ymin = value.m - value.sd, ymax = value.m + value.sd, fill = treatment),
              alpha = 0.5, color = NA) +
  geom_line(data = df.seasonal.sum,
            aes(x = month,
                y = value.m, color = treatment)) +
  theme_bw()

df.diff <- df %>%
  mutate(season = case_when(month %in% c(3,4) ~ "end_dry",
                            month %in% c(1,2) ~ "dry",
                            month %in% c(12) ~ "end_wet",
                            TRUE ~ "wet")) %>%
  group_by(treatment,year,season) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            .groups = "keep") %>%
  pivot_wider(names_from = "treatment",
              values_from = "value.m") %>%
  mutate(diff = Control - Removal)

ggplot(data = df.diff %>%
         filter(year >= 2015)) +
  geom_boxplot(aes(x = season, fill = season, y = diff)) +
  theme_bw()

summary(lm(data = df.diff %>%
             filter(year >= 2015,
                    season != "interm"),
           formula = diff ~ season))

ggplot(data = df.diff %>%
         filter(year >= 2015),
       aes(x = year, y = diff,
           color = season, fill = season)) +
  geom_point(data = df.diff,
            aes(x = year, y = diff,
                color = season),
            linetype = 2) +
  geom_line(data = df.diff,
            aes(x = year, y = diff,
                color = season),
            linetype = 2) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  stat_smooth(method = "lm") +
  facet_wrap(~ season) +
  theme_bw()

df.diff %>%
  filter(year >= 2015) %>%
  group_by(season) %>%
  summarise(pval = summary(lm(formula = diff ~ year))[["coefficients"]][2,4],
            r2 = summary(lm(formula = diff ~ year))[["r.squared"]])


