rm(list = ls())

library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)

plots <- read.csv("~/Downloads/Plots.panama.csv") %>%
  dplyr::select(-X)

A <- read.csv("/home/femeunier/Downloads/NDVI_month_Panama_2016_2024_psscene_analytic_sr_udm2_corners.csv")
A.long <- A %>%
  pivot_longer(cols = -c(X,plot.num)) %>%
  rename(ID = plot.num) %>%
  left_join(plots,
            by = "ID") %>%
  mutate(time = as.Date(sub("\\.","/",(sub("\\.","/",str_sub(name,2,11)))))) %>%
  mutate(year = year(time),
         month = month(time)) %>%
  mutate(rep = case_when(plot.num %in% c(1,2) ~ 1,
                         plot.num %in% c(5,16) ~ 2,
                         plot.num %in% c(3,4) ~ 3,
                         plot.num %in% c(13,14) ~ 4,
                         plot.num %in% c(11,12) ~ 5,
                         plot.num %in% c(9,10) ~ 6,
                         plot.num %in% c(6,7) ~ 7,
                         plot.num %in% c(8,15) ~ 8)) %>%
  mutate(season = case_when(month %in% c(1:4) ~ "dry",
                            month %in% c(6:11) ~ "wet",
                            TRUE ~ "intermediary"))


A.long.sum <- A.long %>%
  group_by(treatment,year,month,season) %>%
  summarise(m = mean(value,na.rm = TRUE),
            .groups = "keep")

ggplot() +
  geom_rect(aes(xmin = c(0/12 + (2017:2024)),
                xmax = c(5/12 + (2017:2024)),
                ymin = -Inf, ymax = Inf),
            alpha = 0.5, fill = "grey",color = NA) +
  geom_line(data = A.long.sum,
            aes(x = year + (month - 1/2)/12,y = m,
                color = treatment)) +
  # stat_smooth(data = A.long.sum,
  #             aes(x = year + (month - 1/2)/12,y = m,
  #                 color = treatment, fill = treatment),
  #             method = "lm") +
  theme_bw()

ggplot() +
  geom_line(data = A.long.sum,
            aes(x = year + (month - 1/2)/12,y = m,
                color = treatment)) +
  stat_smooth(data = A.long.sum,
              aes(x = year + (month - 1/2)/12,y = m,
                  color = treatment, fill = treatment),
              method = "lm") +
  facet_wrap(~ season) +
  theme_bw()

A.seasonal <- A.long %>%
  group_by(month,treatment,plot.num) %>%
  summarise(m = mean(value,na.rm = TRUE),
            .groups = "keep")

A.seasonal.sum <- A.seasonal %>%
  group_by(treatment,month) %>%
  summarise(m.av = mean(m,na.rm = TRUE),
            m.sd = sd(m,na.rm = TRUE),
            m.se = 1.96*sd(m,na.rm = TRUE)/sqrt(8),
            .groups = "keep")

ggplot(data = A.seasonal.sum,
       aes(x = month,y = m.av,
           color = treatment, fill = treatment)) +
  geom_line() +
  geom_ribbon(aes(ymin = m.av-m.sd, ymax = m.av + m.sd), color = NA,
              alpha = 0.5) +
  theme_bw()

A.long.wide <- A.long %>%
  dplyr::select(-c(plot.num,X,ID,lon,lat)) %>%
  pivot_wider(names_from = treatment,
              values_from = value) %>%
  mutate(diff = Control - Removal) %>%
  mutate(season = case_when(month %in% c(1:4) ~ "dry",
                            month %in% c(6:11) ~ "wet",
                            TRUE ~ "intermediary"))

summary(aov(data = A.long,
            formula = value ~ treatment))

ggplot(data = A.long.wide) +
  geom_boxplot(aes(x = season, fill = season,y = diff)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw() +
  guides(fill = "none")

A.diff <- A.long.sum %>%
  pivot_wider(names_from = "treatment",
              values_from = "m") %>%
  mutate(diff = Control - Removal) %>%
  mutate(season = case_when(month %in% c(1:4) ~ "dry",
                            month %in% c(6:11) ~ "wet",
                            TRUE ~ "intermediary"))

summary(aov(data = A.long.sum %>%
              mutate(season = case_when(month %in% c(1:4) ~ "dry",
                                        month %in% c(6:11) ~ "wet",
                                        TRUE ~ "intermediary")),
            formula = m ~ treatment*season))

ggplot(data = A.diff) +
  geom_boxplot(aes(x = season, fill = season,y = diff)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw() +
  guides(fill = "none")

ggplot(data = A.diff,
       aes(x = year + (month - 1/2)/12,
           y = diff)) +
  geom_line() +
  stat_smooth(method = "lm") +
  facet_wrap(~ season) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw() +
  guides(fill = "none")
