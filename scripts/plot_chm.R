rm(list = ls())

library(lidR)
library(LidarProcessoR)
library(dplyr)
library(rgl)
library(LianaBCI)
library(ggplot2)


df.chm <- readRDS("./outputs/df.chm.RDS")
df.SP.true <- readRDS("./outputs/df.SP.RDS")

ggplot(data = df.chm %>% filter(resolution == 0.5),
       aes(x = X.rel, y = Y.rel,
           fill = Z)) +
  geom_tile() +
  geom_point(data = df.SP.true, color = "red", size = 1) +
  facet_grid(year ~ plot) +
  coord_fixed()
