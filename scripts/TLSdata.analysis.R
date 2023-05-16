rm(list = ls())

library(ggplot2)
library(dplyr)
library(sfsmisc)

data.dir <- "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/TLSdata"
datafile <- file.path(data.dir,
                      c("ctrl_19_average_FoliageProfile.csv","rem_19_average_FoliageProfile.csv"))
Treatment <- c("Control","Removal")

df.data <- data.frame()
for (i in seq(1,length(Treatment))){
  data <- read.csv(datafile[i])
  df.data <- bind_rows(list(df.data,
                            data %>% rename(height = X..height,
                                            PAI = weightedPAI) %>% dplyr::select(height,PAI) %>% mutate(treatment = Treatment[i])))
}


ggplot(data = df.data) +
  geom_line(aes(x = PAI, y = height, color = treatment)) +
  theme_bw()

df.data.diff <- df.data %>% group_by(treatment) %>% mutate(iPAI = D1D2(height,PAI)[["D1"]])

ggplot(data = df.data.diff) +
  geom_line(aes(y = iPAI, x = height, color = treatment)) +
  coord_flip() +
  labs(y = "PAI",x = "Height", color = "Treatment") +
  theme_bw() +
  theme(legend.position = c(0.9,0.1),
        text = element_text(size = 20))

ggplot(data = df.data.diff) +
  geom_line(aes(y = iPAI, x = height, color = treatment)) +
  coord_flip() +
  labs(y = "PAI",x = "Height", color = "Treatment") +
  scale_x_continuous(limits = c(30,50)) +
  scale_y_continuous(limits = c(-0.002,0.02)) +
  theme_bw() +
  theme(legend.position = c(0.8,0.8),
        text = element_text(size = 20)) +
  guides(color = FALSE)
