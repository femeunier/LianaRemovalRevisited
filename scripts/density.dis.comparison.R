rm(list = ls())

library(dplyr)

liana.file <- "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/Liana.data.csv"
liana.data <- read.csv(liana.file)

unique(liana.data$Parcela)

liana.data %>%
  group_by(Parcela) %>%
  summarise(n())

liana.data.filt <- liana.data %>%
  filter(Parcela %in% c(1,4,6,8,10,12,13,16))

unique(liana.data.filt$Fecha)

hist(as.numeric(liana.data.filt$Diam.2011))
summary(liana.data.filt$Diam.2011/10)

liana.plot <- liana.data.filt %>%
  group_by(Parcela) %>%
  summarise(density = n()/(60*60)*10000,   # Stems/ha
            BA = pi*sum((Diam.2011/10)**2,na.rm = TRUE)/(4*60*60),   # cm2/m2
            .groups = "keep")

liana.site <- liana.plot %>%
  ungroup() %>%
  summarise(density = mean(density),
            BA = mean(BA))

liana.site
# Compared to nutrient plots: 1.6cm2/m2 and density of around 2000 stems/ha

# BCI, Local canopy disturbance as an explanation for long-term increases in liana abundance
86723/50 # Stems/ha
55.13/50 # cm2/m2
