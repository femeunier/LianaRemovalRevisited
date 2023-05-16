rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)

data.file <- file.path("./data/Arboles 10 cm x-y data for Felicien 2014.csv")
data <- read.csv(data.file) %>%
  filter(Treatment %in% c("C","R")) %>%
  mutate(Cuadrante.mod = sprintf("%04d",Cuadrante)) %>%
  mutate(Delta_x = as.numeric(stringr::str_sub(Cuadrante.mod,3,4)),
         Delta_y = as.numeric(stringr::str_sub(Cuadrante.mod,1,2))) %>%
  mutate(actualX = X + Delta_x,
         actualY = Y + Delta_y)

unique(data$Cuadrante)


N.colored.species <- 10
most.abundant.species <- sort(table(data$Cod..Especies),decreasing = TRUE)
most.abundant.species.select <- most.abundant.species[1:N.colored.species]

df.BA <- data %>%
  group_by(Parcela,Cod..Especies) %>%
  summarise(BA = sum(DBH*DBH,na.rm = TRUE),
            .groups = "keep") %>%
  group_by(Cod..Especies) %>%
  summarise(BA.m = mean(BA),
            .groups = "keep") %>%
  ungroup() %>%
  arrange(desc(BA.m)) %>%
  slice_head(n = N.colored.species)

data.mod <- data %>%
  group_by(Treatment) %>%
  mutate(plot.rel = as.numeric(match(Parcela,unique(Parcela)))) %>%
  ungroup() %>%
  mutate(color.code = case_when(Cod..Especies %in% df.BA$Cod..Especies ~ as.character(Cod..Especies),
                                TRUE ~ "OTHER"))


ggplot(data.mod) +
  geom_point(aes(x = actualX, y = actualY,
                 size = DBH/10, color = as.factor(color.code))) +
  facet_grid(Treatment ~ plot.rel) +
  theme_bw()


library(LVLRE.long)

df.temp <- Tree.data %>%
  dplyr::filter(timing == max(timing))

summary(df.temp$DBH/10)

data %>% slice_head(n = 1)

df.temp %>% filter(ID == 14)
