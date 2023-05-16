rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(LianaRemovalRevisited)

plots2consider = c(8,16,12,13,5,7,14,11)
plots2consider = seq(1,16)

## Hypothesis 1
# Default allometry
Tree.data.H <- Tree.data %>% ungroup() %>% filter(plot %in% plots2consider) %>%
  filter(!is.na(DBH)) %>%
  mutate(DBH = DBH/10) %>%
  mutate(H = 51.38*(1-exp(-0.01322*(DBH*10)**0.6465)),
         AGB = 0.0509*WD*DBH**2*H) %>%
  group_by(timing,Time2,Treatment,plot) %>%
  summarise(Hmean = mean(H),
            Hmax = max(H),
            AGB = sum(AGB,na.rm = TRUE)/(60*60),
            DBHmean = mean(DBH),
            DBHmax = max(DBH),
            .groups = "keep")

ggplot(data = Tree.data.H) +
  geom_line(aes(x = timing, y = Hmean, color = Treatment,group = interaction(Treatment,plot))) +
  theme_bw()



Tree.data.H.sum <- Tree.data.H %>% group_by(timing,Time2,Treatment) %>%
  summarise(H.max = mean(Hmax),
            H.m = mean(Hmean),
            AGB.m = mean(AGB)/2,
            AGB.sd = sd(AGB/2),
            AGB.se = sd(AGB/2)/sqrt(8),
            H.sd = sd(Hmean),
            DBH.max = mean(DBHmax),
            DBH.m = mean(DBHmean),
            DBH.sd = sd(DBHmean),
            .groups = "keep")

levels(Tree.data.H.sum$Treatment) <- c("Control","Removal")

ggplot(data = Tree.data.H.sum,
       aes(x = Time2,
           y = AGB.m, ymin = AGB.m - AGB.sd, ymax = AGB.m + AGB.sd,
           color = Treatment, fill = Treatment,
           group = Treatment)) +
  geom_ribbon(alpha = 0.5, color = NA) +
  geom_line() +
  theme_bw() +
  labs(x = "", y = "Above-ground carbon biomass \r\n (kgC/m²)") +
  scale_fill_manual(values = c('lightgrey',"black")) +
  scale_colour_manual(values = c('darkgrey',"black")) +
  scale_x_continuous(breaks = seq(2011,2019,2)) +
  theme(text = element_text(size = 24),
        legend.position = c(0.1,0.9))


Tree.data.growth <- Tree.data %>% group_by(ID) %>% mutate(growth = c(NA,diff(DBH)),
                                      year = floor(Time2)) %>% filter(year %in% c(2012,2018))


levels(Tree.data.growth$Treatment) <- c("Control","Removal")

ggplot(data = Tree.data.growth %>% filter(growth > 0)) +
  geom_boxplot(aes(x = as.factor(year),y = growth,fill = Treatment),alpha = 0.4) +
  scale_fill_manual(values = c('lightgrey',"black")) +
  scale_colour_manual(values = c('darkgrey',"black")) +
  labs(x = "", y = "\u0394DBH (mm)") +
  theme_bw() +
  theme(text = element_text(size = 24),
        legend.position = c(0.85,0.9))
