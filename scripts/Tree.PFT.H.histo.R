rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(LianaRemovalRevisited)

Tree.data.sum <- Tree.data %>% ungroup() %>% filter(timing %in% c(1,17)) %>%
  filter(!is.na(DBH)) %>%
  mutate(DBH = DBH/10) %>%
  mutate(H = 51.38*(1-exp(-0.01322*(DBH*10)**0.6465))) %>%
  mutate(PFT = case_when(WD > 0.69 ~ 4,
                         WD < 0.49 ~ 2,
                         TRUE ~ 3),
         H.class = case_when(H < 5 ~ 0,
                               H < 10 ~ 1,
                               H < 15 ~ 2,
                               H < 20 ~ 3,
                               H < 25 ~ 4,
                               H < 30 ~ 5,
                               H >= 30 ~ 6)) %>%
  group_by(timing,Treatment,plot,PFT,H.class) %>%
  summarise(N = length(H)/(60*60),
            BA = sum(DBH*DBH/4*pi)/(60*60),
            .groups = 'keep')

Tree.data.sum.plot <- Tree.data.sum %>%
  group_by(timing,Treatment,H.class,PFT) %>%
  summarise(N.mean = mean(N,na.rm = TRUE),
            N.sd = sd(N,na.rm = TRUE),
            BA.mean = mean(BA,na.rm = TRUE),
            BA.sd = sd(BA,na.rm = TRUE),
            .groups = "keep")

ggplot(data = Tree.data.sum.plot,
       aes(x = H.class,y = N.mean*10000, color = as.factor(PFT),fill = as.factor(PFT))) +
  geom_bar(stat="identity",na.rm=TRUE) +
  labs(x = "H (m)", y = "Tree density (#/ha)", fill = "PFT") +
  theme_bw() +
  scale_fill_manual(values = c("#9FFF8C","#44CC29","#137300")) +
  scale_color_manual(values = c("#9FFF8C","#44CC29","#137300")) +
  scale_x_continuous(breaks = seq(2,6),
                     labels = c("< 15","15-20","20-25","25-30","> 30")) +
  theme(text = element_text(size = 20),
        legend.position = c(0.8,0.8),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_grid(timing ~ Treatment) +
  guides(color = FALSE)
