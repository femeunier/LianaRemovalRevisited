rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(LianaRemovalRevisited)

Tree.data.sum <- Tree.data %>% ungroup() %>% filter(timing %in% c(1,17)) %>%
  filter(!is.na(DBH)) %>%
  mutate(DBH = DBH/10) %>%
  mutate(PFT = case_when(WD > 0.69 ~ 4,
                         WD < 0.49 ~ 2,
                         TRUE ~ 3),
         DBH.class = case_when(DBH < 1 ~ 0,
                               DBH < 10 ~ 1,
                               DBH < 20 ~ 2,
                               DBH < 30 ~ 3,
                               DBH < 40 ~ 4,
                               DBH < 50 ~ 5,
                               DBH < 60 ~ 6,
                               DBH < 70 ~ 7,
                               DBH < 80 ~ 8,
                               DBH >= 80 ~ 9)) %>%
  group_by(timing,Treatment,plot,PFT,DBH.class) %>%
  summarise(N = length(H)/(60*60),
            BA = sum(DBH*DBH/4*pi)/(60*60),
            .groups = 'keep')


Tree.data.sum.plot <- Tree.data.sum %>%
  group_by(timing,Treatment,DBH.class,PFT) %>%
  summarise(N.mean = mean(N,na.rm = TRUE),
            N.sd = sd(N,na.rm = TRUE),
            BA.mean = mean(BA,na.rm = TRUE),
            BA.sd = sd(BA,na.rm = TRUE),
            .groups = "keep")

Tree.data %>% group_by(timing,Treatment,plot) %>% summarise(N = length(DBH[!is.na(DBH)])/(60*60)) %>%
  group_by(timing,Treatment) %>% summarise(Nmean = mean(N))

ggplot(data = Tree.data.sum.plot %>% filter(DBH.class > 1),
       aes(x = DBH.class,y = N.mean*10000, color = as.factor(PFT),fill = as.factor(PFT))) +
  geom_bar(stat="identity",na.rm=TRUE) +
  labs(x = "DBH (cm)", y = "Tree density (#/ha)", fill = "PFT") +
  theme_bw() +
  scale_fill_manual(values = c("#9FFF8C","#44CC29","#137300")) +
  scale_color_manual(values = c("#9FFF8C","#44CC29","#137300")) +
  scale_x_continuous(breaks = seq(2,9),
                     labels = c("10-20","20-30","30-40","40-50","50-60",
                              "60-70","70-80","> 80")) +
  theme(text = element_text(size = 20),
        legend.position = c(0.8,0.8),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_grid(timing ~ Treatment) +
  guides(color = FALSE)


Tree.data.sum.plot %>% group_by(timing,Treatment, DBH.class) %>% summarise(N = sum(N.mean)*10000) %>% pivot_wider(values_from = N,
                                                                                                                   names_from = c(Treatment,timing)) %>%
  mutate(diff_init = (R_1 - C_1),
         diff_end = (R_17 - C_17),
         change_removal =  (R_17 - R_1),
         change_control =  (C_17 - C_1))

