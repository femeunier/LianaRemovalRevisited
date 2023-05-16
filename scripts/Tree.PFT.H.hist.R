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

Tree.data.sum.plot <- Tree.data.sum %>% group_by(timing,Treatment,H.class,PFT) %>%
  summarise(N.mean = mean(N,na.rm = TRUE),
            N.sd = sd(N,na.rm = TRUE),
            BA.mean = mean(BA,na.rm = TRUE),
            BA.sd = sd(BA,na.rm = TRUE),
            .groups = "keep")

Tree.data.sum.plot %>% group_by(timing,Treatment, H.class) %>% summarise(N = sum(N.mean)*10000) %>% pivot_wider(values_from = N,
                                                                                                                names_from = c(Treatment,timing)) %>%
  mutate(diff_init = (R_1 - C_1),
         diff_end = (R_17 - C_17),
         change_removal =  (R_17 - R_1),
         change_control =  (C_17 - C_1))
