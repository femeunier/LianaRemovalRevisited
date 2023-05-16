rm(list = ls())

library(LVLRE.long)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyr)

Tree.data.filt <- Tree.data %>%
  filter(plot %in% c(5,7,11,8,13,16)) %>%
  filter(timing %in% c(2,10,16)) %>%
  mutate(Time2 = as.factor(Time2),
         plot = as.factor(plot),
         Treatment = as.factor(Treatment)) %>%
  mutate(plot = as.factor(plot)) %>%
  mutate(rep = case_when(plot %in% c(5,8) ~ 1,
                         plot %in% c(7,16) ~ 2,
                         plot %in% c(11,13) ~ 3)) %>%
  filter(!is.na(DBH))

Tree.data.filt$plot <- factor(Tree.data.filt$plot,levels = c(5,7,11,8,13,16))
Tree.data.filt$Treatment <- factor(Tree.data.filt$Treatment,levels = c("R","C"))

levels(Tree.data.filt$Time2) <- c("2012","2016","2019")
levels(Tree.data.filt$Treatment) <- c("Removal","Control")
levels(Tree.data.filt$plot) <- c("rplot05","rplot07","rplot11","cplot08","cplot13","cplot16")

ggplot(data = Tree.data.filt) +
  geom_density(aes(x = DBH/10, fill = Treatment, group = as.factor(plot)), color = NA, alpha = 0.5) +
  facet_grid(Time2 ~ plot) +
  theme_bw()

ggplot(data = Tree.data.filt) +
  geom_density(aes(x = DBH/10, fill = as.factor(rep), group = as.factor(plot)), color = NA, alpha = 0.5) +
  facet_grid(Time2 ~ Treatment) +
  theme_bw() +
  guides(fill = "none")


ggplot(data = Tree.data.filt) +
  geom_histogram(aes(x = DBH/10, fill = as.factor(rep), group = as.factor(plot)),
                 position = "identity",
                 color = NA, alpha = 0.5) +
  facet_grid(Time2 ~ Treatment) +
  theme_bw() +
  labs(x = "DBH (cm)", y = "Number of trees") +
  guides(fill = "none") +
  theme(text = element_text(22))

Tree.data.filt %>%
  group_by(plot,Time2) %>%
  summarise(N = length(DBH),
            DBH.m = mean(DBH/10),
            DBH.median = median(DBH/10),
            DBH.max = max(DBH/10),
            .groups = "keep") %>%
  arrange(Time2,plot)


############################################################################################################

Liana.data.filt <- Liana.data %>%
  filter(plot %in% c(8,13,16)) %>%
  filter(timing %in% c(2,10,16)) %>%
  mutate(Time2 = as.factor(Time2)) %>%
  mutate(plot = as.factor(plot)) %>%
  filter(!is.na(DBH))

levels(Liana.data.filt$Time2) <- c("2012","2016","2019")
levels(Liana.data.filt$plot) <- c("cplot08","cplot13","cplot16")

Liana.data.filt.mut <-
  Liana.data.filt %>% mutate(x = case_when(cuadrante %in% c(0,2000,4000) ~ 10,
                                         cuadrante %in% c(20,2020,4020) ~ 30,
                                         cuadrante %in% c(40,2040,4040) ~ 50),
                             y = case_when(cuadrante %in% c(0,20,40) ~ 10,
                                           cuadrante %in% c(2000,2020,2040) ~ 30,
                                           cuadrante %in% c(4000,4020,4040) ~ 50)) %>%
  mutate(xx = case_when(subcuadrante %in% c(11,21,31,41) ~ 0,
                        subcuadrante %in% c(12,22,32,42) ~ 1,
                        subcuadrante %in% c(13,23,33,43) ~ 2,
                        subcuadrante %in% c(14,24,34,44) ~ 3),
         yy = case_when(subcuadrante %in% c(11,12,13,14) ~ 0,
                        subcuadrante %in% c(21,22,23,24) ~ 1,
                        subcuadrante %in% c(31,32,33,34) ~ 2,
                        subcuadrante %in% c(41,42,43,44) ~ 3)) %>%
  mutate(X = -7.5 + x + xx*5,
         Y = -7.5 + y + yy*5)

tt = unique(Liana.data.filt.mut$Time2)

Liana.data.filt.mut.sum <- Liana.data.filt.mut %>%
  group_by(plot,x,y,Time2,timing) %>%
  summarise(ba = sum(BA/100)/20/20,
            .groups = "keep") %>%
  ungroup()

Liana.data.filt.mut.sum.sub <- Liana.data.filt.mut %>%
  group_by(plot,X,Y,Time2,timing) %>%
  summarise(ba = sum(BA/100)/5/5,
            .groups = "keep") %>%
  ungroup()

ggplot(data = Liana.data.filt.mut.sum) +
  geom_tile(aes(x = x,y = y,
              fill = ba)) +
  facet_grid(Time2 ~ plot) +
  labs(x = "", y = "", fill = "Liana BA \r\n (cm²/m²)") +
  theme_bw() +
  theme(text = element_text(size = 22))

ggplot(data = Liana.data.filt.mut.sum.sub) +
  geom_tile(aes(x = X,y = Y,
                fill = (ba))) +
  facet_grid(Time2 ~ plot) +
  labs(x = "", y = "", fill = "Liana BA \r\n (cm²/m²)") +
  theme_bw() +
  theme(text = element_text(size = 22))


ggplot(data = Liana.data.filt.mut.sum) +
  geom_boxplot(aes(x = as.factor(plot), y = ba)) +
  facet_wrap(~Time2,nrow = 1) +
  labs(x = "", y = "Liana BA (cm²/m²)") +
  theme_bw() +
  theme(text = element_text(size = 22))


ggplot(data = Liana.data.filt.mut.sum) +
  geom_boxplot(aes(x = as.factor(Time2), y = ba)) +
  facet_wrap(~plot,nrow = 1) +
  labs(x = "", y = "Liana BA (cm²/m²)") +
  theme_bw()

Liana.data.filt %>%
  group_by(plot,Time2) %>%
  summarise(N = length(DBH),
            DBH.m = mean(DBH/10),
            DBH.median = median(DBH/10),
            DBH.max = max(DBH/10),
            .groups = "keep") %>%
  arrange(Time2,plot)


ggplot(data = Liana.data.filt.mut.sum.sub) +
  geom_boxplot(aes(x = as.factor(plot), y = ba)) +
  facet_wrap(~Time2,nrow = 1) +
  labs(x = "") +
  theme_bw()

ggplot(data = Liana.data.filt.mut.sum.sub) +
  geom_boxplot(aes(x = as.factor(Time2), y = ba)) +
  facet_wrap(~ as.factor(plot),nrow = 1) +
  labs(x = "") +
  theme_bw()

# %>%
#   complete(plot = as.integer(c(8,13,16)),
#            x = as.integer(0:2),
#            y = as.integer(0:2),
#            Time2 = tt,
#            fill = list(ba = 0))
