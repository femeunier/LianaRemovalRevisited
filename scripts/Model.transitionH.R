rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(LianaRemovalRevisited)

plots2consider = c(8,16,12,13,5,7,14,11)
plots2consider = c(8,16,13,11,7,5)
# plots2consider = seq(1,16)

## Hypothesis 1
# Default allometry
Tree.data.H <- Tree.data %>% ungroup() %>%
  filter(plot %in% plots2consider) %>%
  filter(!is.na(DBH)) %>%
  mutate(DBH = DBH/10) %>%
  mutate(H = 51.38*(1-exp(-0.01322*(DBH*10)**0.6465))) %>%
  mutate(CA = 0.66*(DBH**1.34)) %>%
  group_by(timing,Time2,Treatment,plot,subplot,subsubplot) %>%
  summarise(H = max(H, na.rm = TRUE),
            DBHmean = mean(DBH, na.rm = TRUE),
            DBHmax = max(DBH, na.rm = TRUE),
            .groups = "keep") %>%
  group_by(timing,Time2,Treatment,plot) %>%
  summarise(Hmean = mean(H, na.rm = TRUE),
            Hmax = max(H, na.rm = TRUE),
            DBHmean = mean(DBHmean, na.rm = TRUE),
            DBHmax = max(DBHmax, na.rm = TRUE),
            .groups = "keep")

ggplot(data = Tree.data.H) +
  geom_line(aes(x = timing, y = Hmean, color = Treatment,group = interaction(Treatment,plot))) +
  theme_bw()



Tree.data.H.sum <- Tree.data.H %>% group_by(timing,Time2,Treatment) %>%
  summarise(H.max = mean(Hmax),
            H.m = mean(Hmean),
            H.sd = sd(Hmean),
            DBH.max = mean(DBHmax),
            DBH.m = mean(DBHmean),
            DBH.sd = sd(DBHmean),
            .groups = "keep")

ggplot(data = Tree.data.H.sum,
       aes(x = timing,
           y = H.m, ymin = H.m - H.sd, ymax = H.m + H.sd,
           color = Treatment, fill = Treatment,
           group = Treatment)) +
  geom_ribbon(alpha = 0.5, color = NA) +
  geom_line() +
  theme_bw()


ggplot(data = Tree.data.H.sum,
       aes(x = timing,
           y = H.max, ymin = H.max - H.sd, ymax = H.max + H.sd,
           color = Treatment, fill = Treatment,
           group = Treatment)) +
  geom_ribbon(alpha = 0.5, color = NA) +
  geom_line() +
  theme_bw()


ggplot(data = Tree.data.H.sum,
       aes(x = timing,
           y = DBH.m, ymin = DBH.m -  DBH.sd, ymax =  DBH.m +  DBH.sd,
           color = Treatment, fill = Treatment,
           group = Treatment)) +
  geom_ribbon(alpha = 0.5, color = NA) +
  geom_line() +
  theme_bw()


ggplot(data = Tree.data.H.sum,
       aes(x = timing,
           y =  DBH.max, ymin =  DBH.max -  DBH.sd, ymax =  DBH.max +  DBH.sd,
           color = Treatment, fill = Treatment,
           group = Treatment)) +
  geom_ribbon(alpha = 0.5, color = NA) +
  geom_line() +
  theme_bw()

Tree.data.H.pval <- Tree.data.H %>% group_by(timing,Time2) %>%
  summarise(p.val = summary(lm(formula = Hmean ~ Treatment))[["coefficients"]][2,4],
            .groups = "keep")

plot(Tree.data.H.pval$timing,Tree.data.H.pval$p.val,type = 'l',ylim = c(0,1))
abline(h = 0.05, col = "red")

#####################################################################################################
## Hypothesis 2
# Liana-specific allometry

ggplot(data = Tree.data) +
  geom_line(aes(x = timing, y = DBH, group = ID,color = as.factor(plot))) +
  facet_wrap(~~Treatment) +
  theme_bw()

Tree.data.growth <- Tree.data %>% filter(plot %in% plots2consider) %>% group_by(Treatment, plot, ID) %>% mutate(Ndata = length(which(!is.na(unique(DBH))))) %>%
  filter(Ndata > 2) %>%
  summarise(r2 = summary(lm(formula = DBH ~ Time))[["r.squared"]],
            slope = coef(lm(formula = DBH ~ Time))[2],
            .groups = "keep")

summary(Tree.data.growth %>% filter(slope > 0) %>% pull(r2))
hist(Tree.data.growth$r2)
hist(Tree.data.growth$slope)

plot(Tree.data.growth$slope,Tree.data.growth$r2)

ggplot(data = Tree.data %>% filter(ID %in% (Tree.data.growth %>% filter(r2 < 0.5) %>% pull(ID)),
                                   plot %in% plots2consider)) +
  geom_line(aes(x = timing, y = DBH, group = ID,color = as.factor(plot))) +
  facet_wrap(~~Treatment) +
  theme_bw()

ggplot(data = Tree.data %>% filter(ID %in% (Tree.data.growth %>% filter(slope > 0.) %>% pull(ID)),
                                   plot %in% plots2consider)) +
  geom_line(aes(x = timing, y = DBH, group = ID,color = as.factor(plot))) +
  facet_wrap(~~Treatment) +
  theme_bw()


Delta_t <- 10  # year

End.time <- max(Tree.data$Time2)
Init.time <- min(Tree.data$Time2)

Tree.data.H.mod.ind <- Tree.data %>% filter(plot %in% plots2consider) %>%
  mutate(DBH = DBH/10) %>%
  mutate(CA = 0.66*(DBH**1.34)) %>%
  group_by(ID) %>%
  mutate(Ndata = length(which(!is.na(unique(DBH))))) %>%
  filter(Ndata > 2) %>%
  mutate(time.init = min(timing[!is.na(DBH)]),
         time.end = time.init + Delta_t*2) %>%
  mutate(a = coef(lm(DBH ~ Time2))[1],
         b = coef(lm(DBH ~ Time2))[2]) %>%
  mutate(DBH.init = DBH[which.min(abs(timing - time.init))],
         DBH.Tr = case_when((Time2 + Delta_t) <= End.time ~ DBH[which.min(abs(timing - time.end))],
                            TRUE ~ a + b*(Time2[time.init] + Delta_t))) %>%
  mutate(H.init = exp(3.89) *(DBH.init**1.0)/(43.19 + DBH.init**1.0),
         H.Tr = exp(3.89) *(DBH.Tr**1.06)/(43.19 + DBH.Tr**1.06)) %>%
  mutate(v.mod = (1. - 1.06)/(DBH.init - DBH.Tr)*DBH + (1. - (1. - 1.06)/(DBH.init - DBH.Tr)*DBH.init)) %>%
  mutate(H.mod = case_when(is.na(DBH[timing == 17]) ~ NA_real_,
                           Treatment == "R" & time.init > 1    ~ exp(3.89) *(DBH**1.06)/(43.19 + DBH**1.06),                                           # Recruits after the removal
                           Treatment == "R" & Time2 >= (Init.time + Delta_t)    ~ exp(3.89) *(DBH**1.06)/(43.19 + DBH**1.06),                          # After transition
                           Treatment == "R" & 1.01*DBH.init >= DBH.Tr                ~ exp(3.89) *(DBH**1.0)/(43.19 + DBH**1.0),                       # Special case: no (significant) differences before and after removal
                           Treatment == "R" ~ exp(3.89) *(DBH**v.mod)/(43.19 + DBH**v.mod),                                                            # Transition period
                           TRUE ~ exp(3.89) *(DBH**1.0)/(43.19 + DBH**1.0))) %>%                                                                       # Control plots
  mutate(case = case_when(is.na(DBH[timing == 17]) ~ "Dead",
                           Treatment == "R" & time.init > 1 ~ "Recruits",
                           Treatment == "R" & Time2 >= (Init.time + Delta_t) ~ "After",
                           Treatment == "R" & 1.01*DBH.init >= DBH.Tr ~ "Special case",
                           Treatment == "R" ~ "Transition",
                           TRUE ~ "Control/before"))


Tree.data.H.mod <- Tree.data.H.mod.ind %>%
  group_by(timing,Time2,Treatment,plot,subplot,subsubplot) %>%
  summarise(H.mod = max(H.mod, na.rm = TRUE),
            DBHmean = mean(DBH, na.rm = TRUE),
            DBHmax = max(DBH, na.rm = TRUE),
            .groups = "keep") %>%
  mutate(H.mod = case_when(H.mod < 0 ~ 0,
                           TRUE ~ H.mod)) %>%
  group_by(timing,Time2,Treatment,plot) %>%
  summarise(Hmean = mean(H.mod, na.rm = TRUE),
            Hmax = max(H.mod, na.rm = TRUE),
            DBHmean = mean(DBHmean, na.rm = TRUE),
            DBHmax = max(DBHmax, na.rm = TRUE),
            .groups = "keep")


Tree.data.H.mod.ind

# ggplot(data = Tree.data.H.mod) +
#   geom_line(aes(x = DBH, y = H.mod, group = ID)) +
#   geom_line(data = Tree.data.H.mod %>% filter(ID == 200),
#             aes(x = DBH, y = H.mod, group = ID), color = "red",size = 2) +
#   facet_wrap(~ Treatment) +
#   theme_bw()
#
# tmp <- Tree.data.H.mod %>% filter(ID == 200)
# dbhs <- seq(min(tmp$DBH),max(tmp$DBH),length.out = 1000)
# dbhs <- seq(1,250)
# plot(dbhs,exp(3.89) *(dbhs**1.0)/(43.19 + dbhs**1.0),col = 'red',type = 'l')
# lines(dbhs,exp(3.89) *(dbhs**1.06)/(43.19 + dbhs**1.06),col = 'green')
# lines(tmp$DBH,tmp$H.mod,type = 'p')
# lines(tmp$DBH.Tr,tmp$H.Tr,type = 'p')
# lines(tmp$DBH.init,tmp$H.init,type = 'p')
# ggplot(data = Tree.data) +
#   geom_line(aes(x = timing, y = DBH, group = ID,color = as.factor(plot))) +
#   facet_wrap(~~Treatment) +
#   theme_bw()

ggplot(data = Tree.data.H.mod) +
  geom_line(aes(x = timing, y = Hmean, color = Treatment,group = interaction(Treatment,plot))) +
  theme_bw()


Tree.data.H.mod.sum <- Tree.data.H.mod %>% group_by(timing,Time2,Treatment) %>%
  summarise(H.max = mean(Hmax),
            H.m = mean(Hmean),
            H.sd = sd(Hmean),
            .groups = "keep")

ggplot(data = Tree.data.H.mod.sum,
       aes(x = timing,
           y = H.m, ymin = H.m - H.sd, ymax = H.m + H.sd,
           color = Treatment, fill = Treatment,
           group = Treatment)) +
  geom_ribbon(alpha = 0.5, color = NA) +
  geom_line() +
  theme_bw()

ggplot(data = Tree.data.H.mod.sum,
       aes(x = timing,
           y = H.max, ymin = H.max - H.sd, ymax = H.max + H.sd,
           color = Treatment, fill = Treatment,
           group = Treatment)) +
  geom_ribbon(alpha = 0.5, color = NA) +
  geom_line() +
  theme_bw()

Tree.data.H.mod.pval <- Tree.data.H.mod %>% group_by(timing,Time2) %>%
  summarise(p.val = summary(lm(formula = Hmean ~ Treatment))[["coefficients"]][2,4],
            .groups = "keep")

plot(Tree.data.H.mod.pval$timing,Tree.data.H.mod.pval$p.val,type = 'l',ylim = c(0,1))
abline(h = 0.05, col = "red")



All.models <- bind_rows(list(Tree.data.H.sum %>% mutate(type = "PNAS, 2015"),
                             Tree.data.H.mod.sum %>% mutate(type = "PNAS + Allom"))) %>%
  mutate(type = factor(type,levels = c("PNAS, 2015","PNAS + Allom")))

levels(All.models$Treatment) <- c("Control","Removal")

ggplot(data = All.models,
       aes(x = Time2,
           y = H.m, ymin = H.m - H.sd, ymax = H.m + H.sd,
           color = Treatment, fill = Treatment,
           group = Treatment)) +
  geom_ribbon(alpha = 0.5, color = NA) +
  labs(x = "", y = "Mean tree height (m)") +
  scale_fill_manual(values = c('lightgrey',"black")) +
  scale_colour_manual(values = c('darkgrey',"black")) +
  scale_x_continuous(breaks = seq(2011,2019,2)) +
  geom_line() +
  facet_wrap(~ type) +
  theme_bw() +
  theme(text = element_text(size = 24),
        legend.position = c(0.6,0.9))

All.models %>% filter(timing == 17) %>% group_by(Treatment,type) %>% summarise(H.m = mean(H.m))
