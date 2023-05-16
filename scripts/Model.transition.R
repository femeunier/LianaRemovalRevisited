rm(list = ls())

library(dplyr)
library(ggplot2)
library(LianaRemovalRevisited)
library(tidyr)

Delta_t <- 10  # year

End.time <- max(Tree.data$Time2)
Init.time <- min(Tree.data$Time2)

Tree.data.m <- Tree.data %>%
  mutate(DBH = DBH/10) %>%
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
  mutate(H = 51.38*(1-exp(-0.01322*(DBH*10)**0.6465)),
         AGB = 0.0509*WD*DBH**2*H) %>%
  mutate(v.mod = (1. - 1.06)/(DBH.init - DBH.Tr)*DBH + (1. - (1. - 1.06)/(DBH.init - DBH.Tr)*DBH.init)) %>%
  mutate(H.mod = case_when(is.na(DBH[timing == 17]) ~ NA_real_,
                           Treatment == "R" & time.init > 1    ~ exp(3.89) *(DBH**1.06)/(43.19 + DBH**1.06),                                           # Recruits after the removal
                           Treatment == "R" & Time2 >= (Init.time + Delta_t)    ~ exp(3.89) *(DBH**1.06)/(43.19 + DBH**1.06),                          # After transition
                           Treatment == "R" & 1.01*DBH.init >= DBH.Tr                ~ exp(3.89) *(DBH**1.0)/(43.19 + DBH**1.0),                       # Special case: no (significant) differences before and after removal
                           Treatment == "R" ~ exp(3.89) *(DBH**v.mod)/(43.19 + DBH**v.mod),                                                            # Transition period
                           TRUE ~ exp(3.89) *(DBH**1.0)/(43.19 + DBH**1.0))) %>%                                                                       # Before transition
  mutate(AGB.mod = 0.0509*WD*DBH**2*H.mod) %>%
  mutate(AGBC.mod =  AGB.mod*CC/100)

ggplot(data = Tree.data.m) +
  geom_point(aes(x = H,y = H.mod, color = Treatment)) +
  geom_abline(intercept = 0, slope = 1,linetype = 2, color = "black") +
  theme_bw()


########################################################################################################################
dbhs <- seq(10,150,length.out = 1000)

H_vdh <- 51.38*(1-exp(-0.01322*(dbhs*10)**0.6465))
H_sruthi <- exp(3.89) *(dbhs**1.06)/(43.19 + dbhs**1.06)
H_sruthi_L <- exp(3.89) *(dbhs**1.0)/(43.19 + dbhs**1.0)

alpha_prime <- 0.0509
rho <- 0.39

AGB_vdh <- alpha_prime*rho*dbhs**2*H_vdh
AGB_sruthi <- alpha_prime*dbhs**2*H_sruthi*rho
AGB_sruthi_L <- alpha_prime*dbhs**2*H_sruthi_L*rho

df_allom <- bind_rows(list(data.frame(dbh = dbhs, h = H_vdh, agb = AGB_vdh, type = "PNAS 2015"),
                           data.frame(dbh = dbhs, h = H_sruthi, agb = AGB_sruthi, type = "Sruthi, COI = 0"),
                           data.frame(dbh = dbhs, h = H_sruthi_L, agb = AGB_sruthi_L, type = "Sruthi, COI > 2")))

ggplot(data = Tree.data.m %>% filter(ID == 1501)) +
  geom_point(aes(x = DBH,y = H), color = "black") +
  geom_point(aes(x = DBH,y = H.mod), color = "red") +
  geom_line(data = df_allom,
            aes(x = dbh, y = h, color = type)) +
  theme_bw()

ggplot(data = Tree.data.m %>% filter(ID == 1500)) +
  geom_point(aes(x = DBH,y = AGB), color = "black") +
  geom_point(aes(x = DBH,y = AGB.mod), color = "red") +
  geom_line(data = df_allom,
            aes(x = dbh, y = agb, color = type)) +
  scale_x_continuous(limits = c(10,1.5*max(Tree.data.m %>% filter(ID == 1500) %>% pull(DBH)))) +
  scale_y_continuous(limits = c(0,1.5*max(Tree.data.m %>% filter(ID == 1500) %>% pull(AGB.mod)))) +
  theme_bw()

########################################################################################################################


Tree.data.m.plot <- Tree.data.m %>%
  group_by(Treatment,Time2,plot) %>%
  summarise(AGB = sum(AGB,na.rm = TRUE)/(60*60),
            AGBC = sum(AGBC,na.rm = TRUE)/(60*60),
            AGB.mod = sum(AGB.mod,na.rm = TRUE)/(60*60),
            AGBC.mod = sum(AGBC.mod,na.rm = TRUE)/(60*60),
            .groups = "keep")

Tree.data.m.Treatment <- Tree.data.m.plot %>% group_by(Treatment,Time2) %>%
  summarise(AGB.m = mean(AGB,na.rm = TRUE),
            AGBC.m = mean(AGBC,na.rm = TRUE),
            AGB.sd = sd(AGB,na.rm = TRUE),
            AGBC.sd = sd(AGBC,na.rm = TRUE),
            AGB.mod.m = mean(AGB.mod,na.rm = TRUE),
            AGBC.mod.m = mean(AGBC.mod,na.rm = TRUE),
            AGB.mod.sd = sd(AGB.mod,na.rm = TRUE),
            AGBC.mod.sd = sd(AGBC.mod,na.rm = TRUE),
            .groups = "keep")

ggplot(data = Tree.data.m.Treatment,
       aes(x = Time2, y = AGBC.m, group = Treatment, color = Treatment, fill = Treatment,
           ymin = AGBC.m - AGBC.sd, ymax = AGBC.m + AGBC.sd)) +
  geom_ribbon(alpha = 0.4, color = NA) +
  geom_line() +
  theme_bw()

Tree.data.m.Treatment.long <- Tree.data.m.Treatment %>% pivot_longer(cols = -c("Treatment","Time2"),
                                                                     names_to = "var",
                                                                     values_to = "value") %>%
  mutate(model = case_when(grepl("mod", var, fixed = TRUE) ~ "Mod",
                           TRUE ~ "Default"),
         metric = case_when(grepl(".sd", var, fixed = TRUE) ~ "sd",
                            TRUE ~ "mean"),
         type = case_when(grepl("AGBC", var, fixed = TRUE) ~ "AGBC",
                          TRUE ~ "AGB")) %>%
  dplyr::select(-c("var")) %>%
  pivot_wider(values_from = value,
              names_from = c(type,metric))

# levels(Tree.data.m.Treatment.long$Treatment) <- c("Control","Removal")
# Tree.data.m.Treatment.long$model <- factor(Tree.data.m.Treatment.long$model)
# levels(Tree.data.m.Treatment.long$model) <- c("PNAS, 2015","PNAS + Allom")

ggplot(data = Tree.data.m.Treatment.long,
       aes(x = Time2, y = AGBC_mean, group = Treatment, color = Treatment, fill = Treatment,
           ymin = AGBC_mean - AGBC_sd, ymax = AGBC_mean + AGBC_sd)) +
  geom_ribbon(alpha = 0.4, color = NA) +
  geom_line() +
  facet_wrap(~ model) +
  labs(x = "", y = "Above-ground carbon biomass \r\n (kgC/m²)") +
  scale_fill_manual(values = c('lightgrey',"black")) +
  scale_colour_manual(values = c('darkgrey',"black")) +
  scale_x_continuous(breaks = seq(2011,2019,2)) +
  theme_bw() +
  theme(text = element_text(size = 24),
        legend.position = c(0.6,0.9))

Tree.data.m.Treatment.wide <- Tree.data.m.Treatment.long %>% pivot_wider(values_from = c(AGB_mean,AGBC_mean,AGB_sd,AGBC_sd),
                                                                         names_from = c(model,Treatment))

ggplot(data = Tree.data.m.Treatment.wide) +
  geom_line(aes(x = Time2, y = ((AGBC_mean_Mod_R - AGBC_mean_Mod_C) - (AGBC_mean_Default_R - AGBC_mean_Default_C))/(AGBC_mean_Default_R - AGBC_mean_Default_C))) +
  theme_bw()


