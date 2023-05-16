rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(wesanderson)
library(cowplot)

dbhs <- seq(10,150,length.out = 1000)
dbhs <- seq(10,100,1)

H_vdh <- 51.38*(1-exp(-0.01322*(dbhs*10)**0.6465))
H_sruthi <- exp(3.89) *(dbhs**1.06)/(43.19 + dbhs**1.06)
H_sruthi_L <- exp(3.89) *(dbhs**1.0)/(43.19 + dbhs**1.0)

alpha_prime <- 0.0509
rho <- 0.62

# AGB_vdh <- alpha_prime*dbhs**2*H_vdh*rho
AGB_vdh <- alpha_prime*rho*dbhs**2*H_vdh
AGB_sruthi <- alpha_prime*dbhs**2*H_sruthi*rho
AGB_sruthi_L <- alpha_prime*dbhs**2*H_sruthi_L*rho

df_allom <- bind_rows(list(data.frame(dbh = dbhs, h = H_vdh, agb = AGB_vdh, type = "PNAS 2015"),
                           data.frame(dbh = dbhs, h = H_sruthi, agb = AGB_sruthi, type = "Sruthi, COI = 0"),
                           data.frame(dbh = dbhs, h = H_sruthi_L, agb = AGB_sruthi_L, type = "Sruthi, COI > 2")))

pal <- wesanderson::wes_palette("Darjeeling1",n = 3)

ggplot(data = df_allom) +
  geom_line(aes(x = dbh, y = h, color = as.factor(type))) +
  labs(x = "DBH (cm)", y = 'H (m)', color = "Allometry") +
  scale_colour_manual(values = c("black",pal[1:2])) +
  theme_bw() +
  theme(legend.position = c(0.15,0.9),
        text = element_text(size = 20))

ggplot(data = df_allom %>% filter(dbh > 30)) +
  geom_line(aes(x = dbh, y = agb, color = as.factor(type))) +
  labs(x = "DBH (cm)", y = 'AGB (kg)', color = "Allometry") +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10() +
  theme(legend.position = c(0.15,0.9),
        text = element_text(size = 20))

df_allom.ref <- df_allom %>% filter(type != "PNAS 2015") %>% mutate(h_ref = rep(H_vdh,2),
                                                                    agb_ref = rep(AGB_vdh,2)) %>%
  mutate(h_diff = h - h_ref,
         h_diff_rel = 100*(h - h_ref)/h_ref,
         agb_diff = agb - agb_ref,
         agb_diff_rel = 100*(agb - agb_ref)/agb_ref)

df_allom.ref.long <- df_allom.ref %>% dplyr::select(dbh,type, h_diff,h_diff_rel,agb_diff,agb_diff_rel) %>%
  pivot_longer(cols = c(h_diff,h_diff_rel,agb_diff,agb_diff_rel),
               names_to = "variable",
               values_to = "value") %>%
  mutate(y = case_when(grepl(pattern = "h_",variable) ~ "h",
                       TRUE ~ "agb"),
         rel = case_when(grepl(pattern = "_rel",variable) ~ TRUE,
                         TRUE ~ FALSE))

ggplot(data = df_allom.ref.long %>% filter(!rel)) +
  geom_line(aes(x = dbh, y = value, color = as.factor(type))) +
  labs(x = "DBH (cm)", y = '', color = "Allometry") +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  facet_wrap(~ y, scales = "free") +
  theme_bw() +
  theme(legend.position = c(0.2,0.9),
        text = element_text(size = 20))

height_diff <- ggplot(data = df_allom.ref.long %>% filter(variable == "h_diff")) +
  geom_line(aes(x = dbh, y = value, color = as.factor(type))) +
  labs(x = "", y = 'Difference in height (m)', color = "Allometry") +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  scale_x_continuous(labels = c()) +
  theme_bw() +
  scale_colour_manual(values = c(pal[1:2])) +
  theme(legend.position = c(0.8,0.2),
        text = element_text(size = 20))


levels(Tree.data$Treatment) <- c("Control","Removal")
Tree_density <- ggplot(data = Tree.data %>% filter(timing == 1)) +
  geom_density(aes(x = DBH/10, fill = Treatment),alpha = 0.4) +
  scale_x_continuous(limits = c(min(dbhs),max(dbhs))) +
  labs(x = "DBH (cm)", y = 'Density', fill = "Treatment") +
  theme_bw() +
  scale_fill_manual(values = c('lightgrey',"black")) +
  theme(legend.position = c(0.8,0.8),
        text = element_text(size = 20))

plot_grid(height_diff,Tree_density, align = 'hv',nrow = 2)

ggplot(data = df_allom.ref.long %>% filter(rel)) +
  geom_line(aes(x = dbh, y = value, color = as.factor(type))) +
  labs(x = "DBH (cm)", y = '', color = "Allometry") +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  facet_wrap(~ y, scales = "free") +
  theme_bw() +
  theme(legend.position = c(0.2,0.2),
        text = element_text(size = 20))



