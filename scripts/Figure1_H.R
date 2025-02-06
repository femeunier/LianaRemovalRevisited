rm(list = ls())

library(dplyr)
library(ggplot2)
library(ggdist)
library(cowplot)
library(ggthemes)
library(tidyr)

alpha = 0.11

Main.OP <- readRDS("./outputs/Main.OP.50.RDS") %>%
  mutate(site = case_when(site == "DAN" ~ "Danum Valley",
                          site == "LAM" ~ "Lambir",
                          site == "SGW" ~ "Sungai Wain",
                          site == "BUL" ~ "	Barito Ulu Nagy",

                          site == "Kisangani_all" ~ "Yoko",
                          site == "ALF" ~ "Parque Cristalino",
                          site == "FLO" ~ "Fazenda Floresta",
                          site == "FRP" ~ "Fazenda Rio Preto",
                          site == "GAU" ~ "Gaúcha do Norte",
                          site == "OKU" ~ "Oku",
                          site == "PEA" ~ "Parque Estadual do Araguaia",
                          site == "POA" ~ "Porto Alegre do Norte",
                          site == "SAA" ~ "Santana do Araguaia",
                          site == "SAT" ~ "Santa Terezinha",
                          site == "SIP" ~ "Sinop",
                          site == "TAN" ~ "Tanguro",
                          site == "VCR" ~ "Vera Cruz",
                          site == "Casa_Roubik" ~ "Casa Roubik",
                          site == "group_Metro" ~ "Metropolitano",
                          site == "group_North" ~ "Plot 32",
                          site == "Sherman" ~ "Fort Sherman",
                          TRUE ~ site)) %>%
  ungroup() %>%
  mutate(site = factor(site,
                       levels = sort(unique(site),decreasing = TRUE))) %>%
  filter(site != "Danum Valley") %>%
  mutate(site.group = case_when(site == "Mokabi" ~ "Africa",
                                site == "Luki" ~ "Africa",
                                TRUE ~ site.group))


diff.H.sites <- Main.OP %>%
  group_by(site,liana.cat) %>%
  summarise(m = median(diff_h/no*100,na.rm = TRUE),
            Qlow = quantile(diff_h/no*100,alpha/2,na.rm = TRUE),
            Qhigh = quantile(diff_h/no*100,1-alpha/2,na.rm = TRUE),

            m.abs = median(diff_h,na.rm = TRUE),
            low.abs = quantile(diff_h,alpha/2,na.rm = TRUE),
            high.abs = quantile(diff_h,1-alpha/2,na.rm = TRUE),
            .groups = "keep") %>%
  pivot_wider(names_from = liana.cat,
              values_from = -c(site,liana.cat))


diff.H.sites %>%
  filter(site != "Total") %>%
  filter(m_high < m_low) %>%
  filter(Qhigh_high < Qlow_low)

ggplot(data = diff.H.sites) +
  geom_segment(aes(y = site, yend = site, x = Qlow_low, xend = Qhigh_low)) +
  geom_segment(aes(y = site, yend = site, x = Qlow_high, xend = Qhigh_high),
               color = "red", linetype = 2) +
  theme_bw()

ggplot(diff.H.sites,
       aes(x = m_low, y = m_high)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw()

summary(diff.H.sites$m_high - diff.H.sites$m_low)


diff.H.sites %>%
  ungroup() %>%
  summarise(N = length(which(m_low < m_high)),
            N2 = length(which(m_low > m_high)))

ggplot(data = Main.OP %>%
         filter(site == "BCI"),
       aes(x = diff_h/no*100,
           y = site,
           color = liana.cat,
           fill = liana.cat,
           alpha = 0.7)) +
  geom_vline(xintercept = 0,linetype = 1) +
  stat_halfeye(color = NA) +
  stat_pointinterval(aes(alpha = signif_rel2),
                     .width = c(1-alpha),
                     position = position_dodge(width = 0)) +
  scale_x_continuous(limits = c(-15,5), breaks = 0) +
  labs(y = "", color = "", fill = "",x = "") +
  theme_minimal_hgrid() +
  # facet_wrap(~ site.group, scales = "free_y") +
  guides(alpha = "none", fill = "none", color = "none") +
  theme(legend.position = c(0.1,0.9),
        text = element_text(size = 24)) +
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  scale_fill_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred"))


# ggplot(data = Main.OP %>%
#          filter(site.group == "Panama"),
#        aes(x = diff_h/no*100,
#            y = site,
#            color = liana.cat,
#            fill = liana.cat,
#            alpha = 0.7)) +
#   geom_vline(xintercept = 0,linetype = 1) +
#   stat_halfeye(color = NA) +
#   stat_pointinterval(aes(alpha = signif_rel2),
#                      .width = c(1-alpha),
#                      position = position_dodge(width = 0)) +
#   scale_x_continuous(limits = c(-15,5)) +
#   labs(y = "", color = "", fill = "",x = "") +
#   theme_minimal_hgrid() +
#   # facet_wrap(~ site.group, scales = "free_y") +
#   guides(alpha = "none", fill = "none", color = "none") +
#   theme(legend.position = c(0.1,0.9),
#         text = element_text(size = 24)) +
#   scale_color_manual(values = c("no" = "darkgreen",
#                                 "low" = "orange",
#                                 "high"= "darkred")) +
#   scale_fill_manual(values = c("no" = "darkgreen",
#                                "low" = "orange",
#                                "high"= "darkred"))


Main.OP <- Main.OP %>%
  mutate(new.site = paste0(site,"\r\n",
                           "(N = ",N.tot,")"),
         new.site2 = paste0(site," ",
                           "(N = ",N.tot,")")) %>%
  mutate(new.site = factor(new.site,
                       levels = sort(unique(new.site),decreasing = TRUE)),
         new.site2 = factor(new.site2,
                           levels = sort(unique(new.site2),decreasing = TRUE)))

ggplot(data = Main.OP %>%
         filter(site.group == "Panama"),
       aes(x = diff_h/no*100,
           y = new.site,
           color = liana.cat,
           fill = liana.cat,
           alpha = 0.9)) +
  geom_vline(xintercept = 0,linetype = 1) +
  stat_halfeye(color = NA, aes(alpha = signif_rel)) +
  stat_pointinterval(aes(alpha = signif_rel2),
                     .width = c(1-alpha),
                     position = position_dodge(width = 0)) +
  scale_x_continuous(limits = c(-15,20)) +
  labs(y = "", color = "", fill = "",x = "") +
  theme_minimal_hgrid() +
  # facet_wrap(~ site.group, scales = "free_y") +
  guides(alpha = "none", fill = "none", color = "none") +
  theme(legend.position = c(0.1,0.9),
        text = element_text(size = 24)) +
  scale_color_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred")) +
  scale_fill_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred"))

ggplot(data = Main.OP %>%
         filter(site.group == "Africa"),
       aes(x = diff_h/no*100,
           y = site,
           color = liana.cat,
           fill = liana.cat,
           alpha = 0.5)) +
  geom_vline(xintercept = 0,linetype = 1) +
  stat_halfeye(color = NA, aes(alpha = signif_rel)) +
  stat_pointinterval(aes(alpha = signif_rel2),
                     .width = c(1-alpha),
                     position = position_dodge(width = 0)) +
  scale_x_continuous(limits = c(-30,10)) +
  labs(y = "", color = "", fill = "",x = "") +
  theme_minimal_hgrid() +
  # facet_wrap(~ site.group, scales = "free_y") +
  guides(alpha = "none", fill = "none", color = "none") +
  theme(legend.position = c(0.1,0.9),
        text = element_text(size = 24)) +
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  scale_fill_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred"))


ggplot(data = Main.OP %>%
         filter(site.group == "Africa",
                (site %in% c("Asenanyo","Cape Three Points","Cavalla","Dadieso"))),
       aes(x = diff_h/no*100,
           y = new.site,
           color = liana.cat,
           fill = liana.cat,
           alpha = 0.5)) +
  geom_vline(xintercept = 0,linetype = 1) +
  stat_halfeye(color = NA, aes(alpha = signif_rel)) +
  stat_pointinterval(aes(alpha = signif_rel2),
                     .width = c(1-alpha),
                     position = position_dodge(width = 0)) +
  scale_x_continuous(limits = c(-30,5)) +
  labs(y = "", color = "", fill = "",x = "") +
  theme_minimal_hgrid() +
  # facet_wrap(~ site.group, scales = "free_y") +
  guides(alpha = "none", fill = "none", color = "none") +
  theme(legend.position = c(0.1,0.9),
        text = element_text(size = 24)) +
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  scale_fill_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred"))


ggplot(data = Main.OP %>%
         filter(site.group == "Africa",
                !(site %in% c("Asenanyo","Cape Three Points","Cavalla","Dadieso"))),
       aes(x = diff_h/no*100,
           y = new.site2,
           color = liana.cat,
           fill = liana.cat,
           alpha = 0.5)) +
  geom_vline(xintercept = 0,linetype = 1) +
  stat_halfeye(color = NA, aes(alpha = signif_rel)) +
  stat_pointinterval(aes(alpha = signif_rel2),
                     .width = c(1-alpha),
                     position = position_dodge(width = 0)) +
  scale_x_continuous(limits = c(-30,5)) +
  labs(y = "", color = "", fill = "",x = "") +
  theme_minimal_hgrid() +
  # facet_wrap(~ site.group, scales = "free_y") +
  guides(alpha = "none", fill = "none", color = "none") +
  theme(legend.position = c(0.1,0.9),
        text = element_text(size = 24)) +
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  scale_fill_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred"))



ggplot(data = Main.OP %>%
         filter(site.group == "Australasia"),
       aes(x = diff_h/no*100,
           y = new.site,
           color = liana.cat,
           fill = liana.cat,
           alpha = 0.5)) +
  geom_vline(xintercept = 0,linetype = 1) +
  stat_halfeye(color = NA, aes(alpha = signif_rel)) +
  stat_pointinterval(aes(alpha = signif_rel2),
                     .width = 1-alpha,
                     position = position_dodge(width = 0)) +
  scale_x_continuous(limits = c(-50,15)) +
  labs(y = "", color = "", fill = "",x = "") +
  theme_minimal_hgrid() +
  # facet_wrap(~ site.group, scales = "free_y") +
  guides(alpha = "none", fill = "none", color = "none") +
  theme(legend.position = c(0.1,0.9),
        text = element_text(size = 24)) +
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  scale_fill_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred"))


ggplot(data = Main.OP %>%
         filter(site.group == "Amazon"),
       aes(x = diff_h/no*100,
           y = new.site2,
           color = liana.cat,
           fill = liana.cat,
           alpha = 0.5)) +
  geom_vline(xintercept = 0,linetype = 1) +
  stat_halfeye(color = NA, aes(alpha = signif_rel)) +
  stat_pointinterval(aes(alpha = signif_rel2),
                     .width = c(1-alpha),
                     position = position_dodge(width = 0)) +
  scale_x_continuous(limits = c(-40,25)) +
  labs(y = "", color = "", fill = "",x = "") +
  theme_minimal_hgrid() +
  # facet_wrap(~ site.group, scales = "free_y") +
  guides(alpha = "none", fill = "none", color = "none") +
  theme(legend.position = c(0.1,0.9),
        text = element_text(size = 24)) +
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  scale_fill_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred"))


Main.OP %>%
  group_by(site) %>%
  summarise(m.low = unique((N.tot - N.high - N.low)/N.tot),
            m.high = unique(N.high/N.tot),
            .groups = "keep") %>%
  ungroup() %>%
  summarise(m.m.low = mean(m.low),
            m.m.high = mean(m.high),
            .groups = "keep")


Main.OP %>%
  filter(site != 'Total') %>%
  dplyr::select(site,liana.cat,signif_rel2) %>%
  distinct() %>%
  group_by(liana.cat,signif_rel2) %>%
  summarise(N = n())

Main.OP %>%
  filter(site != 'Total') %>%
  dplyr::select(site,liana.cat,signif_rel2,diff_h,no) %>%
  group_by(liana.cat,site,signif_rel2) %>%
  summarise(Delta = mean(diff_h/no,na.rm = TRUE)) %>%
  filter(Delta > 0,
         signif_rel2 == 1)

Main.OP %>%
  filter(site != 'Total') %>%
  dplyr::select(site,liana.cat,diff_h,no) %>%
  group_by(liana.cat,site) %>%
  summarise(Delta = mean(diff_h/no,na.rm = TRUE)) %>%
  pivot_wider(names_from = liana.cat,
              values_from = Delta) %>%
  filter(high < low)



Main.OP %>%
  filter(site != 'Total') %>%
  dplyr::select(site,liana.cat,signif_rel2,diff_h,no) %>%
  group_by(liana.cat,site,signif_rel2) %>%
  summarise(Delta = mean(diff_h/no,na.rm = TRUE),
            Delta_low = quantile(diff_h/no,0.055,na.rm = TRUE),
            Delta_high = quantile(diff_h/no,1-0.055,na.rm = TRUE),
            Delta.h = mean(diff_h,na.rm = TRUE),
            Delta.h_low = quantile(diff_h,0.055,na.rm = TRUE),
            Delta.h_high = quantile(diff_h,1-0.055,na.rm = TRUE)) %>%
  filter(signif_rel2 == 1) %>%
  filter(liana.cat == "low") %>%
  arrange((Delta.h))
