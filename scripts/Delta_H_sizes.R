rm(list = ls())

library(brms)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

# Data

all.df <- bind_rows(readRDS("./outputs/All.COI.data.RDS") %>%
                      mutate(sp = str_squish(sp)) %>%
                      filter(dbh >= 10),
                    readRDS("./outputs/All.COI.data.RDS") %>%
                      mutate(sp = str_squish(sp)) %>%
                      filter(dbh >= 10) %>%
                      mutate(site = "Total"))

# Best models

site.loc <- readRDS("./outputs/site.loc.RDS") %>%
  mutate(continent = case_when(lon <= -35 ~ "America",
                               lon <= 50 ~ "Africa",
                               TRUE ~ "Australasia")) %>%
  rename(site = site.common)

sites <- unique(all.df$site)
sites.fac <- c(sites[order(sites[sites != "Total"])],"Total")

# Model predictions
Model.predictions <- bind_rows(readRDS("./outputs/Main.OP.25.RDS") %>%
                                 mutate(target = 25),
                               readRDS("./outputs/Main.OP.50.RDS") %>%
                                 mutate(target = 50),
                               readRDS("./outputs/Main.OP.100.RDS") %>%
                                 mutate(target = 100),
                               readRDS("./outputs/Main.OP.150.RDS") %>%
                                 mutate(target = 150)) %>%
  filter(liana.cat == "high") %>%
  group_by(target,site) %>%
  summarise(m = round(100*mean(diff_h/no,na.rm = TRUE),digits = 1),
            CIlow = round(100*quantile(diff_h/no,0.11/2,na.rm = TRUE),digits = 1),
            CIhigh = round(100*quantile(diff_h/no,1-0.11/2,na.rm = TRUE),digits = 1),
            .groups = "keep")

Nvseffect <- Model.predictions %>%
  left_join(site.loc,
            by = "site") %>%
  mutate(m = case_when(target > dbh.max ~ NA_real_,
                       TRUE ~ m))

Nvseffect.wide <- Nvseffect %>%
  pivot_wider(names_from = target,
              values_from = c(m,CIlow,CIhigh))

df.A <- Nvseffect.wide %>%
  filter(!is.na(m_50) & !is.na(m_25)  & site != "Total")
X <- seq(min(df.A$m_50,na.rm = TRUE),
    max(df.A$m_50,na.rm = TRUE),
    length.out = 1000)

LMA <- lm(data = df.A,
          formula = m_25 ~ m_50, weights = Nlarge)
predictionA <- as.data.frame(predict(LMA,
                       newdata = data.frame(m_50 = X),
                       interval = "confidence") ) %>%
  mutate(m_50 = X) %>%
  rename(m_25 = fit)

A <- ggplot(data = Nvseffect.wide %>%
         filter(!is.na(m_50) & !is.na(m_25)  & site != "Total"),
       aes(x = m_50, y = m_25)) +
  geom_ribbon(data = predictionA,
              aes(ymin = lwr, ymax = upr),color = NA, fill = "grey", alpha = 0.5) +
  geom_line(data = predictionA) +
  geom_point(aes(size = Nlarge)) +
  geom_errorbarh(aes(xmin = CIlow_50, xmax = CIhigh_50)) +
  geom_errorbar(aes(ymin = CIlow_25, ymax = CIhigh_25)) +
  # stat_smooth(method = "lm", color = "black") +

  geom_abline(slope = 1,intercept = 0, color = "black", linetype = 2) +
  theme_bw() +
  labs(x = "",y = "") +
  scale_x_continuous(limits = c(-40,30)) +
  scale_y_continuous(limits = c(-40,30)) +
  coord_equal() +
  theme(text = element_text(size = 24)) +
  guides(size = "none")

################################################################################

df.B <- Nvseffect.wide %>%
  filter(!is.na(m_50) & !is.na(m_100)  & site != "Total")
X <- seq(min(df.B$m_50,na.rm = TRUE),
         max(df.B$m_50,na.rm = TRUE),
         length.out = 1000)

LMB <- lm(data = df.B,
          formula = m_100 ~ m_50, weights = Nlarge)
predictionB <- as.data.frame(predict(LMB,
                                     newdata = data.frame(m_50 = X),
                                     interval = "confidence") ) %>%
  mutate(m_50 = X) %>%
  rename(m_100 = fit)

B <- ggplot(data = Nvseffect.wide %>%
              filter(!is.na(m_100) & !is.na(m_50)  & site != "Total"),
            aes(x = m_50, y = m_100)) +
  geom_ribbon(data = predictionB,
              aes(ymin = lwr, ymax = upr),color = NA, fill = "grey", alpha = 0.5) +
  geom_line(data = predictionB) +
  geom_point(aes(size = Nlarge)) +
  geom_errorbarh(aes(xmin = CIlow_50, xmax = CIhigh_50)) +
  geom_errorbar(aes(ymin = CIlow_100, ymax = CIhigh_100)) +
  # stat_smooth(method = "lm", color = "black") +

  geom_abline(slope = 1,intercept = 0, color = "black", linetype = 2) +
  theme_bw() +
  labs(x = "",y = "") +
  scale_x_continuous(limits = c(-40,30)) +
  scale_y_continuous(limits = c(-40,30)) +
  coord_equal() +
  theme(text = element_text(size = 24)) +
  guides(size = "none")

B

################################################################################

df.C <- Nvseffect.wide %>%
  filter(!is.na(m_50) & !is.na(m_150)  & site != "Total")
X <- seq(min(df.C$m_50,na.rm = TRUE),
         max(df.C$m_50,na.rm = TRUE),
         length.out = 1000)

LMC <- lm(data = df.B,
          formula = m_150 ~ m_50, weights = Nlarge)
predictionC <- as.data.frame(predict(LMC,
                                     newdata = data.frame(m_50 = X),
                                     interval = "confidence") ) %>%
  mutate(m_50 = X) %>%
  rename(m_150 = fit)

C <- ggplot(data = Nvseffect.wide %>%
              filter(!is.na(m_150) & !is.na(m_50)  & site != "Total"),
            aes(x = m_50, y = m_150)) +
  geom_ribbon(data = predictionC,
              aes(ymin = lwr, ymax = upr),color = NA, fill = "grey", alpha = 0.5) +
  geom_line(data = predictionC) +
  geom_point(aes(size = Nlarge)) +
  geom_errorbarh(aes(xmin = CIlow_50, xmax = CIhigh_50)) +
  geom_errorbar(aes(ymin = CIlow_150, ymax = CIhigh_150)) +
  # stat_smooth(method = "lm", color = "black") +

  geom_abline(slope = 1,intercept = 0, color = "black", linetype = 2) +
  theme_bw() +
  labs(x = "",y = "") +
  scale_x_continuous(limits = c(-40,30)) +
  scale_y_continuous(limits = c(-40,30)) +
  coord_equal() +
  theme(text = element_text(size = 24)) +
  guides(size = "none")

C


plot_grid(A,B,C, align = "hv",nrow = 1)
