rm(list = ls())

library(BCI.AGB)
library(dplyr)
library(ggplot2)
library(tidyr)

data.file <- "/home/femeunier/Documents/projects/LianaBCI/data/2021-09-22_liana_bci_data_V9_0_felicien.csv"
data <- read.csv(data.file)

threshold <- 3*10 # 1cm = 10mm

data.filtered <- data %>%
  filter(DBH >= threshold)

data.filtered %>% group_by(Census) %>% summarise(Nliving = length(DBH[Status %in% c("L","N")]),
                                                 N_large_living = length(DBH[Status %in% c("L","N") & DBH >= 50]),
                                                 N_very_large = length(DBH[Status %in% c("L","N") & DBH >= 100]),

                                                 DBH_mean = mean(DBH[Status %in% c("L","N")], na.rm = TRUE)/10,
                                                 DBH_large_mean = mean(DBH[Status %in% c("L","N") & DBH >= 50], na.rm = TRUE)/10,

                                                 BA_living = sum(pi*DBH[Status %in% c("L","N")]*DBH[Status %in% c("L","N")]/4)/1000/1000,

                                                 BA_large_living = sum(pi*DBH[Status %in% c("L","N") & DBH >= 50]*DBH[Status %in% c("L","N") & DBH >= 50]/4)/1000/1000,

                                                 .groups = "keep")

Delta_X <- Delta_Y <-
  10

data.patch <- data.filtered %>% filter(PX >= 0 & PX < 1000 &
                                         PY >= 0 & PY < 500,
                                       Status %in% c("L","N")) %>% ungroup() %>%
  mutate(patch = LianaBCI::patchnumber_from_position(PX,PY,
                                                     patch_X = Delta_X,patch_Y = Delta_Y,
                                                     extr_x = c(0,1000),
                                                     extr_y = c(0,500))[["patch"]])

patches <- sort(unique(data.patch$patch))
Npatches <- length(patches)


data.LAI <- data.patch %>%
  mutate(BA = DBH*DBH*pi/4/100) %>%
  mutate(LA = pmax(0,(BA*0.109 - 0.376)*5)) # Assuming SLA = 10 m²/kgC = 5 m²/kgbiomass

data.LI <- data.LAI %>%
  group_by(patch) %>%
  summarise(BA.tot = sum(BA),
            LI = pmin(1,sum(LA)/Delta_X/Delta_Y)) %>%
  complete(patch = 1:(50*100*100/Delta_X/Delta_Y),
           fill = list(BA.tot = 0,
                       LI = 0)) %>%
  mutate(coi = case_when(LI < 0.05 ~ 0,
                         LI < 0.25 ~ 1,
                         LI < 0.5 ~ 2,
                         LI < 0.75 ~ 3,
                         TRUE ~ 4)) %>%
  mutate(liana.cat = factor(case_when(coi == 0 ~ "no",
                                      coi <= 2 ~ "low",
                                      coi <= 4 ~ "high"),
                            levels = c("no","low","high")))

summary((data.LI$LI))
ggplot(data = data.LI) +
  geom_bar(aes(x = as.factor(liana.cat))) +
  theme_bw()


ggplot(data = data.LI) +
  geom_jitter(aes(x = BA.tot, y = liana.cat)) +
  scale_x_log10() +
  theme_bw()

################################################################################
# Load tree data

HBmodel.liana <- readRDS("./outputs/Fit.BCI.weibull_a.RDS")
HBmodel.null <- readRDS("./outputs/Fit.BCI.weibull_none.RDS")

load("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/bci.spptable.rdata")

c.liana <- as.numeric(exp(summary(HBmodel.liana)[["spec_pars"]][1]**2/2))
c.null <- as.numeric(exp(summary(HBmodel.null)[["spec_pars"]][1]**2/2))

df.BCI.f <- tree.BCI %>%
  filter(census.time == 2015,
         status %in% c("A","AM","AD","AR"),
         gx >= 0 & gx < 1000,
         gy >= 0 & gy < 500,
         !is_liana) %>%
  mutate(patch = LianaBCI::patchnumber_from_position(gx,gy,
                                                     patch_X = Delta_X,patch_Y = Delta_Y,
                                                     extr_x = c(0,1000),
                                                     extr_y = c(0,500))[["patch"]]) %>%
  left_join(data.LI,
            by = "patch") %>%
  filter(dbh >= 10*10) %>%
  left_join(bci.spptable %>%
              dplyr::select(sp,Latin),
            by = "sp") %>%
  mutate(sp = Latin) %>%
  dplyr::select(-Latin)

df.BCI.f.height <- df.BCI.f %>%
  mutate(h.liana = c.liana *
           exp(predict(HBmodel.liana,newdata = data.frame(dbh = dbh/10,
                                                          sp = sp,
                                                          liana.cat = liana.cat),
                       allow_new_levels = TRUE)[,1]),
         h.null =   c.null *
           exp(predict(HBmodel.null,newdata = data.frame(dbh = dbh/10,
                                                         sp = sp,
                                                         liana.cat = liana.cat),
                       allow_new_levels = TRUE)[,1])
  ) %>%
  mutate(diff = h.liana - h.null,
         diff.rel = diff/h.null)

ggplot(data = df.BCI.f.height) +
  geom_point(aes(x = h.null,
                 y = h.liana, color = liana.cat)) +
  geom_abline(slope = 1, intercept = 0) +
  # geom_point(aes(x = dbh/10,
  #                y = h.null), shape = 1,
  #            color = "black") +
  scale_x_log10() + scale_y_log10() +
  theme_bw()

 bvcplot(df.BCI.f.height$h.null,df.BCI.f.height$h.liana,
     ylim = c(0,60),
     xlim = c(0,60))
abline(a = 0, b = 1, col = "red")
# lines(df.BCI.f.height$dbh/10,df.BCI.f.height$h.null, col = "red", type = "p")

ggplot(data = df.BCI.f.height) +
  geom_boxplot(aes(x = liana.cat, y = diff.rel)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw()

rho = 0.45

df.BCI.f.agb <- df.BCI.f.height %>%
  mutate(agb.liana = 0.0673*(rho*((dbh/10)**2)*h.liana)**0.976,
         agb.null = 0.0673*(rho*((dbh/10)**2)*h.null)**0.976) %>%
  mutate(diff.agb = agb.liana - agb.null,
         diff.agb.rel = diff.agb/agb.null)


plot.sum <- df.BCI.f.agb %>%
  group_by(liana.cat,patch) %>%
  # ungroup() %>%
  summarise(BA.tot = unique(BA.tot),
            coi = unique(coi),
            agb.tot.liana = sum(agb.liana,na.rm = TRUE)/2/Delta_X/Delta_Y,
            agb.tot.null = sum(agb.null,na.rm = TRUE)/2/Delta_X/Delta_Y,
            .groups = "keep") %>%
  ungroup() %>%
  complete(patch = 1:(50*100*100/Delta_X/Delta_Y),
           fill = list(liana.cat = "no",
                       agb.tot.liana = 0,
                       agb.tot.null = 0)) %>%
  mutate(diff.agb = agb.tot.liana - agb.tot.null,
         diff.agb.rel = diff.agb/agb.tot.null)

table(plot.sum %>%
        filter(agb.tot.null > 0) %>% pull(liana.cat))

ggplot(data = plot.sum %>%
         filter(agb.tot.null > 0)) +
  geom_boxplot(aes(x = liana.cat,
                   y = diff.agb.rel)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw()

summary(plot.sum %>%
          filter(agb.tot.null > 0) %>% pull(diff.agb.rel))

ggplot(data = plot.sum,
       aes(x = BA.tot, y = diff.agb.rel)) +
  geom_point() +
  scale_x_log10() +
  stat_smooth(method = "lm") +
  geom_hline(yintercept = 0, color = "red") +
  theme_bw()


-(mean(plot.sum$agb.tot.null) - mean(plot.sum$agb.tot.liana))/mean(plot.sum$agb.tot.null)



