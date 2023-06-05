rm(list = ls())

library(dplyr)
library(ggplot2)
library(BCI.AGB)
library(tidyr)
library(raster)

source("http://chave.ups-tlse.fr/pantropical_allometry/readlayers.r")

data.file <- "/home/femeunier/Documents/projects/LianaBCI/data/2021-09-22_liana_bci_data_V9_0_felicien.csv"
data <- read.csv(data.file)

threshold <- 1*10 # 1cm = 10mm
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

Delta_X <- Delta_Y <- 20
data.patch <- data.filtered %>% filter(PX >= 0 & PX < 1000 &
                                         PY >= 0 & PY < 500,
                                       Status %in% c("L","N")) %>% ungroup() %>%
  mutate(patch = LianaBCI::patchnumber_from_position(PX,PY,
                                                     patch_X = Delta_X,patch_Y = Delta_Y,
                                                     extr_x = c(0,1000),
                                                     extr_y = c(0,500))[["patch"]])

patches <- sort(unique(data.patch$patch))
Npatches <- length(patches)

Npatch.X = 1000/Delta_X; N.patch.Y = 500/Delta_Y

data.patch.sum <- data.patch %>%
  filter(Status %in% c("L","N"),
         !is.na(DBH)) %>%
  group_by(Census,patch) %>%
  summarise(N = length(DBH)/Delta_X/Delta_Y,  # #/m²
            DBH.max = max(DBH),
            BA = sum(DBH*DBH,na.rm = TRUE)*pi/4/100/Delta_X/Delta_Y, #cm²/m²
            .groups = "keep") %>%
  ungroup() %>%
  complete(patch = 1:1250,
           Census = 1:2,
           fill = list(N = 0,
                       BA = 0,
                       DBH.max = 0)) %>%
  mutate(patch.x = patch - (((patch-1) %/% Npatch.X)*Npatch.X),
         patch.y = 1 + ((patch-1) %/% Npatch.X)) %>%
  mutate(x = -Delta_X/2 + patch.x*Delta_X,
         y = -Delta_Y/2 + patch.y*Delta_Y)

N2keep <- 25
data.patch.sum.wide <- data.patch.sum %>%
  pivot_wider(names_from = c(Census),
              values_from = -c(patch,Census)) %>%
  mutate(N.mean.geom = sqrt(N_1*N_2),
         BA.mean.geom = sqrt(BA_1*BA_2)) %>%
  mutate(liana.cat = case_when(BA.mean.geom >= sort(BA.mean.geom)[Npatches-N2keep] ~ "high",
                               BA.mean.geom <= sort(BA.mean.geom)[N2keep] ~ "no",
                          TRUE ~ "other"))

patch2keep <- data.patch.sum.wide %>%
  filter(liana.cat != "other") %>%
  pull(patch)

hist(data.patch.sum.wide$DBH.max_2 -
       data.patch.sum.wide$DBH.max_1)

hist(data.patch.sum.wide$BA.mean.geom)


data.patch.sum.2.plot <- data.patch.sum %>%
  left_join(data.patch.sum.wide %>%
              dplyr::select(patch,liana.cat),
            by = c("patch"))

data.ind.2.plot <- data.patch %>%
  left_join(data.patch.sum.wide %>%
              dplyr::select(patch,liana.cat),
            by = c("patch"))

ggplot(data = data.patch.sum.2.plot %>%
         filter(liana.cat != "other")) +
  geom_tile(aes(x = x, y = y,
                fill = as.factor(liana.cat)), color = "black") +
  geom_point(data = data.ind.2.plot %>%
               filter(liana.cat != "other"),
             aes(x = PX, y = PY,
                 size = (pi/4*(DBH/10)**2/10000))) +
  facet_wrap(~Census) +
  theme_bw()



df.BCI.f <- tree.BCI %>%
  filter(census.time >= 1990,
         status %in% c("A","AM","AD","AR"),
         gx >= 0 & gx < 1000,
         gy >= 0 & gy < 500, !is_liana) %>%
  mutate(patch = LianaBCI::patchnumber_from_position(gx,gy,
                                                     patch_X = Delta_X,patch_Y = Delta_Y,
                                                     extr_x = c(0,1000),
                                                     extr_y = c(0,500))[["patch"]])

rho = 0.45

longitude=-79.8553;latitude=9.15125
coord <- cbind(longitude,latitude)
# E <- retrieve_raster("E",coord)

# Chave equation for BCI
E <- 0.04635461

# Best HB model for BCI
HBmodel.liana <- readRDS("./outputs/Fit.BCI.weibull_a.RDS")
HBmodel.ref <- readRDS("./outputs/Fit.BCI.weibull_none.RDS")

load("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/bci.spptable.rdata")

trees2keep <- df.BCI.f %>%
  left_join(bci.spptable %>%
              dplyr::select(sp,Latin),
            by = "sp") %>%
  mutate(sp = Latin) %>%
  dplyr::select(-Latin) %>%

  dplyr::filter(patch %in% patch2keep) %>%
  left_join(data.patch.sum.wide %>%
              dplyr::select(patch,liana.cat),
            by = c("patch")) %>%
  mutate(h.ref = exp(0.893 - E +0.76*log(dbh/10) - 0.0340*(log(dbh/10))**2))


df2predict <- trees2keep %>%
  filter(!is.na(dbh)) %>%
  dplyr::select(dbh,sp,liana.cat) %>%
  distinct()

N = dim(df2predict)[1] ; step = 1000 ; Nloop <- (ceiling(N/step))

c.mod <- as.numeric(exp(summary(HBmodel.liana)[["spec_pars"]][1]/2))
c.ref <- as.numeric(exp(summary(HBmodel.ref)[["spec_pars"]][1]/2))

hmod <- href <-
  c()

for (i in seq(1,Nloop)){

  print(i/Nloop)
  count <- ((i-1)*step + 1 ):min(N,(i*step))

  hmod[count] <-
    c.mod *
    exp(predict(HBmodel.liana,newdata = data.frame(dbh = df2predict$dbh[count]/10,
                                                   sp = df2predict$sp[count],
                                                   liana.cat = df2predict$liana.cat[count]),
                allow_new_levels = TRUE)[,1])


  href[count] <-
    c.ref *
    exp(predict(HBmodel.ref,newdata = data.frame(dbh = df2predict$dbh[count]/10,
                                                 sp = df2predict$sp[count],
                                                 liana.cat = df2predict$liana.cat[count]),
                allow_new_levels = TRUE)[,1])

}

df2predict[["hmod"]] <- hmod
df2predict[["href"]] <- href

ggplot(data = df2predict) +
  geom_point(aes(x = dbh,y = href), color = "black") +
  geom_point(aes(x = dbh,y = hmod, shape = as.factor(liana.cat)), color = "red") +
  theme_bw()

trees2keep.agb <- trees2keep %>%
  left_join(df2predict,
            by = c("sp","dbh","liana.cat")) %>%
  mutate(agb.chave = 0.0673*(rho*((dbh/10)**2)*h.ref)**0.976,
         agb.ref = 0.0673*(rho*((dbh/10)**2)*href)**0.976,
         agb.mod = 0.0673*(rho*((dbh/10)**2)*hmod)**0.976)


plot(trees2keep.agb$agb.mod,trees2keep.agb$agb.chave, log = 'xy')
abline(a = 0, b = 1, col = "red",lty = 2)

trees2keep.sum <- trees2keep.agb %>%
  group_by(census.time,liana.cat,patch) %>%
  summarise(agb.chave = sum(agb.chave,na.rm = TRUE)/Delta_X/Delta_Y/2,    # kgC/m²
            agb.mod = sum(agb.mod,na.rm = TRUE)/Delta_X/Delta_Y/2, # kgC/m²
            agb.ref = sum(agb.ref,na.rm = TRUE)/Delta_X/Delta_Y/2, # kgC/m²
            .groups = "keep") %>%
  ungroup()


trees2keep.sum.m.long <-
  trees2keep.sum %>%
  pivot_longer(cols = -c(census.time,liana.cat,patch),
               names_to = "var",
               values_to = "value") %>%
  mutate(metric = sub("\\..*","",var),
         scenario = sub(".*\\.","",var)) %>%
  dplyr::select(-var) %>%
  pivot_wider(names_from = metric,
              values_from = value) %>%
  mutate(scenario = relevel(as.factor(scenario),
                            ref = "ref"))


patch2keep <- trees2keep.sum.m.long %>%
  group_by(patch) %>%
  summarise(keep = mean(agb) < 20) %>%
  filter(keep) %>%
  pull(patch)

ggplot(data = trees2keep.sum.m.long %>%
         filter(patch %in% patch2keep),
       aes(x = census.time, y = agb,
           group = interaction(as.factor(patch),liana.cat),
           color = liana.cat)) +
  facet_wrap(~ scenario) +
  geom_line() +
  theme_bw()

trees2keep.sum.m <-
  trees2keep.sum %>%
  filter(patch %in% patch2keep) %>%
  group_by(census.time,liana.cat) %>%
  summarise(agb.m_ref = mean(agb.ref),
            agb.sd_ref = sd(agb.ref),
            agb.se_ref = 1.96*sd(agb.ref)/sqrt(length(agb.ref)),

            agb.m_chave = mean(agb.chave),
            agb.sd_chave = sd(agb.chave),
            agb.se_chave = 1.96*sd(agb.chave)/sqrt(length(agb.chave)),

            agb.m_mod = mean(agb.mod),
            agb.sd_mod = sd(agb.mod),
            agb.se_mod = 1.96*sd(agb.mod)/sqrt(length(agb.mod)),

            .groups = "keep")

trees2keep.sum.m.long <-
  trees2keep.sum.m %>%
  pivot_longer(cols = -c(census.time,liana.cat),
               names_to = "var",
               values_to = "value") %>%
  mutate(metric = sub("\\_.*","",var),
         scenario = sub(".*\\_","",var)) %>%
  dplyr::select(-var) %>%
  pivot_wider(names_from = metric,
              values_from = value) %>%
  mutate(scenario = relevel(as.factor(scenario),
                           ref = "ref"))

ggplot(data = trees2keep.sum.m.long,
       aes(x = census.time, y = agb.m,
           fill = liana.cat, color = liana.cat)) +
  geom_line() +
  geom_ribbon(aes(ymin = agb.m - agb.se, ymax = agb.m + agb.se),
              alpha = 0.5, color = NA) +
  stat_smooth(method = "lm", se = FALSE, linetype = 2) +
  facet_wrap(~ scenario) +
  theme_bw()


