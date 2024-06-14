rm(list = ls())

library(ggridges)
library(ggplot2)
library(dplyr)
################################################################################
# Multiple sites

data.BCI <- readRDS("./data/BCI/BCI_CA_2023_Helene.RDS") %>%
  rename(sp = spcode,
         h = MaxHt,
         dbh =  DBH,
         area = Area) %>%
  dplyr::select(sp,dbh,h,area,liana.cat) %>%
  mutate(site = "BCI")


load("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/bci.spptable.rdata")

df.match <- data.frame(sp = unique(data.BCI$sp)) %>%
  left_join(bci.spptable %>%
              dplyr::select(sp,Latin),
            by = "sp") %>%
  mutate(sp.final = case_when(is.na(Latin) ~ sp,
                              TRUE ~ Latin)) %>%
  dplyr::select(sp,sp.final)

data.BCI <- data.BCI %>%
  left_join(df.match,
            by = "sp") %>%
  mutate(sp = sp.final) %>%
  dplyr::select(-sp.final)

saveRDS(data.BCI,"./outputs/BCI.h.CA.RDS")


data.begum <- read.csv("./data/Begum/tree_frontiers_269.csv") %>%
  rename(coi = COI,
         dbh = diam,
         h = Height,
         sp = sp_tree,
         area = area_tot) %>%
  mutate(liana.cat = case_when(coi %in% c(0) ~ "no",
                               coi %in% c(1,2) ~ "low",
                               TRUE ~ "high")) %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high"))) %>%
  dplyr::select(dbh,h,sp,area,liana.cat) %>%
  mutate(site = "Loundoungou")

saveRDS(data.begum,"./outputs/Loundoungou.h.CA.RDS")

data.DV <- readRDS("./data/Danum/DV.processed.RDS") %>%
  rename(coi = COI) %>%
  dplyr::select(c(dbh,area,sp,liana.cat)) %>%
  mutate(site = "Danum Valley")

data.all <- bind_rows(
  data.BCI,
  data.begum,
  data.DV) %>%
  filter(dbh >= 10) %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high")),
         site = factor(site,
                       levels = c("BCI","Loundoungou","Danum Valley")))

ggplot(data.all,
       aes(x = dbh, y = area,
           color = as.factor(liana.cat),
           fill = as.factor(liana.cat))) +
  geom_point() +
  stat_smooth(method = "lm", se = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ site) +
  theme_bw()

data.all %>%
  group_by(site, liana.cat) %>%
  summarise(m = mean(area))

data.all %>%
  group_by(site) %>%
  summarise(m = mean(area),
            N = n())

ggplot(data = data.all) +
  geom_density_ridges(aes(x = area,fill = liana.cat, y = site),
                      alpha = 0.5) +
  scale_x_log10() +
  theme_bw()

ggplot(data = data.all) +
  geom_density_ridges(aes(x = dbh,fill = liana.cat, y = site),
                      alpha = 0.5) +
  scale_x_log10() +
  theme_bw()

saveRDS(data.all,
        "./outputs/All.CA.data.RDS")

################################################################################
# BCI comparison

ch.correction <- readRDS("./data/ch.data.correction.RDS")

BCI.CA.df <- bind_rows(
  readRDS("./data/BCI/BCI_CA_2015_Helene.RDS") %>%
    dplyr::select(tag,spcode,DBH,Area,liana.cat) %>%
    rename(dbh = DBH,
           CA = Area,
           sp = spcode,
           Tag = tag) %>%
    mutate(year = 2015) %>%
    left_join(bci.spptable %>%
                dplyr::select(sp,Latin),
              by = "sp") %>%
    mutate(sp = Latin) %>%
    dplyr::select(-Latin),

 read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/2019_TLS_data.csv",stringsAsFactors = FALSE) %>%
  dplyr::select(Tag,Species,Final.DBH..cm.,CPA..m2.,Liana) %>%
   left_join(ch.correction %>%
               dplyr::select(tag,charea) %>%
               rename(Tag = tag),
             by = "Tag") %>%
  mutate(year = 2019) %>%
  rename(dbh = Final.DBH..cm.,
         CA = charea,                     # Convex hull correction!
         Lianas = Liana,
         sp = Species) %>%
    mutate(liana.cat = case_when(Lianas == 0 ~ "no",
                                 Lianas == 1 ~ "low",
                                 Lianas == 2 ~ "high")) %>%
    dplyr::select(-c(Lianas,CPA..m2.)))

ggplot(data = BCI.CA.df,
       aes(x = dbh, y = CA,
           color = liana.cat, fill = liana.cat)) +
  geom_point() +
  stat_smooth(method = "lm",
              se = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ year) +
  theme_bw()


saveRDS(BCI.CA.df,
        "./outputs/BCI.CA.data.RDS")

# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/outputs/All.CA.data.RDS hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/
# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/outputs/BCI.CA.data.RDS hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/

