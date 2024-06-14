rm(list = ls())

library(sf)
library(dplyr)
library(rgdal)
library(raster)
library(ggplot2)

trees <- rgdal::readOGR("./data/BCI/CM2023/crownmap2022_2023_dbh_canopy.shp")

# plot(trees$newArea,trees$crownArea)
# trees$newArea <- trees$crownArea

spplot(trees, zcol="crown_area")

# # trees.old <- rgdal::readOGR("/home/femeunier/Downloads/CrownMapsPanama/BCI 50ha plot/BCI 50ha 2014 partialfix/BCI_All_Crown_Data_10ha_50ha_partialfix.shp")
# # trees.old.df <- as.data.frame(trees.old)

trees.df <- as.data.frame(trees) %>%
  mutate(Lianas = as.numeric(lianas),
         tag = as.numeric(tag),
         crownArea = crown_area,
         Illuminati = as.numeric(iluminatio),
         DBH =  as.numeric(dbh)/10,
         h = max_canopy) %>%
  mutate(liana.cat = case_when(Lianas == 0 ~ "no",
                               Lianas < 3 ~ 'low',
                               TRUE ~ "high")) %>%
  filter(dead == "no",
         status == "complete")


trees.df  %>%
  # filter(crownArea >= 0.01) %>%
  pull(Illuminati) %>% table()

# All
ggplot(data = trees.df  %>%
         filter(crownArea >= 0.01,
                DBH > 0),
       aes(x = DBH, y = crownArea,
           color = as.factor(liana.cat),
           fill = as.factor(liana.cat)) ) +
  geom_point() +
  stat_smooth(method = "lm",
              se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

DBH.thrshold <- 40
Illuminati.thrshold <- 4
CA.thrshold <- 0.0

ggplot(data = trees.df %>% filter(DBH >= DBH.thrshold,
                                  Illuminati >= Illuminati.thrshold,
                                  !is.na(DBH),!is.na(crownArea),
                                  crownArea >= CA.thrshold),
       aes(x = DBH, y = crownArea,
           color = as.factor(liana.cat),
           fill = as.factor(liana.cat)) ) +
  geom_point(alpha = 0.5,size = 0.5) +
  stat_smooth(method = "lm",
              se = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()


ggplot(data = trees.df %>% filter(DBH >= DBH.thrshold,
                                  Illuminati >= Illuminati.thrshold,
                                  !is.na(DBH),!is.na(crownArea),
                                  crownArea >= CA.thrshold),
       aes(x = DBH, y = h,
           color = as.factor(liana.cat),
           fill = as.factor(liana.cat)) ) +
  geom_point(alpha = 0.5,size = 0.5) +
  stat_smooth(method = "lm",
              se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

nrow(trees.df %>%
       filter(DBH >= DBH.thrshold,
              Illuminati >= Illuminati.thrshold,
              !is.na(DBH),!is.na(crownArea),
              crownArea >= CA.thrshold))

# trees.df %>%
#   filter(DBH >= DBH.thrshold,
#          Illuminati >= Illuminati.thrshold,
#          !is.na(DBH),!is.na(newArea),
#          newArea >= CA.thrshold)%>%
#   group_by(liana.cat) %>%
#   summarise(intercept = coef(lm(formula =  log(newArea) ~ log(DBH)))[1],
#             slope = coef(lm(formula = log(newArea) ~ log(DBH)))[2]) %>%
#   mutate(CA = exp(intercept)*(50**slope))
#
#
saveRDS(trees.df %>%
          filter(DBH >= DBH.thrshold,
                 Illuminati >= Illuminati.thrshold,
                 crownArea >= CA.thrshold),
        "./data/BCI/BCI_CA_2023_Helene.RDS")
