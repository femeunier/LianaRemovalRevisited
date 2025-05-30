rm(list = ls())

library(sf)
library(dplyr)
library(rgdal)
library(raster)
library(ggplot2)

trees2 <- rgdal::readOGR("/home/femeunier/Downloads/CrownMapsPanama/BCI 50ha plot/BCI 50ha 2020/Crowns_2020_08_01_MergedWithPlotData.shp")

trees <- rgdal::readOGR("/home/femeunier/Downloads/CrownMapsPanama/BCI 50ha plot/BCI 50ha 2020 wcensusinfo/Crowns_2020_08_01_FullyMergedWithPlotData.shp")
trees$newArea <- area(trees)

# plot(trees$newArea,trees$crownArea)
# trees$newArea <- trees$crownArea

spplot(trees, zcol="crownArea")

# # trees.old <- rgdal::readOGR("/home/femeunier/Downloads/CrownMapsPanama/BCI 50ha plot/BCI 50ha 2014 partialfix/BCI_All_Crown_Data_10ha_50ha_partialfix.shp")
# # trees.old.df <- as.data.frame(trees.old)

trees.df <- as.data.frame(trees) %>%
  mutate(Lianas = as.numeric(Lianas),
         tag = as.numeric(tag),
         crownArea = newArea,
         Illuminati = as.numeric(Illuminati),
         DBH =  as.numeric(dbh)/10) %>%
  mutate(liana.cat = case_when(Lianas == 0 ~ "no",
                               Lianas < 3 ~ 'low',
                               TRUE ~ "high")) %>%
  filter(status == "A")


trees.df  %>%
  # filter(crownArea >= 0.01) %>%
  pull(Illuminati) %>% table()

# All
ggplot(data = trees.df  %>%
         filter(crownArea >= 0.01),
       aes(x = DBH, y = crownArea, color = as.factor(liana.cat)) ) +
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
                                  !is.na(DBH),!is.na(newArea),
                                  newArea >= CA.thrshold),
       aes(x = DBH, y = newArea, color = as.factor(liana.cat)) ) +
  geom_point(alpha = 0.5,size = 0.5) +
  stat_smooth(method = "lm",
              se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

nrow(trees.df %>%
       filter(DBH >= DBH.thrshold,
              liana.cat == "high",
              Illuminati >= Illuminati.thrshold,
              !is.na(DBH),!is.na(newArea),
              newArea >= CA.thrshold))

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
                 newArea >= CA.thrshold),
        "./data/BCI/BCI_CA_2020_Helene.RDS")
