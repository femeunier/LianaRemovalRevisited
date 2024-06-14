rm(list = ls())

library(sf)
library(dplyr)
library(rgdal)
library(raster)
library(ggplot2)

# trees <- rgdal::readOGR("/home/femeunier/Downloads/CrownMapsPanama/BCI 50ha plot/BCI 50ha 2020 wcensusinfo/Crowns_2020_08_01_FullyMergedWithPlotData.shp")
# trees$newArea <- area(trees)
# # trees$newArea <- trees$crownArea
#
# spplot(trees, zcol="crownArea")
#
# # trees.old <- rgdal::readOGR("/home/femeunier/Downloads/CrownMapsPanama/BCI 50ha plot/BCI 50ha 2014 partialfix/BCI_All_Crown_Data_10ha_50ha_partialfix.shp")
# # trees.old.df <- as.data.frame(trees.old)
#
# trees.df <- as.data.frame(trees) %>%
#   mutate(Lianas = as.numeric(Lianas),
#          tag = as.numeric(tag),
#          Illuminati = as.numeric(Illuminati),
#          DBH =  as.numeric(dbh)/10) %>%
#   mutate(liana.cat = case_when(Lianas == 0 ~ "no",
#                                Lianas < 3 ~ 'low',
#                                TRUE ~ "high")) %>%
#   filter(status == "A")
#   # left_join(trees.old.df %>%
#   #             dplyr::select(tag,MaxHt),
#   #           by = "tag") %>%
#
# plot(trees.df$newArea,trees.df$crownArea)
#
# # ggplot(data = trees.df  %>% filter(crownArea >= 0.01),
# #        aes(x = DBH, y = MaxHt, color = as.factor(liana.cat)) ) +
# #   geom_point() +
# #   stat_smooth(method = "lm",
# #               se = FALSE) +
# #   # scale_x_log10() +
# #   # scale_y_log10() +
# #   theme_bw()
#
# DBH.thrshold <- 40
# Illuminati.thrshold <- 5
# CA.thrshold <- 0.01
#
# ggplot(data = trees.df %>% filter(DBH >= DBH.thrshold,
#                                   Illuminati >= Illuminati.thrshold,
#                                   !is.na(DBH),!is.na(newArea),
#                                   newArea >= CA.thrshold),
#        aes(x = DBH, y = newArea, color = as.factor(liana.cat)) ) +
#   geom_point(alpha = 0.5,size = 0.5) +
#   stat_smooth(method = "lm",
#               se = FALSE) +
#   scale_x_log10() +
#   scale_y_log10() +
#   theme_bw()
#
# nrow(trees.df %>%
#        filter(DBH >= DBH.thrshold,
#               Illuminati >= Illuminati.thrshold,
#               !is.na(DBH),!is.na(newArea),
#               newArea >= CA.thrshold))
#
# trees.df %>%
#   filter(DBH >= DBH.thrshold,
#          Illuminati >= Illuminati.thrshold,
#          !is.na(DBH),!is.na(newArea),
#          newArea >= CA.thrshold)%>%
#
#   group_by(liana.cat) %>%
#   summarise(intercept = coef(lm(formula =  log(newArea) ~ log(DBH)))[1],
#             slope = coef(lm(formula = log(newArea) ~ log(DBH)))[2]) %>%
#   mutate(CA = exp(intercept)*(50**slope))
#
#
# saveRDS(trees.df %>%
#           filter(DBH >= DBH.thrshold,
#                  Illuminati >= Illuminati.thrshold,
#                  newArea >= CA.thrshold),
#         "./data/BCI/BCI_CA_2020_Helene.RDS")


trees <- rgdal::readOGR("/home/femeunier/Downloads/CrownMapsPanama/BCI 50ha plot/BCI 50ha 2014 partialfix/BCI_All_Crown_Data_10ha_50ha_partialfix.shp")
# trees$newArea <- area(trees)
# trees$newArea <- trees$crownArea

spplot(trees, zcol="Area")

df.trees <- as.data.frame(trees)
trees.df <- df.trees  %>%
  mutate(Lianas = as.numeric(Liana_dend),
         Illuminati = as.numeric(luz_dendro),
         DBH =  as.numeric(dbh_census)/10) %>%
  mutate(liana.cat = case_when(Lianas == 0 ~ "no",
                               Lianas < 3 ~ 'low',
                               TRUE ~ "high")) %>%
  filter(status_cen == "alive",
         Illuminati > 0)

ggplot(data = trees.df,
       aes(x = DBH, y = MaxHt, color = as.factor(liana.cat)) ) +
  geom_point() +
  stat_smooth(method = "lm",
              se = FALSE) +
  # scale_x_log10() +
  # scale_y_log10() +
  theme_bw()

DBH.thrshold <- 0
Illuminati.thrshold <- 5
CA.thrshold <- 0.0

ggplot(data = trees.df %>% filter(DBH >= DBH.thrshold,
                                  Illuminati >= Illuminati.thrshold,
                                  !is.na(DBH),!is.na(Area),
                                  Area >= CA.thrshold),
       aes(x = DBH, y = Area, color = as.factor(liana.cat),
           fill = as.factor(liana.cat))) +
  geom_point(alpha = 0.5,size = 0.5) +
  stat_smooth(method = "lm",
              se = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ Illuminati) +
  theme_bw()

nrow(trees.df %>%
       filter(DBH >= DBH.thrshold,
              Illuminati >= Illuminati.thrshold,
              !is.na(DBH),!is.na(Area),
              Area >= CA.thrshold))

trees.df %>%
  filter(DBH >= DBH.thrshold,
         Illuminati >= Illuminati.thrshold,
         !is.na(DBH),!is.na(Area),
         Area >= CA.thrshold)%>%

  group_by(liana.cat) %>%
  summarise(intercept = coef(lm(formula =  log(Area) ~ log(DBH)))[1],
            slope = coef(lm(formula = log(Area) ~ log(DBH)))[2]) %>%
  mutate(CA = exp(intercept)*(50**slope))


stop()
saveRDS(trees.df %>%
          filter(DBH >= DBH.thrshold,
                 Illuminati >= Illuminati.thrshold,
                 Area >= CA.thrshold),
        "./data/BCI/BCI_CA_2015_Helene.RDS")

df.trees  %>%
  mutate(Lianas = as.numeric(Liana_dend),
         Illuminati = as.numeric(luz_dendro),
         DBH =  as.numeric(dbh_census)/10) %>%
  mutate(liana.cat = case_when(Lianas == 0 ~ "no",
                               Lianas < 3 ~ 'low',
                               TRUE ~ "high")) %>%
  filter(status_cen == "alive" & Illuminati == 5)
