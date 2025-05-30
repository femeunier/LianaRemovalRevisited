rm(list = ls())

library(dplyr)
library(tidyr)
library(raster)
library(ggplot2)

# raw <-  readRDS("/home/femeunier/Documents/data/monthly.climate.global.ERA5.RDS")
# climate <- raw %>%
#   ungroup() %>%
#   filter(year %in% c(1981:2010),
#          abs(lat) <= 30)
#
# saveRDS(climate,
#         "/home/femeunier/Documents/data/monthly.climate.pantropical.ERA5.years.RDS")

# climate <- readRDS("/home/femeunier/Documents/data/monthly.climate.pantropical.ERA5.years.RDS")

# climate.long <- climate %>%
#   # dplyr::select(-lon.lat) %>%
#   pivot_longer(cols = -c(lon,lat,month,year),
#                names_to = "variable")

# climate.month <- readRDS("/home/femeunier/Documents/data/seasonal.ERA5.pantropics.RDS")
#
# vars <- unique(climate.month$variable)
# vars <- vars[!(vars %in% c("month","lat","lon","year"))]
#
# # climate.month <- climate.long %>%
# #   group_by(lon,lat,variable,month) %>%
# #   summarise(m = mean(value,
# #                      na.rm = TRUE),
# #             .groups = "keep")
#
# climate.month.wide <- climate.month %>%
#   pivot_wider(names_from = "variable",
#               values_from = "m")
#
# for (cvar in vars){
#   cstack <- stack()
#
#   print(cvar)
#   for (imonth in seq(1,12)){
#
#     cdf <- climate.month.wide %>%
#       filter(month == imonth) %>%
#       ungroup() %>%
#       dplyr::select(lon,lat,!!cvar)
#     craster <- rasterFromXYZ(cdf)
#
#     cstack <- stack(cstack,
#                     craster)
#   }
#
#   writeRaster(cstack,
#               paste0("./outputs/ERA5/",cvar,".tif"))
# }
#
# saveRDS(climate.month,
#         "/home/femeunier/Documents/data/seasonal.ERA5.pantropics.RDS")
#
# climate.month <- readRDS("/home/femeunier/Documents/data/seasonal.ERA5.pantropics.RDS")
#
# climate.month.sum <- climate.month %>%
#   group_by(month,variable) %>%
#   summarise(m = mean(m,
#                      na.rm = TRUE),
#             .groups = "keep")
#
# ggplot(data = climate.month.sum) +
#   geom_line(aes(x = month, y = m)) +
#   facet_wrap(~ variable,scales = "free") +
#   theme_bw()


################################################################################
# CRUJRA

climate <- readRDS("~/Downloads/monthly.climate.pantropical.CRUJRA.RDS")

climate.month <- climate %>%
  filter(year %in% c(1981:2010)) %>%
  pivot_longer(cols = -c(year,month,lon,lat),
               names_to = "variable",
               values_to = "value") %>%
  group_by(lon,lat,month,variable) %>%
  summarise(value = mean(value,na.rm = TRUE),
            .groups = "keep")

vars <- unique(climate.month$variable)
vars <- vars[!(vars %in% c("month","lat","lon","year"))]

# climate.month <- climate.long %>%
#   group_by(lon,lat,variable,month) %>%
#   summarise(m = mean(value,
#                      na.rm = TRUE),
#             .groups = "keep")

climate.month.wide <- climate.month %>%
  pivot_wider(names_from = "variable",
              values_from = "value")

for (cvar in vars){
  cstack <- stack()

  print(cvar)
  for (imonth in seq(1,12)){

    cdf <- climate.month.wide %>%
      filter(month == imonth) %>%
      ungroup() %>%
      dplyr::select(lon,lat,!!cvar)
    craster <- rasterFromXYZ(cdf)

    cstack <- stack(cstack,
                    craster)
  }

  writeRaster(cstack,
              paste0("./outputs/CRUJRA/",cvar,".tif"),overwrite = TRUE)
}

saveRDS(climate.month,
        "/home/femeunier/Documents/data/seasonal.CRUJRA.pantropics.RDS")

climate.month.sum <- climate.month %>%
  group_by(month,variable) %>%
  summarise(m = mean(value,
                     na.rm = TRUE),
            .groups = "keep")

ggplot(data = climate.month.sum) +
  geom_line(aes(x = month, y = m)) +
  facet_wrap(~ variable,scales = "free") +
  theme_bw()


