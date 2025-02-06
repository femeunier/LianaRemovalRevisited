rm(list = ls())

library(xlsx)
library(ggplot2)
library(dplyr)
library(raster)
library(ggrepel)

plots <- c("ALF_01","ALF_02",
           "FLO_01","FLO_02",
           "FRP_01",
           "GAU_02","GAU_04","GAU_05","GAU_06","GAU_07",
           "PEA_01","PEA_02","PEA_03","PEA_04","PEA_05","PEA_06","PEA_07","PEA_08",
           "POA_01",
           "SAA_01","SAA_02",
           "SAT_01",
           "SIP_01",
           "TAN_02","TAN_03","TAN_04",
           "VCR_02")

lats <- c(-9.6,-9.58,
          -12.81,-12.75,
          -11.48,
          -13.43,-13.1,-12.98,-13.31,-13.1,
          -12.15,-12.32,-12.38,-12.42,-11.9,-11.92,-12.48,-12.54,
          -10.96,
          -9.79,-9.64,
          -9.84,
          -11.41,
          -13.08,-12.83,-12.92,
          -14.83)

lons <- c(-55.94,-55.92,
          -51.85,-51.88,
          -51.52,
          -53.31,-53.35,-52.92,-53.41,-53.35,
          -50.83,-50.74,-50.89,-50.71,-50.75,-50.71,-50.9,-50.74,
          -52.17,
          -50.43,-50.45,
          -50.46,
          -55.32,
          -52.38,-52.35,-52.37,
          -52.17)



last.census <- c(6,5,
                 6,5,
                 3,
                 3,3,1,3,2,
                 3,3,2,2,2,2,2,2,
                 3,
                 5,4,
                 4,
                 3,
                 6,5,5,
                 7)



Dir <- "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/ForestPlots/data/"

df.all <- data.frame()

for (iplot in seq(1,length(plots))){

  print(iplot/length(plots))
  clast.census <- last.census[iplot]

  for (icensus in seq(1,(clast.census))){

    file.name <- file.path(Dir,
                           paste0(plots[iplot],
                                  "_Census_",icensus,"_PlotDump.xlsx"))
    if (file.exists(file.name)){

      data <- tryCatch(xlsx::read.xlsx(file.name,
                              sheetName = "Data"),
                       error = function(e){
                                return(NULL)
                              })
      if (is.null(data)){
        print(paste("Error reading this site:",plots[iplot]))
        next()
      }

      data.mut <- data %>%
        # filter(F2 != 1) %>%
        mutate(COI = as.numeric(LI),
               h = as.numeric(Height),
               DBH = D4/10) %>%
        mutate(liana.cat = case_when(COI == 0 ~ "no",
                                     COI < 3 ~ "low",
                                     COI < 5 ~ "high",
                                     TRUE ~ NA)) %>%
        filter(!is.na(DBH),!is.na(liana.cat),
               !is.na(h))

      df.all <- bind_rows(list(df.all,
                               data.mut %>%
                                 dplyr::select(TreeID, Tag.No,DBH,liana.cat,COI,h,Species) %>%
                                 mutate(site = plots[iplot],
                                        lat = lats[iplot],
                                        lon = lons[iplot],
                                        census = icensus,
                                        last.census = clast.census)))
    }
  }
}

saveRDS(df.all,
        './data/rainfor.loc.RDS')

df.all2 <- df.all %>%
  group_by(site,TreeID) %>%
  mutate(N = n()) %>%
  arrange(desc(census)) %>%
  slice_head(n = 1) %>% # We only keep the most recent census
  mutate(group = substr(site,1,3)) %>%
  group_by(group) %>%
  mutate(group.N = paste0(group,", N = ",length(DBH)))


df.all2

print(c(nrow(df.all),
        nrow(df.all2),
        nrow(df.all %>% filter(census == last.census))))

df.all.filt <- df.all2 %>%
  filter(h > 0) %>%
  filter(!(site %in% c("GAU_02","PEA_07","PEA_08"))) # no high liana infestation

ggplot(df.all.filt %>%
         filter(DBH >= 10),
       aes(x = DBH, y = h,
           color = as.factor(liana.cat))) +
  geom_point() +
  # scale_x_log10() +
  # scale_y_log10() +
  stat_smooth(se = FALSE, method = "lm") +
  facet_wrap(~ group, scales = "free") +
  theme_bw()

df.all.filt %>%
  filter(DBH >= 10) %>%
  filter(group == "PEA") %>%
  group_by(site) %>%
  summarise(N = n())


ggplot(df.all.filt %>%
         filter(DBH >= 10) %>%
         filter(group == "PEA"),
       aes(x = DBH, y = h,
           color = as.factor(liana.cat))) +
  geom_point() +
  # scale_x_log10() +
  # scale_y_log10() +
  stat_smooth(se = FALSE, method = "lm") +
  facet_wrap(~ site, scales = "free") +
  theme_bw()

# "ALF" "FLO" "FRP" "GAU" "PEA" "POA" "SAA" "SAT" "SIP" "TAN" "VCR"
#
# ggplot(df.all.filt %>%
#          filter(DBH >= 10) %>%
#          filter(group == "VCR"),
#        aes(x = DBH, y = h,
#            color = as.factor(liana.cat))) +
#   geom_point() +
#   stat_smooth(se = FALSE, method = "lm") +
#   facet_wrap(~ site) +
#   theme_bw()

# ggplot(df.all.filt %>%
#          filter(DBH >= 10,
#                 group == "PEA"),
#        aes(x = DBH, y = h,
#            color = as.factor(liana.cat))) +
#   geom_point() +
#   stat_smooth(se = FALSE, method = "lm") +
#   facet_wrap(~ site) +
#   theme_bw()


df.map <- df.all.filt %>%
  dplyr::select(group,site,lat,lon) %>%
  distinct() %>%
  group_by(group) %>%
  summarise(lat.m = mean(lat),
            lon.m = mean(lon))


r <- raster("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1_aggr_reclassified.tif")
df.r <- as.data.frame(r,xy = TRUE) %>%
  rename(lon =  x,
         lat = y,
         LU = layer) %>%
  filter(!is.na(LU))

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = df.r,
              aes(x = lon, y = lat, fill = as.factor(LU)),
              alpha = 0.3,show.legend = FALSE) +
  geom_sf(data = world,
          fill = NA,
          color = "black",
          alpha = 0.5) +
  geom_point(aes(x = lon.m, y = lat.m),
             data = df.map, shape = 16) +
  geom_label_repel(aes(x = lon.m, y = lat.m, label = group),
                   size=5,
                   data = df.map,
                   segment.color = 'grey50') +
  scale_fill_manual(values = c("white",c("#72a83d"),"darkgreen")) +
  scale_y_continuous(limits = c(-20,-5)) +
  scale_x_continuous(limits = c(-60,-50),expand = c(0,0)) +
  scale_size_continuous(range = c(0.1, 2)) +
  labs(x = "", y = "") +
  theme_bw() +
  guides(size = "none") +
  theme(text = element_text(size = 20))


# saveRDS(df.map,
#         "./data/ForestPlots/data/RainForMD.RDS")
#
# saveRDS(df.all.filt,
#         "./data/ForestPlots/data/df.Rainfor.RDS")
