rm(list = ls())

library(ggplot2)
library(ggrepel)
library(dplyr)
library(ggplot2)
library(tidyr)
library(raster)
library(readxl)
library(tidyr)
library(caret)

# Assemble datasets
####################################################################################################################################
# Helene + Sruthi, Panama for height
raw.data <- bind_rows(list(
  read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/2011_Height_Data.csv",stringsAsFactors = FALSE) %>%
    dplyr::select(Tag,Species,DBH_cm,Height,Lianas) %>%
    mutate(Species = tolower(Species)) %>%
    mutate(year = 2011),


  # To replace with most recent dataset
  read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/new_height_liana_data_2015_no_outliers_q20.csv",stringsAsFactors = FALSE) %>%
    dplyr::select(Tag,Species,DBH_cm,Height,Lianas) %>%
    mutate(Lianas = case_when(Lianas == 0 ~ 0,
                              Lianas < 3 ~ 1,
                              TRUE ~ 2)) %>%
    mutate(year = 2015),
  read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/2019_TLS_data.csv",stringsAsFactors = FALSE) %>%
    dplyr::select(Tag,Species,Final.DBH..cm.,Height..m.,Liana) %>%
    mutate(year = 2019) %>%
    rename(DBH_cm = Final.DBH..cm.,
           Height = Height..m.,
           Lianas = Liana)))
# BCI, Helene and Sruthi, CA
load("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/bci.spptable.rdata")

# ch.correction <- readRDS("./data/ch.data.correction.RDS")
# BCI.CA.df <- bind_rows(
#   readRDS("./data/BCI/BCI_CA_2015_Helene.RDS") %>%
#     dplyr::select(tag,spcode,DBH,Area,liana.cat) %>%
#     rename(CA = Area,
#            sp = spcode,
#            Tag = tag) %>%
#     mutate(year = 2015) %>%
#     left_join(bci.spptable %>%
#                 dplyr::select(sp,Latin),
#               by = "sp") %>%
#     mutate(sp = Latin) %>%
#     dplyr::select(-Latin),
#
#  read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/2019_TLS_data.csv",stringsAsFactors = FALSE) %>%
#   dplyr::select(Tag,Species,Final.DBH..cm.,CPA..m2.,Liana) %>%
#    left_join(ch.correction %>%
#                dplyr::select(tag,charea) %>%
#                rename(Tag = tag),
#              by = "Tag") %>%
#   mutate(year = 2019) %>%
#   rename(DBH = Final.DBH..cm.,
#          CA = charea,                     # Convex hull correction!
#          Lianas = Liana,
#          sp = Species) %>%
#     mutate(liana.cat = case_when(Lianas == 0 ~ "no",
#                                  Lianas == 1 ~ "low",
#                                  Lianas == 2 ~ "high")) %>%
#     dplyr::select(-c(Lianas,CPA..m2.)))
#
# saveRDS(BCI.CA.df,
#         "./outputs/BCI.CA.data.RDS")

# BCI.CA.change <- BCI.CA.df %>%
#   dplyr::select(Tag,DBH,CA,year) %>%
#   pivot_wider(names_from = year,values_from = c(DBH,CA)) %>%
#   remove_missing()

# ggplot(data = BCI.CA.change,
#        aes(x = DBH_2015, xend = DBH_2019,
#            y = CA_2015, yend = CA_2019)) +
#   geom_segment() +
#   geom_point(color = 'red') +
#   scale_x_log10() +
#   scale_y_log10() +
#   theme_bw()

################################################################################

df.match <- data.frame(sp = unique(raw.data$Species)) %>%
  left_join(bci.spptable %>%
              dplyr::select(sp,Latin),
            by = "sp") %>%
  mutate(sp.final = case_when(is.na(Latin) ~ sp,
                              TRUE ~ Latin)) %>%
  dplyr::select(sp,sp.final)


raw.data %>% group_by(year) %>%
  summarise(n(),
            .groups = "keep")

length(raw.data %>% pull(Tag) %>% unique())

BCI.ds <-
  raw.data %>%
  rename(dbh = DBH_cm,
         sp = Species,
         h  = Height) %>%
  mutate(liana.cat = case_when(Lianas == 0 ~ "no",
                               Lianas == 1 ~ "low",
                               Lianas == 2 ~ "high"),
         coi = NA) %>%
  filter(!is.na(h),
         !is.na(dbh)) %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high"))) %>%
  dplyr::select(c(Tag,dbh,h,year,coi,sp,liana.cat)) %>%
  mutate(site = "BCI") %>%
  left_join(df.match,
            by = "sp") %>%
  mutate(sp = sp.final) %>%
  dplyr::select(-sp.final)

saveRDS(BCI.ds,
        "./outputs/BCI.COI.data.RDS")

BCI.ds2keep <- BCI.ds %>%
  mutate(year = factor(year,
                       levels = c(2019,2015,2011))) %>%
  group_by(Tag) %>%
  arrange(year) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  dplyr::select(-c(Tag,year))


################################################################################
# Wannes Data

df.Wannes <- readRDS("./data/Afritron.RDS")
meta.data <- read.csv("./data/Afritron/africa_metadataforR_112017.csv") %>%
  filter(PlotID %in% df.Wannes[["PlotID"]]) %>%
  dplyr::select(PlotCode,PlotID,Country,ClusterName,LatitudeDecimal,LongitudeDecimal) %>%
  mutate(site = case_when(
    grepl("Haute-Abanga",ClusterName) ~ "Haute-Abanga",
    grepl("Leke",ClusterName) ~ "Leke",
    grepl("Mbam Djerem",ClusterName) ~ "Mbam Djerem",
    grepl("Monts du Cristal",ClusterName) ~ "Monts du Cristal",
    grepl("Nguti Cluster",ClusterName) ~ "Nguti Cluster",
    grepl("Noubale-Ndoki",ClusterName) ~ "Noubale-Ndoki",  # DONT FORGET TO CHANGE TO Nouabale
    grepl("Ogooue",ClusterName) ~ "Ogooue",
    grepl("Sangha",ClusterName) ~ "Sangha",
    grepl("Kisangani",ClusterName) ~ "Kisangani_all",
    TRUE ~ ClusterName)) %>%
  mutate(lat = LatitudeDecimal,
         lon = LongitudeDecimal) %>%
  mutate(common = case_when(site == ClusterName ~ 0,
                            TRUE ~ 1))
# AAA <-sort(unique(meta.data$PlotCode))
# write.csv(data.frame(ID = 1:length(AAA),
#                      plot = AAA),
#           "./data/Afritron.plots.csv")

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

  geom_point(aes(x = lon, y = lat, color = site,
                 shape = as.factor(common)),
             data = meta.data) +
  scale_fill_manual(values = c("white",c("#72a83d"),"darkgreen")) +
  scale_y_continuous(limits = c(-20,15)) +
  scale_x_continuous(limits = c(-15,50)) +
  scale_size_continuous(range = c(0.1, 2)) +
  labs(x = "", y = "") +
  theme_bw() +
  guides(size = "none") +
  theme(text = element_text(size = 20))


meta.data.sum <- meta.data %>%
  group_by(Country,site) %>%
  summarise(lat = mean(lat),
            lon = mean(lon),
            .groups = "keep")

df.Wannes.mut <- df.Wannes %>%
  left_join(meta.data %>%
              dplyr::select(PlotID,PlotCode,site,Country) %>%
              distinct(),
            by = "PlotID") %>%
  mutate(dbh = dbh/10) %>%
  dplyr::select(-PlotID)

df.Wannes.crit <- df.Wannes.mut %>%
  group_by(site) %>%
  summarise(all.plots = paste(unique(PlotCode),collapse = "|"),
            .groups = "keep",
            Ntot = n(),
            liana.cats = (length(unique(liana.cat))),
            Nhigh = length(which(liana.cat == "high")),
            Nlow = length(which(liana.cat == "low")),
            Nno = length(which(liana.cat == "no"))) %>%
  mutate(keep = (liana.cats == 3) & (Nhigh > 10) & (Nlow > 10)
         & (Nno > 10))

# View(df.Wannes.crit %>%
#        filter(!keep))

df.Wannes.site <- df.Wannes.mut %>%
  group_by(site) %>%
  # summarise(Ncat = length(unique(liana.cat)),
  #           Nhigh = length(which(liana.cat == "high")))
  filter(length(unique(liana.cat)) == 3,
         length(which(liana.cat == "high")) > 10,
         length(which(liana.cat == "low")) > 10,
         length(which(liana.cat == "no")) > 10)

sort(unique(df.Wannes.mut$site)[!(unique(df.Wannes.mut$site) %in% df.Wannes.site$site)])

# %>%
#   filter(!(site %in% c("Mbam Djerem","Monte Mitra")))        # Not all liana categories

ggplot(data = df.Wannes.site,
       aes(x = dbh, y = h, color = liana.cat)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(~site) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

df.Wannes.site %>%
  # filter(Country == "CONGO-BRAZZAVILLE") %>%
  group_by(Country,site) %>%
  summarise(Nno = length(site[liana.cat == "no"]),
            Nlow = length(site[liana.cat == "low"]),
            Nhigh = length(site[liana.cat == "high"]),
            .groups = "keep")

meta.data.sum.kept <- meta.data.sum %>%
  filter(site %in% unique(df.Wannes.site$site))

saveRDS(meta.data.sum.kept,
        "./data/Afritron/Afritron.metadata.RDS")



ggplot() +
  geom_raster(data = df.r,
              aes(x = lon, y = lat, fill = as.factor(LU)),
              alpha = 0.3,show.legend = FALSE) +
  geom_sf(data = world,
          fill = NA,
          color = "black",
          alpha = 0.5) +

  geom_point(aes(x = lon, y = lat),
             data = meta.data.sum.kept, shape = 16) +
  # geom_label_repel(aes(x = lon, y = lat, label = ClusterName),
  #                  size=5,
  #                  data = df.Wannes.site,
  #                  segment.color = 'grey50') +
  scale_fill_manual(values = c("white",c("#72a83d"),"darkgreen")) +
  scale_y_continuous(limits = c(-20,15)) +
  scale_x_continuous(limits = c(-15,50)) +
  scale_size_continuous(range = c(0.1, 2)) +
  labs(x = "", y = "") +
  theme_bw() +
  guides(size = "none") +
  theme(text = element_text(size = 20))

################################################################################
# Tapajos

data <- read.csv("./data/Tapajos.csv") %>%
  rename(sp = Species,
         dbh = DBH_2014,
         h = Height_2014) %>%
  mutate(coi = case_when(Liana_load == 0 ~ 0,
                         Liana_load <= 25 ~ 1,
                         Liana_load <= 50 ~ 2,
                         Liana_load <= 75 ~ 3,
                         Liana_load <= 100 ~ 4,
                         TRUE ~ NA_integer_)) %>%
  mutate(liana.cat = case_when(coi == 0 ~ "no",
                               coi <= 2 ~ "low",
                               coi <= 4 ~ "high",
                               TRUE ~ NA_character_)) %>%
  dplyr::select(dbh,h,coi,sp,liana.cat) %>%
  mutate(site = "Tapajos")

# table(data$Plot)
# table(data$liana.cat)

data.tapajos <- data %>%
  filter(dbh >= 10,
         !is.na(liana.cat))

# ggplot(data = data.filt,
#        aes(x = dbh,
#            y = h,
#            color = liana.cat)) +
#   geom_point() +
#   stat_smooth(method = "lm") +
#   # facet_wrap(~ Forest_class) +
#   theme_bw()
##################################################################################
# Rainfor

Rainfor.df <- readRDS("./data/ForestPlots/data/df.Rainfor.RDS") %>%
  ungroup() %>%
  mutate(site = group) %>%
  rename(dbh = DBH,
         sp = Species,
         coi = COI) %>%
  dplyr::select(c(dbh,h,coi,sp,liana.cat,site)) %>%
  filter(dbh > 0, h > 0)

################################################################################
# Karin Santos
raw.data.Karin <- read_xlsx("./data/Karin/Karin data_Felicien.xlsx")

Karin.df <- raw.data.Karin %>%
  ungroup() %>%
  rename(sp = Species,
         dbh = `DBH (cm)`,
         COI = Lianas,
         h = `Total Height (m)`) %>%
  mutate(coi = case_when(COI == "0" ~ 0,
                         COI == "x" ~ 1,
                         COI == "xx" ~ 2,
                         COI == "xxxx" ~ 4,
                         COI == "xxx" ~ 3,
                         TRUE ~ NA)) %>%
  mutate(liana.cat = case_when(coi %in% c(0) ~ "no",
                               coi %in% c(1,2) ~ "low",
                               TRUE ~ "high"),
         h = as.numeric(h),
         site = "Campinas") %>%
  dplyr::select(c(dbh,h,coi,sp,liana.cat,site)) %>%
  filter(dbh > 0, h > 0) %>%
  filter(!is.na(dbh),
         !is.na(h),
         !is.na(coi))

# ggplot(data = Karin.df %>%
#          filter(dbh > 5),
#        aes(x = dbh, y = h, color = as.factor(liana.cat),
#            fill = as.factor(liana.cat))) +
#   geom_point(alpha = 0.5, size = 0.1) +
#   stat_smooth(method = "lm") +
#   scale_x_log10() +
#   scale_y_log10() +
#   theme_bw()

# raw.data.Karin <- read_xlsx("./data/Karin/Karin data_Felicien.xlsx")
#
# coords <- raw.data.Karin %>%
#   dplyr::select(Coordinates) %>%
#   distinct() %>%
#   mutate(lat.string = sub("\"S .*", "", Coordinates),
#          lon.string = str_sub(str_match(Coordinates, " 4\\s*(.*?)\\s*W")[,1],
#                               end = -3)) %>%
#   mutate(lat =   as.numeric(substr(lat.string,1,2)) +
#            as.numeric(substr(lat.string,4,5))/60 +
#            as.numeric(sub(".*’", "", lat.string))/3600,
#          lon =   as.numeric(substr(lon.string,2,3)) +
#            as.numeric(substr(lon.string,5,6))/60 +
#            as.numeric(sub(".*’", "", lon.string))/3600)

################################################################################
# Arildo

Arildo.df <- read.csv("./data/lianas_COI.csv") %>%
  ungroup() %>%
  rename(sp = species,
         COI = coi,
         h = height) %>%
  mutate(dbh = trunk_circ/(2*pi),
         coi = case_when(COI == "0" ~ 0,
                         COI == "x" ~ 1,
                         COI == "xx" ~ 2,
                         COI == "xxxx" ~ 4,
                         COI == "xxx" ~ 3,
                         TRUE ~ NA)) %>%
  mutate(liana.cat = case_when(coi %in% c(0) ~ "no",
                               coi %in% c(1,2) ~ "low",
                               TRUE ~ "high"),
         site = "Sao Paulo") %>%
  dplyr::select(c(dbh,h,coi,sp,liana.cat,site)) %>%
  filter(dbh > 0, h > 0)

###############################################################################
# Erika (Brazil)

Erika.df <- readRDS("./outputs/Erika_data.RDS") %>%
  ungroup() %>%
  mutate(site = as.character(site)) %>%
  dplyr::select(dbh,h,coi,sp,liana.cat,site)

# ################################################################################
# # https://www.sciencedirect.com/science/article/pii/S0378112710002689#fig2
# # 2 lianas --> COI = 1 and 5.5 lianas --> COI = 2
# # slope = (2 - 1)/(5.5 - 2)
# # intercept = 2 - slope*5.5
# # lianas = 0:7
# # plot(lianas,COI <- intercept + lianas*slope,type = 'l')
#
#
# Jennifer.df <- read.csv(
#   "./data/Costa_Rica/Data Base, TREE plantaciones Mixtas 2020_FINAL-1.csv") %>%
#   rename(dbh = DBH_cm,
#          h = ALTURA_COPA.mt,
#          lianas = X..LIANAS) %>%
#   # mutate(coi = floor(intercept + lianas*slope)) %>%
#   # mutate(liana.cat = case_when(coi %in% c(0) ~ "no",
#   #                              coi %in% c(1,2) ~ "low",
#   #                              TRUE ~ "high")) %>%
#   mutate(liana.cat = case_when(lianas <= 1 ~ "no",
#                                lianas <= 3 ~ "low",
#                                TRUE ~ "high")) %>%
#   filter(h > 0)
#
# # plot(Jennifer.df$lianas,
# #      as.numeric(factor(Jennifer.df$liana.cat,
# #                        levels = c("no","low","high"))))
# # sum <- Jennifer.df %>%
# #   group_by(ID.PLOT) %>%
# #   summarise(m.liana = mean(lianas),
# #             dbh.m = mean(dbh),
# #             h.m = mean(h))
# #
# # plot(sum$dbh.m,sum$h.m)
#
# ggplot(data = Jennifer.df %>%
#          filter(dbh >= 10),
#        aes(x = dbh, y = h, color = liana.cat)) +
#   geom_point() +
#   stat_smooth(method = "lm", se = FALSE) +
#   scale_x_log10() +
#   scale_y_log10() +
#   theme_bw()
#
#
#
# ggplot(data = Jennifer.df,
#        aes(x = dbh, y = h)) +
#   geom_point() +
#   geom_point(data = Jennifer.df %>% filter(lianas > 10),
#              aes(x = dbh, y = h), color = "red") +
#   # stat_smooth(method = "lm", se = FALSE) +
#   # scale_x_log10() +
#   # scale_y_log10() +
#   theme_bw()


################################################################################
# Jocker
# https://www.sciencedirect.com/science/article/pii/S0378112713004337

Jocker.df <- read.csv(
  "./data/Jocker/dados.csv") %>%
  rename(coi = ioc,
         h = ht) %>%
  mutate(dbh = cap/pi,
         sp = "Parapiptadenia rigida") %>%
  mutate(liana.cat = case_when(coi == 1 ~ "no",
                               coi <= 3 ~ "low",
                               TRUE ~ "high")) %>%
  filter(!is.na(h),
         !is.na(dbh)) %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high"))) %>%
  dplyr::select(c(arv,dbh,h,coi,sp,liana.cat)) %>%
  mutate(site = "Rio Grande") %>%
  distinct() %>%
  dplyr::select(-arv)

# ggplot(data = Jocker.df,
#        aes(x = 1/dbh,y = log(h), color = liana.cat)) +
#   geom_point() +
#   stat_smooth(method = "lm", se = FALSE) +
#   theme_bw()

#################################################################################

data.DV <- readRDS("./data/Danum/DV.processed.RDS") %>%
  rename(coi = COI) %>%
  # mutate(dbh = dbh/10) %>%
  # filter(dbh >= 30) %>%
  dplyr::select(c(dbh,h,coi,sp,liana.cat)) %>%
  mutate(site = "Danum Valley")

data.DV %>%
  group_by(sp) %>%
  mutate(Ntot = n()) %>%
  group_by(liana.cat,
           sp) %>%
  summarise(N = n(),
            Ntot = Ntot[1]) %>%
  arrange(desc(Ntot))

ggplot(data = data.DV %>%
         filter(dbh >= 10),
       aes(x = dbh, y = h, color = as.factor(liana.cat),
           fill = as.factor(liana.cat))) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw()

####################################################################################################################################
# Joe, Panama
raw.data <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/c97to18.csv",stringsAsFactors = FALSE)

df.match <- data.frame(sp = tolower(unique(raw.data$sp))) %>%
  left_join(bci.spptable %>%
              dplyr::select(sp,Latin),
            by = "sp") %>%
  mutate(sp.final = case_when(is.na(Latin) ~ sp,
                              TRUE ~ Latin)) %>%
  dplyr::select(sp,sp.final) %>%
  mutate(sp = toupper(sp))


Panama.ds <-
  raw.data %>%
  left_join(df.match,
            by = "sp") %>%
  mutate(sp = sp.final) %>%
  dplyr::select(-sp.final) %>%
  rename(dbh = dbhtot18,
         h  = hght18,
         coi = liana18) %>%
  mutate(liana.cat = case_when(coi %in% c(0) ~ "no",
                               coi %in% c(1,2) ~ "low",
                               TRUE ~ "high")) %>%
  filter(!is.na(h),
         !is.na(dbh),
         !is.na(coi)) %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high")),
         dbh = dbh/10) %>%
  dplyr::select(c(dbh,h,coi,sp,liana.cat)) %>%
  mutate(site = "Gigante")

####################################################################################################################################
# Congo sites -- Grace data
raw.data <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/Congo/database.csv",
                            stringsAsFactors = FALSE) %>%
  mutate(liana.cat = case_when(Liana %in% c(0) ~ "no",
                               Liana %in% c(1,2) ~ "low",
                               TRUE ~ "high"))

Congo.ds <-
  raw.data %>%
  rename(dbh = DBH_cm,
         h  = Ht_m,
         sp = Species,
         coi = Liana,
         site = Site) %>%
  mutate(liana.cat = case_when(coi %in% c(0) ~ "no",
                               coi %in% c(1,2) ~ "low",
                               TRUE ~ "high")) %>%
  filter(!is.na(h),
         !is.na(dbh),
         !is.na(liana.cat)) %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high"))) %>%
  dplyr::select(c(dbh,h,coi,sp,site,liana.cat)) %>%
  filter(site != "Semi-F") %>%
  mutate(site = case_when(site == "Atla-F" ~ "Luki",
                          site == "Sand-F" ~ "Mokabi"))


ggplot(data = Congo.ds,
       aes(x = dbh, y = h, color = liana.cat),
       size = 0.1) +
  geom_point() +
  stat_smooth(method = "lm",
              se = FALSE) +
  facet_wrap(~ site) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()


################################################################################
# Grace # 2

# part1 <- raw.data %>%
#   filter(Site == "Semi-F") %>% pull(Liana) %>% table()

part1 <- read_xlsx("./data/Data_Loundoungou.xlsx") %>%
  mutate(Id_ARB = paste0(Subplot,"_",Tree_ID))
uniqueIDs <- part1 %>%
  pull(Id_ARB)

part2 <- read.csv("~/Downloads/tree_frontiers_269.csv")
uniqueIDs2 <- part2 %>%
  pull(Id_ARB)

Intersect <- intersect(uniqueIDs,uniqueIDs2)

df1 <- part1 %>%
  dplyr::select(Id_ARB,species, DBH_cm, Ht_m,Liana_Infestation) %>%
  rename(sp = species) %>%
  mutate(dbh = as.numeric(DBH_cm),
         h = as.numeric(Ht_m),
         coi = as.integer(Liana_Infestation)) %>%
  mutate(liana.cat = case_when(coi %in% c(0) ~ "no",
                               coi %in% c(1,2) ~ "low",
                               TRUE ~ "high")) %>%
  filter(!is.na(h),
         !is.na(dbh),
         !is.na(liana.cat)) %>%
  mutate(site = "Loundoungou") %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high"))) %>%
  dplyr::select(c(Id_ARB,dbh,h,coi,sp,site,liana.cat))

df2 <- part2 %>%
  mutate(site = "Loundoungou") %>%
  rename(coi = COI,
         dbh = diam,
         h = Height,
         sp = sp_tree) %>%
  mutate(liana.cat = case_when(coi %in% c(0) ~ "no",
                               coi %in% c(1,2) ~ "low",
                               TRUE ~ "high")) %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high"))) %>%
  filter(!is.na(h),
         !is.na(dbh),
         !is.na(liana.cat)) %>%
  dplyr::select(c(Id_ARB,dbh,h,coi,sp,site,liana.cat))

df.intersect <- bind_rows(df1 %>%
  filter(Id_ARB %in% Intersect) %>%
    mutate(source = "Grace"),
  df2 %>%
  filter(Id_ARB %in% Intersect) %>%
    mutate(source = "Begum"))

df.intersect.wide <- df.intersect %>%
  pivot_wider(names_from = source,
             values_from = c(dbh,h,coi,sp,liana.cat))

ggplot(data = df.intersect.wide,
       aes(y = h_Begum, x = h_Grace)) +
  geom_point() +
  geom_abline(slope = 1, linetype = 2, intercept = 0) +
  stat_smooth(method = "lm") +
  theme_bw()

ggplot(data = df.intersect.wide,
       aes(y = dbh_Begum, x = dbh_Grace)) +
  geom_point() +
  geom_abline(slope = 1, linetype = 2, intercept = 0) +
  stat_smooth(method = "lm") +
  theme_bw()

same.species <- df.intersect.wide %>%
  mutate(same = sp_Begum == sp_Grace) %>% pull(same) %>% table()

CM <- confusionMatrix(data=as.factor(df.intersect.wide$coi_Begum),
                      reference = factor(df.intersect.wide$coi_Grace,
                                            levels = c(0,1,2,3,4)))

CM2 <- confusionMatrix(data=as.factor(df.intersect.wide$liana.cat_Begum),
                      reference = as.factor(df.intersect.wide$liana.cat_Grace))

begum.ds <- bind_rows(df1 %>%
                              filter(!(Id_ARB %in% Intersect)),
                            df2) %>%
  dplyr::select(-c(Id_ARB))


ggplot(data = begum.ds,
       aes(x = dbh, y = h, color = liana.cat),
       size = 0.1) +
  geom_point() +
  stat_smooth(method = "lm",
              se = FALSE) +
  facet_wrap(~ site) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

# begum.ds <- read.csv("~/Downloads/tree_frontiers_269.csv") %>%
#   mutate(site = "Loundoungou") %>%
#   rename(coi = COI,
#          dbh = diam,
#          h = Height,
#          sp = sp_tree) %>%
#   mutate(liana.cat = case_when(coi %in% c(0) ~ "no",
#                                coi %in% c(1,2) ~ "low",
#                                TRUE ~ "high")) %>%
#   mutate(liana.cat = factor(liana.cat,
#                             levels = c("no","low","high"))) %>%
#   filter(!is.na(h),
#          !is.na(dbh),
#          !is.na(liana.cat)) %>%
#   dplyr::select(c(dbh,h,coi,sp,site,liana.cat))


####################################################################################################################################
# Liana removal experiment

# Panama2.ds <- bind_rows(list(
#   read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/TreeAttri_Rplot (1).csv",
#                      stringsAsFactors = FALSE) %>%
#   rename(dbh = DBH,
#          h = Height) %>%
#   mutate(sp = "Unknown",
#          coi = 0,
#          liana.cat = "no"),
#   read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/TreeAttri_Cplot (1).csv",
#            stringsAsFactors = FALSE) %>%
#     rename(dbh = DBH,
#            h = Height) %>%
#     mutate(sp = "Unknown",
#            coi = 4,
#            liana.cat = "high"))) %>%
#   filter(!is.na(h),
#          !is.na(dbh),
#          !is.na(liana.cat)) %>%
#     mutate(liana.cat = factor(liana.cat,
#                               levels = c("no","low","high")),
#            site = "Gigante-Us") %>%
#     dplyr::select(c(dbh,h,coi,sp,site,liana.cat))

Panama2.ds <- read.csv("~/Downloads/allwithspev4.csv") %>%
  rename(dbh = tls.dbh,
         h = tls.h) %>%
  mutate(dbh = dbh/10,
         sp = paste(Genus,Species),
         site = "Gigante-Us",
         coi = case_when(Treatment == "R" ~ 0,
                         TRUE  ~ 4),
         liana.cat = case_when(Treatment == "R" ~ "no",
                               TRUE  ~ "high")) %>%
  filter(!is.na(h),
         !is.na(dbh),
         !is.na(liana.cat)) %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high")),
         site = "Gigante-Us") %>%
  dplyr::select(c(dbh,h,coi,sp,site,liana.cat)) %>%
  filter(dbh >= 20)


####################################################################################################################################
# Australia
md.australia <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/all_mFoRCEAU_plots_summary-1.csv")

Australia.ds <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/COI_data_FoRCE_Australia.csv",
           stringsAsFactors = FALSE) %>%
  left_join(md.australia %>% dplyr::select(plot_name,canopy),
            by = "plot_name") %>%
  filter(form %in% c("tree"),
         bent_broken == 0) %>%
  # dplyr::select(-canopy) %>%
  rename(h = height..m.,
         coi = COI) %>%
  mutate(coi = as.numeric(coi)) %>%
  mutate(coi = case_when(coi == 0 ~ 0,
                         coi < 25 ~ 1,
                         coi < 50 ~ 2,
                         coi < 75 ~ 3,
                         TRUE ~ 4)) %>%
  mutate(sp = paste(genus_est,species_est),
         site = forest) %>%
  mutate(liana.cat = case_when(coi %in% c(0) ~ "no",
                               coi %in% c(1,2) ~ "low",
                               TRUE ~ "high")) %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high"))) %>%
  filter(!is.na(h),
         !is.na(dbh),
         !is.na(liana.cat)) %>%
  dplyr::select(c(dbh,h,coi,sp,site,liana.cat,canopy))

Australia.ds.can <- bind_rows(Australia.ds,
                              Australia.ds %>%
                                mutate(canopy = "all"))


ggplot(data = Australia.ds.can %>%
         filter(dbh >= 10) %>%
         filter(!is.na(canopy)),
       aes(x = dbh, y = h, color = liana.cat)) +
  geom_point(alpha = 0.5, size = 0.5) +
  facet_wrap(~ canopy,nrow = 1) +
  stat_smooth(se = FALSE,method = "lm") +
  scale_x_log10(limits = c(10,200),breaks = c(10,30,50,100)) +
  scale_y_log10(limits = c(1,50),breaks = seq(10,24,2)) +
  theme_bw()

# Australia.ds <- Australia.ds %>%
#   filter(dbh >= 10) %>%
#   filter(canopy == "open") %>%
#   dplyr::select(-canopy)

########################################################################################################################

Alain.ds <- read.csv("./data/Tree_COI_Data_Cameroon_MissingHeights.csv") %>%

  rename(coi = COI,
         dbh = dbh_cm,
         h = height_m) %>%
  mutate(sp = paste(genus,species),
         site = sub("\\_.*", "", plotID)) %>%
  mutate(liana.cat = case_when(coi %in% c(0) ~ "no",
                               coi %in% c(1,2) ~ "low",
                               TRUE ~ "high")) %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high"))) %>%
  filter(!is.na(h),
         !is.na(dbh),
         !is.na(liana.cat)) %>%
  dplyr::select(c(dbh,h,coi,sp,site,liana.cat)) %>%
  filter(site == "OKU") # The other site does not have high liana infestation data..

# Other plots Panama

cdf.Panama <- readRDS("./outputs/Data.otherplots.Panama.RDS") %>%
  dplyr::select(-plot) %>%
  filter(h > 0) %>%
  rename(site = plot.group)

additional.BCI <- cdf.Panama %>%
  dplyr::filter(site == "BCI")

ccdf.Panama <- cdf.Panama %>%
  dplyr::filter(site != "BCI")


BCI.ds2keep.addi <- bind_rows(BCI.ds2keep,
                              additional.BCI)

ggplot(data = additional.BCI,
       aes(x = dbh, y = h, color = liana.cat,
      fill = liana.cat)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm", formula = y ~ x) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

# Tanzanian data

file <- "./data/TanzaniaData_FoRCE_COI.csv"
# file <- "./data/TanzaniaData_microFoRCE_COI.csv"
raw.data.Tan <- read.csv(file)

unique(sort(raw.data.Tan$plotID))

data.Tan <-  raw.data.Tan %>%
  filter(!is.na(height_m)) %>%
  mutate(liana.cat = case_when(COI == 0 ~ "no",
                               COI < 3 ~ "low",
                               COI <= 4 ~ "high")) %>%
  rename(dbh = dbh_cm,
         h = height_m,
         coi = COI) %>%
  mutate(sp = tolower(paste(genus,species))) %>%
  filter(!is.na(h),
         !is.na(dbh)) %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high"))) %>%
  # dplyr::select(c(plotID,dbh,h,coi,sp,liana.cat)) %>%
  filter(dbh >= 10) %>%
  mutate(site = "Tanzania") %>%
  # mutate(site = substr(plotID,1,4)) %>%
  # mutate(site = case_when(longitude < 36.1 ~ "Tan_1",
  #                         longitude < 36.6 ~ "Tan_2",
  #                         TRUE ~ "Tan_3")) %>%
  group_by(site,liana.cat) %>%
  mutate(N = n()) %>%
  group_by(site) %>%
  # filter(min(N)>10,
  #        length(unique(liana.cat)) == 3) %>%
  # filter(site != "IKUL") %>% # We also remove IKUL for which only small ind. with no liana.cat
  filter(!is.na(h),
         !is.na(dbh),
         !is.na(liana.cat)) %>%
  mutate(Elev = case_when(elevation < 1000 ~ "low",
                          TRUE ~ "high")) %>%
  dplyr::select(c(dbh,h,coi,sp,site,liana.cat))

# data.site.Tan <- data.Tan %>%
#   dplyr::select(c(site,forest,latitude,longitude,N,liana.cat)) %>%
#   distinct()
#
# data.site.Tan.N <- data.site.Tan %>%
#   dplyr::select(site,N,liana.cat) %>%
#   distinct() %>%
#   group_by(site) %>%
#   mutate(Ntot = sum(N))
#
#
#
# ggplot(data = data.site) +
#   geom_point(aes(x = longitude, y = latitude, color = site)) +
#   theme_bw()

ggplot(data.Tan,
       aes(x = dbh, y = h,
           color = liana.cat)) +
  geom_point(size = 0.1) +
  # facet_wrap(~as.factor(elevation)) +
  scale_x_log10() +
  scale_y_log10() +
  stat_smooth(method = "lm", se = FALSE) +
  theme_bw()

#################################################################################
Pasoh.ds <- readRDS("./data/Pasoh/data.merged.RDS") %>%
  rename(h = height,
         liana.cat = liana.cat.consensus) %>%
  mutate(site = "Pasoh") %>%
  filter(!is.na(h),
         !is.na(dbh),
         !is.na(liana.cat)) %>%
  dplyr::select(c(dbh,h,coi,sp,site,liana.cat))

# ggplot(data = Pasoh.ds %>%
#          filter(dbh >= 10),
#        aes(x = dbh, y = h, color = liana.cat)) +
#   geom_point(alpha = 0.5, size = 0.2) +
#   facet_wrap(~ site) +
#   stat_smooth(se = FALSE,method = "lm") +
#   scale_x_log10(limits = c(10,200)) +
#   scale_y_log10(limits = c(1,50)) +
#   theme_bw()

all.df <- bind_rows(list(
  Rainfor.df,
  data.tapajos,
  Alain.ds,
  data.Tan,
  BCI.ds2keep.addi,
  ccdf.Panama,
  Panama.ds,
  Karin.df,
  Jocker.df,
  Congo.ds,
  Arildo.df,
  Erika.df,
  df.Wannes.site %>%
    dplyr::select(-PlotCode,Country) %>%
    dplyr::select(-Country),
  data.DV,
  # Panama2.ds,   # We don't include it for the upscaling
  begum.ds,
  Pasoh.ds %>%
    ungroup() %>%
    dplyr::select(-tag),
  Australia.ds %>%
    dplyr::select(-canopy) %>%
    mutate(site = "Australia"))) %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high")))

saveRDS(all.df,
        "./outputs/All.COI.data.RDS")

all.df %>%
  filter(dbh >= 10) %>%
  group_by(site) %>%
  summarise(N = n(),
          dbh = mean(dbh),
          h = mean(h),
          COI = median(coi),
          COI.m = mean(coi),
          .groups = "keep")

all.df %>%
  filter(dbh >= 10) %>%
  # group_by(site) %>%
  summarise(N = n(),
            dbh = mean(dbh),
            h = mean(h),
            COI = median(coi,na.rm = TRUE),
            COI.m = mean(coi,na.rm = TRUE),
            .groups = "keep")

df.Wannes.site %>%
  ungroup() %>%
  filter(dbh >= 10) %>%
  summarise(N = n(), Nsite = length(unique(site)))


ggplot(data = all.df %>%
         filter(dbh >= 10),
       aes(x = dbh, y = h, color = liana.cat)) +
  geom_point(alpha = 0.5, size = 0.2) +
  facet_wrap(~ site) +
  stat_smooth(se = FALSE,method = "lm") +
  # scale_x_log10(limits = c(1,200)) +
  # scale_y_log10(limits = c(1,50)) +
  theme_bw()


ggplot(data = all.df %>%
         filter(dbh >= 10),
       aes(x = dbh, y = h, color = liana.cat, fill = liana.cat)) +
  geom_point(alpha = 0.5, size = 0.2) +
  stat_smooth(se = TRUE,method = "lm") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

ggplot(data = all.df %>%
         filter(dbh >= 10),
       aes(x = dbh, y = h, color = liana.cat, fill = liana.cat)) +
  geom_point(alpha = 0.5, size = 0.2) +
  stat_smooth(se = TRUE,method = "lm") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

summary(lm(data = all.df %>%
             filter(dbh >= 10),
           formula = log(h) ~ log(dbh) + liana.cat))

bind_rows(all.df %>%
  filter(dbh >= 10) %>%
  group_by(liana.cat) %>%
  summarise(N = n()),
  all.df %>%
    filter(dbh >= 10) %>%
    # group_by(liana.cat) %>%
    summarise(N = n()) %>% mutate(liana.cat = "total"))

# ################################################################################
# # # Slenderness
# Slenderness <- all.df %>%
#   filter(dbh >= 10) %>%
#   mutate(S = (h)/(dbh)) %>%
#   group_by(liana.cat) %>%
#   mutate(S.m = mean(S,na.rm = TRUE))
#
# alpha = 0.11
#
# f <- function(x) {
#   r <- quantile(x, probs = c(alpha/2, 0.25, 0.5, 0.75, 1 - alpha/2))
#   names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
#   r
# }
#
# ggplot(data = Slenderness,
#        aes(y = S, x = (as.factor(liana.cat)),
#            fill = as.factor(liana.cat),
#            color = as.factor(liana.cat))) +
#
#   stat_summary(fun.data = f,
#                geom="boxplot",
#                width = .15,
#                alpha = 0.5,
#                outlier.shape = NA) +
#
#   ggdist::stat_halfeye(
#     adjust = .5,
#     width = .6,
#     .width = 0,
#     justification = -0.2, alpha = 0.5,
#     point_colour = NA
#   ) +
#   # ggdist::stat_pointinterval(aes(x = as.numeric(as.factor(liana.cat)) + 0.11),
#   #                            .width = c(1-alpha),
#   #                            adjust = .5,
#   #                            width = 1,
#   #                            justification = -0.2, alpha = 1) +
#   scale_fill_manual(values = c("no" = "darkgreen",
#                                "low" = "orange",
#                                "high"= "darkred")) +
#   scale_color_manual(values = c("no" = "darkgreen",
#                                "low" = "orange",
#                                "high"= "darkred")) +
#   scale_y_continuous(limits = c(0,1.8)) +
#   # scale_y_log10(limits = c(0.1,2)) +
#   labs(x = "", y = "Tree slenderness (m/cm)") +
#   scale_x_discrete(labels = c("No","Low \r\n liana infestation","High")) +
#   theme_bw() +
#   guides(fill = "none", color = "none") +
#   theme(text = element_text(size = 24),
#         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
#         axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))
#
# Slenderness %>%
#   group_by(liana.cat) %>%
#   summarise(med = median(S))
#
# # summary(aov(data = Slenderness,
# #     formula = S ~ liana.cat))
#
#
# ggplot(data = Slenderness,
#        aes(x = dbh, y = S, color = liana.cat)) +
#   geom_point() +
#   stat_smooth(method = "lm") +
#   scale_x_log10() +
#   scale_y_log10() +
#   # facet_wrap(~ site) +
#   theme_bw()
# #
# # scp /home/femeunier/Documents/projects/LianaRemovalRevisited/outputs/Slenderness.RDS hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/
#
# # ggplot(data = Slenderness %>%
# #          filter(site == "Pasoh")) +
# #   geom_point(aes(x = dbh, y = S, color = liana.cat)) +
# #   theme_bw()
# #
# # Slenderness %>%
# #   filter(site == "Pasoh") %>%
# #          # dbh >= 10, dbh <= 50) %>%
# #   group_by(liana.cat) %>%
# #   summarise(dbh.m = mean(dbh),
# #             dbh.min = min(dbh),
# #             dbh.max = max(dbh),
# #             h.m = mean(h),
# #             S.m = mean(S))
# #
# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/outputs/All.COI.data.RDS hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/
# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/outputs/BCI.COI.data.RDS hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/
