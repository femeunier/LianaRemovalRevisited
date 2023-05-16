rm(list = ls())

library(ggplot2)
library(ggrepel)
library(dplyr)

# Assemble datasets
####################################################################################################################################
# Helene + Sruthi, Panama
raw.data <- bind_rows(list(
  read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/2011_Height_Data.csv",stringsAsFactors = FALSE) %>%
    dplyr::select(Species,DBH_cm,Height,Lianas) %>%
    mutate(year = 2011),
  read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/2015_Height_Data.csv",stringsAsFactors = FALSE) %>%
    dplyr::select(Species,DBH_cm,Height,Lianas) %>%
    mutate(year = 2015),
  read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/2019_TLS_data.csv",stringsAsFactors = FALSE) %>%
    dplyr::select(Species,Final.DBH..cm.,Height..m.,Liana) %>%
    mutate(year = 2019) %>%
    rename(DBH_cm = Final.DBH..cm.,
           Height = Height..m.,
           Lianas = Liana)))

saveRDS(raw.data,
        "./outputs/BCI.COI.data.RDS")

raw.data %>% group_by(year) %>%
  summarise(n())

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
  dplyr::select(c(dbh,h,year,coi,sp,liana.cat)) %>%
  mutate(site = "BCI")

saveRDS(BCI.ds,
        "./outputs/BCI.COI.data.RDS")

BCI.ds <- BCI.ds %>%
  dplyr::select(-year)


####################################################################################################################################
# Joe, Panama
raw.data <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/c97to18.csv",stringsAsFactors = FALSE)
Panama.ds <-
  raw.data %>%
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
  mutate(site = "Gigante-Joe")

####################################################################################################################################
# Congo sites
raw.data <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/Congo/database.csv",
                            stringsAsFactors = FALSE)

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
  dplyr::select(c(dbh,h,coi,sp,site,liana.cat))

####################################################################################################################################
# Liana removal experiment

Panama2.ds <- bind_rows(list(
  read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/TreeAttri_Rplot (1).csv",
                     stringsAsFactors = FALSE) %>%
  rename(dbh = DBH,
         h = Height) %>%
  mutate(sp = "Unknown",
         coi = 0,
         liana.cat = "no"),
  read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/TreeAttri_Cplot (1).csv",
           stringsAsFactors = FALSE) %>%
    rename(dbh = DBH,
           h = Height) %>%
    mutate(sp = "Unknown",
           coi = 4,
           liana.cat = "high"))) %>%
  filter(!is.na(h),
         !is.na(dbh),
         !is.na(liana.cat)) %>%
    mutate(liana.cat = factor(liana.cat,
                              levels = c("no","low","high")),
           site = "Gigante-Us") %>%
    dplyr::select(c(dbh,h,coi,sp,site,liana.cat))



####################################################################################################################################
# Australia

Australia.ds <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/COI_data_FoRCE_Australia.csv",
           stringsAsFactors = FALSE) %>%
  filter(form %in% c("tree")) %>%
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
  dplyr::select(c(dbh,h,coi,sp,site,liana.cat))


# Australia.md.ds <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/COI_metadata_FoRCE_Australia.csv",
#                             stringsAsFactors = FALSE)
#
# ggplot() +
#   geom_sf(data = world,
#           fill = NA,
#           color = "darkgrey") +
#   geom_point(aes(x = centre_e, y = -centre_s),
#              data = Australia.md.ds, shape = 1) +
#   # geom_label_repel(aes(x = centre_e, y = -centre_s, label = forest),
#   #                  size=2,
#   #                  data = Australia.md.ds,
#   #                  box.padding = unit(0.35, "lines"),
#   #                  point.padding = unit(0.5, "lines"),
#   #                  segment.color = 'grey50') +
#   # scale_y_continuous(limits = c(-1,-0.5)*23.5) +
#   # scale_x_continuous(limits = c(140,150)) +
#   scale_size_continuous(range = c(0.1, 2)) +
#   theme_bw()


all.df <- bind_rows(list(
  BCI.ds,
  Panama.ds,
  Congo.ds,
  Panama2.ds,
  Australia.ds %>% mutate(site = "Australia"))) %>%
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


ggplot(data = all.df,
       aes(x = dbh, y = h, color = liana.cat)) +
  geom_point(alpha = 0.5, size = 0.2) +
  facet_wrap(~ site) +
  stat_smooth(se = FALSE,method = "lm") +
  scale_x_log10(limits = c(1,200)) +
  scale_y_log10(limits = c(1,50)) +
  theme_bw()


ggplot(data = all.df,
       aes(x = dbh, y = h, color = liana.cat, fill = liana.cat)) +
  geom_point(alpha = 0.5, size = 0.2) +
  stat_smooth(se = TRUE,method = "lm") +
  scale_x_log10(limits = c(1,200)) +
  scale_y_log10(limits = c(3,50)) +
  theme_bw()

summary(lm(data = all.df %>% filter(dbh >= 10),
           formula = log(h) ~ log(dbh) + liana.cat))

# plot_model int
