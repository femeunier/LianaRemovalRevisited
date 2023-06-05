rm(list = ls())

library(ggplot2)
library(ggrepel)
library(dplyr)

# Assemble datasets
####################################################################################################################################
# Helene + Sruthi, Panama
raw.data <- bind_rows(list(
  read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/2011_Height_Data.csv",stringsAsFactors = FALSE) %>%
    dplyr::select(Tag,Species,DBH_cm,Height,Lianas) %>%
    mutate(Species = tolower(Species)) %>%
    mutate(year = 2011),
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

load("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/bci.spptable.rdata")

df.match <- data.frame(sp = unique(raw.data$Species)) %>%
  left_join(bci.spptable %>%
              dplyr::select(sp,Latin),
            by = "sp") %>%
  mutate(sp.final = case_when(is.na(Latin) ~ sp,
                              TRUE ~ Latin)) %>%
  dplyr::select(sp,sp.final)


raw.data %>% group_by(year) %>%
  summarise(n())

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
                       levels = c(2019,2011,2015))) %>%
  group_by(Tag) %>%
  arrange(year) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  dplyr::select(-c(Tag,year))


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

########################################################################################################################
begum.ds <- read.csv("~/Downloads/tree_frontiers_269.csv") %>%
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
  dplyr::select(c(dbh,h,coi,sp,site,liana.cat))


# ggplot(data = begum.ds) +
#   geom_boxplot(aes(x = liana.cat, fill = liana.cat, y = h)) +
#   theme_bw()


#################################################################################
Pasoh.ds <- readRDS("./data/Pasoh/data.merged.RDS") %>%
  rename(h = height,
         liana.cat = liana.cat.consensus) %>%
  mutate(site = "Pasoh") %>%
  filter(!is.na(h),
         !is.na(dbh),
         !is.na(liana.cat)) %>%
  dplyr::select(c(dbh,h,coi,sp,site,liana.cat))

all.df <- bind_rows(list(
  BCI.ds2keep,
  Panama.ds,
  Congo.ds,
  Panama2.ds,
  begum.ds,
  Pasoh.ds,
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
  # scale_x_log10(limits = c(1,200)) +
  # scale_y_log10(limits = c(1,50)) +
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

# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/outputs/All.COI.data.RDS hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/
# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/outputs/BCI.COI.data.RDS hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/
# plot_model int
