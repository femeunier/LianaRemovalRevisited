rm(list = ls())

library(stringr)
library(dplyr)
library(ggplot2)
library(readxl)

data <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/Afritron/africa_individualsdata_mainplotview_112017.csv",stringsAsFactors = FALSE) %>%
  filter(F5 > 1)

saveRDS(data %>%
  filter(Plot.Code %in%
           c("ASN-02","ASN-04",
             "CVL-01","CVL-11",
             "DAD-04","EKO-01",
             "GBO-04","GBO-11",
             "GBO-15","GBO-19",
             "LKM-01","OGI-01")) %>%
  dplyr::select(Plot.Code,PlotViewID) %>%
  distinct(),
  "./outputs/plot.vs.plotviewID")

data.filt <- data %>%
  mutate(coi = as.numeric(substr(LI,1,1)) ) %>%
  filter(!is.na(D4),
         !is.na(Height),
         !is.na(coi)) %>%
  mutate(liana.cat = case_when(coi == 0 ~ "no",
                               coi < 3 ~ "low",
                               TRUE ~ "high"))

data.filt %>%
  filter(D4 >= 100) %>%
  group_by(Country) %>%
  summarise(N = n())

ggplot(data = data.filt %>%
         filter(D4 >= 100),
       aes(x = D4 , y = Height, color = liana.cat)) +
  geom_point() +
  stat_smooth(method = "lm",
              se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ Country) +
  theme_bw()

plots <- data.filt %>%
  filter(D4 >= 100) %>%
  pull(Plot.Code) %>%
  unique() %>% sort()

metadata.file <- "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/Afritron/africa_metadataforR_112017_extra.xlsx"
metadata <- read_xlsx(metadata.file) %>%
  right_join(data.frame(PlotCode = plots),
             by = "PlotCode") %>%
  mutate(origin = "Afritron") %>%
  dplyr::select(origin,PlotCode,ForestMoistureName,ForestElevationName,ForestEdaphicName,`Forest status plotview`) %>%
  arrange(PlotCode) %>%
  distinct()

write.csv(metadata,
          "~/Downloads/test.csv")

# ggplot(data = data.filt %>%
#          filter(D4 >= 100,
#                 grepl("Noubale-Ndoki",Plot.Name)),
#        aes(x = D4 , y = Height, color = liana.cat)) +
#   geom_point() +
#   stat_smooth(method = "lm",
#               se = FALSE) +
#   scale_x_log10() +
#   scale_y_log10() +
#   facet_wrap(~ Country) +
#   theme_bw()


data.filt %>%
  filter(D4 >= 100) %>%
  dplyr::select(Country,Plot.Name) %>%
  distinct()

data.format <- data.filt %>%
  filter(F2 == "1") %>%
  group_by(PlotID,TreeID) %>%
  arrange(desc(Census.No)) %>%
  slice_head(n = 1) %>%
  filter(!grepl("b|c",F1)) %>%
  ungroup() %>%
  rename(dbh = D4,
         h = Height,
         sp = Species) %>%
  dplyr::select(dbh,h,sp,PlotID,coi,liana.cat,Plot.Code)

saveRDS(data.format,
        "./data/Afritron.RDS")

# Check overlaps with Afritron!
rainfordb <- read_xlsx("./data/Afritron/ForestPlots_Access_FelicienMunier_19Oct23.xlsx") %>%
  mutate(accessDate = as.Date(ExpirationDate)) %>%
  filter(accessDate < as.Date("2026-10-10"))
plotcodes <- sort(unique(rainfordb$PlotCode))


Afritondb <- read_xlsx("./data/Afritron/ForestPlots_Access_FelicienMunier_19Oct23.xlsx") %>%
  mutate(accessDate = as.Date(ExpirationDate)) %>%
  filter(accessDate > as.Date("2026-10-10"))
plotcodes <- unique(Afritondb$PlotCode)


# In Wannes'DB but not in Afritron
unique(data.format$Plot.Code)[!(unique(data.format$Plot.Code) %in% Afritondb$PlotCode)] # Only MMI --> not used anyway

# InAfritron but not in Wannes'
unique(Afritondb$PlotCode)[!(unique(Afritondb$PlotCode) %in% data.format$Plot.Code)] # Empty



# check if the DB are exactly the same

Plot <- "SNG-01"

tmp <- read_xlsx("~/Downloads/SNG_01_Census_3_PlotDump.xlsx", sheet = "Data") %>%
  mutate(liana.cat = case_when(LI == 0 ~ "no",
                               LI < 3 ~ "low",
                               LI < 5 ~ "high",
                               TRUE ~ NA_character_))

data2plot <- bind_rows(
  tmp %>%
  filter(!is.na(D4),!is.na(Height),!is.na(LI)) %>%
  mutate(DBH = D4/10,
         Height=as.numeric(Height)) %>%
  dplyr::select(DBH,Species,Height,liana.cat) %>%
    mutate(origin = "Afritron"),
  data.format %>% filter(Plot.Code == Plot) %>%
    mutate(DBH = dbh/10,
           origin = "Wannes") %>%
    rename(Species = sp,
           Height = h) %>%
    dplyr::select(DBH,Species,Height,liana.cat,origin)
)



ggplot(data = data2plot,
       aes(x = DBH, y = as.numeric(Height), color = as.factor(liana.cat))) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  stat_smooth(method = "lm") +
  facet_wrap(~ origin) +
  theme_bw()

data2plot %>%
  group_by(liana.cat,origin) %>%
  summarise(N = n(),
            .groups = "keep")

