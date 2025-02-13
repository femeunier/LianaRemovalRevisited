rm(list = ls())

library(xlsx)
library(dplyr)
library(ggplot2)
library(tidyr)

sheets <- c("2014","2016","2018","2022")

# A <- readxl::read_xlsx("./data/Amazon/240531 Liana data to Sruthi.xlsx",
#                        sheet = "2022")
# Aok <- A %>%
#   filter(Forma == "A") %>%
#   rename(plot = `Plot code`,
#          type = `Forest type`,
#          ID = `Tree code`,
#          dbh = DAP2022_average,
#          coi = COI2022,
#          h = Altura2022) %>%
#   mutate(sp = paste(Gênero,Espécie),
#          h = as.numeric(h),
#          coi = as.numeric(coi),
#          year = 2022) %>%
#   mutate(liana.cat = case_when(coi == 0 ~ "no",
#                                coi <= 50 ~ "low",
#                                coi <= 100 ~ "high")) %>%
#   dplyr::select(year,type,plot,ID,dbh,coi,h,sp,liana.cat)
#
#
# B <- readxl::read_xlsx("./data/Amazon/240531 Liana data to Sruthi.xlsx",
#                        sheet = "2018")
# Bok <- B %>%
#   filter(`Plant form` == "A") %>%
#   rename(plot = `Plot code`,
#          type = `Forest type`,
#          ID = `Tree code`,
#          dbh = DAP2018_average,
#          coi = COI2018,
#          h = Altura2018) %>%
#   mutate(sp = paste(Gênero,Espécie),
#          h = as.numeric(h),
#          coi = as.numeric(coi),
#          year = 2018) %>%
#   mutate(liana.cat = case_when(coi == 0 ~ "no",
#                                coi <= 50 ~ "low",
#                                TRUE ~ "high")) %>%
#   dplyr::select(year,type,plot,ID,dbh,coi,h,sp,liana.cat)
#
#
# C <- readxl::read_xlsx("./data/Amazon/240531 Liana data to Sruthi.xlsx",
#                        sheet = "2016")
# Cok <- C %>%
#   filter(`Plant form` == "A") %>%
#   rename(plot = `Plot code`,
#          type = `Forest type`,
#          ID = `Tree code`,
#          dbh = `DBH 2016 (average)`,
#          coi = COI2016,
#          h = Altura2016) %>%
#   mutate(sp = paste(Gênero,Espécie),
#          h = as.numeric(h),
#          coi = as.numeric(coi),
#          year = 2016) %>%
#   mutate(liana.cat = case_when(coi == 0 ~ "no",
#                                coi <= 50 ~ "low",
#                                TRUE ~ "high")) %>%
#   dplyr::select(year,type,plot,ID,dbh,coi,h,sp,liana.cat)
#
#
#

D <- readxl::read_xlsx("./data/Amazon/240531 Liana data to Sruthi.xlsx",
                       sheet = "2014")
Dok <- D %>%
  filter(`Plant form` == "A") %>%
  rename(plot = `Plot code`,
         type = `Forest type`,
         ID = `Tree code`,
         dbh = `DBH 2014 (average)`,
         coi = COI2014,
         h = `Corrected height`) %>%
  mutate(sp = paste(Genus,Spevies),
         h = as.numeric(h),
         coi = as.numeric(coi),
         year = 2014) %>%
  mutate(liana.cat = case_when(coi == 0 ~ "no",
                               coi <= 50 ~ "low",
                               TRUE ~ "high")) %>%
  dplyr::select(year,type,plot,ID,dbh,coi,h,sp,liana.cat,Notes)

df.all <- bind_rows(
  # Aok,
  # Bok,
  # Cok,
  Dok
  ) %>%
  filter(!is.na(coi),!is.na(dbh),!is.na(h)) %>%
  filter(dbh > 10) %>%
  filter(h > 0) %>%
  mutate(site = as.numeric(sub("\\_.*", "", plot))) %>%
  filter(!(Notes %in%
             c("Copa quebrada 100%")))


ggplot(data = df.all,
       aes(x = dbh,
           y = h,
           color = liana.cat,
           fill = liana.cat)) +
  geom_point() +
  stat_smooth(method = "lm") +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ year) +
  theme_bw()

df.single <- df.all %>%
  mutate(type_site = paste0(type,'_',site)) %>%
  arrange(desc(year)) %>%
  group_by(ID) %>%
  slice_head(n = 1)

plots2keep <- df.single %>%
  group_by(type,site,liana.cat) %>%
  summarise(N = n(),
            .groups = "keep") %>%
  pivot_wider(names_from = liana.cat,
              values_from = N) %>%
  filter(no > 10 & low > 10 &
           high > 10 & (no + low + high) > 50) %>%
  mutate(type_site = paste0(type,'_',site))

ggplot(data = df.single %>%
         filter(type_site %in% plots2keep[["type_site"]]),
       aes(x = dbh,
           y = h,
           color = liana.cat,
           fill = liana.cat)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm") +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ type_site) +
  theme_bw()

df.single %>%
  filter(type_site %in% plots2keep[["type_site"]]) %>%
  group_by(h) %>%
  summarise(N = n()) %>%
  arrange(desc(N))

nrow(df.single)
df.single %>%
  group_by(liana.cat) %>%
  summarise(N = n(),
            .groups = "keep")

df.single %>%
  ungroup() %>%
  dplyr::select(plot,type) %>%
  distinct()

df.single %>%
  group_by(liana.cat,type) %>%
  summarise(n())

saveRDS(df.single %>%
          filter(type_site %in% plots2keep[["type_site"]]),
        "./outputs/Erika_data.RDS")

