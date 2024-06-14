rm(list = ls())

library(dplyr)
library(ggplot2)
library(readxl)

data <- readxl::read_xlsx("./data/Panama/TreeHeightDataSmallPlotsCombined.xlsx",sheet = "data")

data.form <- data %>%
  rename(plot = Plotname,
         dbh = PaintDBH,
         h = TreeHt,
         sp = LatinBinomial,
         coi = Lianas) %>%
  mutate(dbh = dbh/10) %>%
  mutate(liana.cat = case_when(coi == 0 ~ "no",
                               coi < 3 ~ "low",
                               coi < 5 ~ "high",
                               TRUE ~ NA_character_)) %>%
  filter(!is.na(dbh),
         !is.na(h),
         !is.na(coi)) %>%
  dplyr::select(plot,dbh,h,sp,coi,liana.cat) %>%
  filter(dbh >= 10)

ggplot(data = data.form,
       aes(x = dbh, y = h,
           color = liana.cat,
           fill = liana.cat)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm",se = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

ggplot(data = data.form,
       aes(x = dbh, y = h,
           color = liana.cat,
           fill = liana.cat)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm",se = TRUE) +
  facet_wrap(~ as.factor(plot)) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()
