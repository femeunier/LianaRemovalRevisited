rm(list = ls())

library(dplyr)
library(ggplot2)


raw.data.2019 <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/2019_TLS_data.csv",stringsAsFactors = FALSE) %>%
  rename(dbh = Final.DBH..cm.,
         CA = CPA..m2.) %>%
  mutate(liana.cat = case_when(Liana == 0 ~ "no",
                               Liana == 1 ~ "low",
                               Liana == 2 ~ "high"))

tags <- raw.data.2019 %>% pull(Tag)


data <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/new_height_liana_data_2015_no_outliers_q20.csv",stringsAsFactors = FALSE) %>%
  dplyr::select(Tag,Species,DBH_cm,Height,Lianas,Crown.area, Illumination) %>%
  mutate(liana.num = case_when(Lianas == 0 ~ 0,
                               Lianas < 3 ~ 1,
                            TRUE ~ 2)) %>%
  mutate(liana.cat = factor(case_when(liana.num == 0 ~ "no",
                               liana.num == 1 ~ "low",
                               TRUE ~ "high"),
                            levels = c("no","low","high"))) %>%
  rename(dbh = DBH_cm,
         CA = Crown.area)

data.comp <- data %>%
  rename(dbh0 = dbh,
         CA0 = CA) %>%
  left_join(raw.data.2019 %>%
              dplyr::select(Tag,dbh,Height..m.,Liana,CA),
            by = "Tag")

ggplot(data = data.comp %>%
         filter(!is.na(CA)),
       aes(x = CA0, y = CA, color = as.factor(Liana), fill = as.factor(Liana))) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  stat_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "black") +
  theme_bw()

ggplot(data = data.comp %>%
         filter(!is.na(CA)),
       aes(x = dbh0, y = dbh, color = as.factor(Liana), fill = as.factor(Liana))) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  stat_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "black") +
  theme_bw()

data.f <- data %>%
  filter(Tag %in% tags)

tags[!(tags %in% data$Tag)]

all.data <- bind_rows(
  list(data.f %>%
         mutate(source =  "Subset common 2015-2019"),
       data %>%
         mutate(source = "Full dataset 2015"),
       data %>%
         filter(Illumination >= 4) %>%
         mutate(source = "Subset 2015 Ill. >= 4"),
       raw.data.2019 %>%
         dplyr::select(Tag,dbh,CA,liana.cat) %>%
         mutate(source = "Dataset 2019"))) %>%
  mutate(source = factor(source,
                         levels = c("Full dataset 2015",
                                    "Subset common 2015-2019",
                                    "Subset 2015 Ill. >= 4",
                                    "Dataset 2019")))

table(all.data$source)
table(data$Illumination)

all.data %>%
  group_by(liana.cat,Illumination) %>%
  summarise(N = n()) %>%
  group_by(liana.cat) %>%
  mutate(frac = round(N/sum(N)*100,1))

ggplot(all.data,
       aes(x = dbh, y = CA, color = as.factor(liana.cat),
           fill = as.factor(liana.cat))) +
  geom_point() +
  stat_smooth(method = "lm",
              # formula = log(y) ~ log(x),
              se = FALSE) +
  facet_wrap(~ source, nrow = 1) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "DBH (cm)", y = "Crown area (m²)", color = "Liana infestation",
       fill = "Liana infestation") +
  theme_bw() +
  theme(legend.position = c(0.1,0.87))

