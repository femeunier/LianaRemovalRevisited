rm(list = ls())


all.df <- readRDS("./outputs/All.COI.data.RDS") %>%
                      mutate(sp = str_squish(sp)) %>%
                      filter(dbh >= 10) %>%
  filter(site == "Gigante") %>%
  mutate(sp = tolower(sp))

WDdata <- read.csv("/home/femeunier/Documents/projects/LVLRE.long/data/WoodDens_20130917.csv")

WD.db <- WDdata %>%
            mutate(sp = tolower(paste(GENUS., SPECIES.))) %>%
            dplyr::select(sp,SG100C_AVG) %>%
            group_by(sp) %>%
            summarise(WD = mean(SG100C_AVG,na.rm = TRUE))

all.df.WD <- all.df %>%
  left_join(WD.db,
            by = "sp")

ggplot(data = all.df.WD) +
  geom_boxplot(aes(x = as.factor(liana.cat),
                   y = WD, fill = as.factor(liana.cat))) +
  theme_bw()
