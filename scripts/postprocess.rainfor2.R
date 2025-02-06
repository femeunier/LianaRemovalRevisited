rm(list = ls())

library(dplyr)
library(ggplot2)
library(ggthemes)

df <- readRDS("./data/rainfor2.loc.RDS") %>%
  mutate(group = substr(site,1,3)) %>%
  filter(DBH >= 10) %>%
  filter(!is.na(DBH)) %>%
  filter(!is.na(h)) %>%
  filter(!is.na(liana.cat))

meta.data <- readxl::read_xlsx("./data/FP/PITeamEmailTracker_SruthiKrishnaMoorthy_030724_complete.xlsx",
                               sheet = "LastCensus_withHeight_AND_LI")

md2keep <- meta.data %>%
  filter(PlotCode %in% sub("_","-",df$site)) %>%
  mutate(group = substr(PlotCode,1,3))

md2keep.sum <- md2keep %>%
  group_by(group) %>%
  summarise(N = n(),
            Latitude = mean(Latitude,na.rm = TRUE),
            Longitude = mean(Longitude,na.rm = TRUE),
            .groups = "keep")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

plot2keep <-
  df %>%
  group_by(group) %>%
  summarise(Ntot = n(),
            liana.cats = (length(unique(liana.cat))),
            Nhigh = length(which(liana.cat == "high")),
            Nlow = length(which(liana.cat == "low")),
            Nno = length(which(liana.cat == "no")),
            .groups = "keep") %>%
  mutate(keep = (liana.cats == 3) & (Nhigh > 10) & (Nlow > 10)
         & (Nno > 10))


md2plot <- md2keep.sum %>%
  filter(group %in% c(plot2keep %>%
                        filter(keep) %>%
                        pull(group)))

ggplot() +
  geom_sf(data = world,
          fill = NA,
          color = "black",
          alpha = 0.5) +
  geom_point(aes(x = Longitude,
                 y = Latitude,
                 color = group),
             data = md2plot, shape = 16,
             alpha = 1,
             size = 1.5) +
  scale_y_continuous(limits = c(-30,10)) +
  scale_x_continuous(limits = c(-85,-30),expand = c(0,0)) +
  scale_size_continuous(range = c(0.1, 2)) +
  labs(x = "", y = "") +
  theme_map() +
  guides(size = "none") +
  theme(text = element_text(size = 20)) +
  guides(color = "none")

saveRDS(md2plot,
        "./data/rainfor2.md.RDS")

df2keep <- df %>%
  filter(group %in% c(plot2keep %>%
                        filter(keep) %>%
                        pull(group))) %>%
  rename(dbh = DBH) %>%
  filter(h > 3)  # Weirdly small trees to remove?

summary(df2keep$h)

ggplot(data = df2keep,
       aes(x = dbh, y = h,
           color = as.factor(liana.cat))) +
  geom_point(size = 0.1) +
  scale_x_log10() +
  scale_y_log10() +
  stat_smooth(se = FALSE, method = "lm") +
  facet_wrap(~ group, scales = "free") +
  theme_bw()

saveRDS(df2keep,
        "./data/rainfor2.trees.RDS")

