rm(list = ls())

library(dplyr)
library(ggridges)
library(leaflet)
library(leafsync)
library(ggplot2)

data <- readxl::read_xlsx("./data/Panama/TreeHeightDataSmallPlotsCombined_2024-05-24.xlsx",
                          sheet = "data") %>%
  filter(is.na(Q)) # non broken stem


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
  dplyr::select(plot,dbh,h,sp,coi,liana.cat) %>%
  filter(!is.na(liana.cat),
         !is.na(dbh),
         !is.na(h)) %>%
  filter(dbh >= 10)

plots2keep <- (data.form %>%
                 group_by(plot) %>%
                 summarise(Ntot = n(),
                           Nno = sum((liana.cat == "no")),
                           Nlow = sum((liana.cat == "low")),
                           Nhigh = sum((liana.cat == "high")))) %>%
  filter(Nhigh >= 10,
         Nno >= 10)

ggplot(data = data.form %>%
         filter(plot %in% plots2keep[["plot"]]),
       aes(x = dbh, y = h, color = liana.cat,
           fill = liana.cat)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm", formula = y ~ x) +
  facet_wrap(~ as.factor(plot),nrow = 4) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

ggplot(data = data.form,
       aes(x = dbh, y = h, color = liana.cat,
           fill = liana.cat)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm", formula = y ~ x) +
  facet_wrap(~ as.factor(plot)) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()


data.form.name <- data.form %>%
  mutate(plot = case_when(plot == "PCaritas" ~ "Caritas",
                          plot == "PCharco" ~ "ElCharco",
                          plot == "PMetro1" ~ "Metrop",
                          plot == "PMetro2" ~ "Metrop2",
                          plot == "PSherman" ~ "Sherman",
                          plot == "PRoubik" ~ "Casa Roubik",
                          TRUE ~ plot))

plots <- read.csv("~/Downloads/Panama_Plot_Locations_-5492404959867224624.csv") %>%
  dplyr::select(dendroPlotname,Latitude,Longitude) %>%
  rename(plot = dendroPlotname,
         lat = Latitude,
         lon = Longitude) %>%
  dplyr::filter(plot %in% unique(sort(data.form.name$plot))) %>%
  mutate(plot.group = case_when(plot %in% c("P32") ~ "group_North",
                                plot %in% c("Casa Roubik") ~ "Casa_Roubik",
                                plot %in% c("P14","P12","P18") ~ "BCI",
                                plot %in% c("Sherman") ~ "Sherman",
                                # plot %in% c("P25","P26") ~ "South",
                                grepl("Metro",plot) ~ "group_Metro",
                                TRUE ~ "Canal"))



world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world,
          fill = NA,
          color = "darkgrey") +
  geom_point(aes(x = lon, y = lat,
                 color = plot.group),
             data = plots,
             shape = 1) +
  scale_y_continuous(limits = c(8,10)) +
  scale_x_continuous(limits = c(-80,-78)) +
  scale_size_continuous(range = c(0.1, 2)) +
  theme_bw()

data.form.name.plot <- data.form.name %>%
  left_join(plots %>%
              dplyr::select(plot,plot.group),
            by = "plot") %>%
  filter(!(dbh > 100 & h < 5)) # Weird tiny tree..

groupplots2keep <- (data.form.name.plot %>%
                 group_by(plot.group) %>%
                 summarise(Ntot = n(),
                           Nno = sum((liana.cat == "no")),
                           Nlow = sum((liana.cat == "low")),
                           Nhigh = sum((liana.cat == "high")))) %>%
  filter(Nhigh >= 10,
         Nno >= 10)


ggplot(data = data.form.name.plot %>%
         filter(plot.group %in% groupplots2keep[["plot.group"]]),
       aes(x = dbh, y = h, color = liana.cat,
           fill = liana.cat)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm", formula = y ~ x) +
  facet_wrap(~ as.factor(plot.group),nrow = 1) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()


ggplot(data = data.form.name.plot %>%
         filter(plot.group %in% c("Casa_Roubik","group_North")),
       aes(x = dbh, y = h, color = liana.cat,
           fill = liana.cat)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm", formula = y ~ x) +
  facet_wrap(~ as.factor(plot),nrow = 1) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()


ggplot(data = data.form.name.plot %>%
         filter(plot.group %in% c("Casa_Roubik","group_North")),
       aes(x = dbh, y = h, color = plot,
           fill = plot)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm", formula = y ~ x) +
  # facet_wrap(~ as.factor(plot),nrow = 1) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()


# data.form.name.plot %>%
#   filter(plot == "P32") %>%
#   group_by(liana.cat) %>%
#   summarise(N = n())

ggplot(data = data.form.name.plot, # %>%
         # filter(plot %in% c("P12","P14","P18"))
         # filter(plot %in% c("P32")),
       aes(x = dbh, y = h, color = liana.cat,
           fill = liana.cat)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm", formula = y ~ x) +
  facet_wrap(~ as.factor(plot)) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

data.form.name.plot %>%
  filter(plot == "P32") %>%
  group_by(liana.cat) %>%
  summarise(n())



cols <- c("blue","red","black","yellow","pink","brown")
plots2show <- plots %>%
  mutate(color = cols[as.numeric(as.factor(plot.group))])

leaflet() %>%
  addCircleMarkers(plots2show$lon,
                   plots2show$lat,
                   radius = 1,
                   opacity = 0.4,
                   color = plots2show$color)  %>%
  addScaleBar(options = list(imperial = FALSE)) %>%
  addProviderTiles(providers$Esri.WorldImagery)


DB <- read.csv("~/Downloads/TurnerCondit-PlotSoilClimate-2022.tsv",sep = "\t")

DB.select <- DB %>%
  filter(tolower(Plot.code) %in% c("bci","gigante1","gigante2",tolower(plots[["plot"]])))

hist(DB.select$AnnualPpt)

saveRDS(data.form.name.plot,
        "./outputs/Data.otherplots.Panama.RDS")

plots.prop <- plots %>%
  mutate(plot = tolower(plot)) %>%
  left_join(DB.select %>%
              dplyr::select(Plot.code,AnnualPpt) %>%
              rename(plot = Plot.code) %>%
              mutate(plot = tolower(plot)),
            by = "plot")


ggplot() +
  geom_sf(data = world,
          fill = NA,
          color = "darkgrey") +
  geom_point(aes(x = lon, y = lat,
                 color = AnnualPpt),
             data = plots.prop,) +
  scale_y_continuous(limits = c(8,10)) +
  scale_x_continuous(limits = c(-80,-78)) +
  scale_size_continuous(range = c(0.1, 2)) +
  theme_bw()


ggplot() +
  geom_point(aes(x = lon, y = AnnualPpt,
                 color = plot.group),
             data = plots.prop) +
  scale_color_manual(values = c("blue","red","black","yellow","pink","brown")) +
  theme_bw()

plots.prop %>%
  filter(AnnualPpt <= 2200,
         lon <= -79.75)

saveRDS(plots,"./outputs/plots.panama.RDS")
