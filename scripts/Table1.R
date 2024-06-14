rm(list = ls())

library(brms)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

# Data

all.df <- bind_rows(readRDS("./outputs/All.COI.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10),
  readRDS("./outputs/All.COI.data.RDS") %>%
    mutate(sp = str_squish(sp)) %>%
    filter(dbh >= 10) %>%
    mutate(site = "Total"))

# Best models
check.all.diagnosis <- readRDS("./outputs/check.all.diagnosis.RDS")

sites <- unique(all.df$site)
sites.fac <- c(sites[order(sites[sites != "Total"])],"Total")

# Model predictions
Model.predictions <- readRDS("./outputs/Main.OP.50.RDS")  %>%
  group_by(site,liana.cat) %>%
  summarise(m = round(100*mean(diff_h/no,na.rm = TRUE),digits = 1),
            CIlow = round(100*quantile(diff_h/no,0.11/2,na.rm = TRUE),digits = 1),
            CIhigh = round(100*quantile(diff_h/no,1-0.11/2,na.rm = TRUE),digits = 1),
            .groups = "keep")

Model.predictions %>%
  group_by(liana.cat) %>%
  summarise(Nneg = sum(m < 0),
            Nneg.sign = sum(CIhigh < 0 & m < 0),
            Npos = sum(m > 0),
            Npos.sign = sum(CIlow > 0 & m > 0),
            Nnull = sum(m==0))

check.all.diagnosis %>%
  filter(site %in% c(Model.predictions %>%
                       filter(liana.cat == "high",
                              m > 0) %>%
  pull(site)))

Nvseffect <- Model.predictions %>%
  left_join(check.all.diagnosis)

Nvseffect.wide <- Nvseffect %>%
  pivot_wider(names_from = liana.cat,
              values_from = c(m,CIlow,CIhigh))

ggplot(data = Nvseffect.wide,
       aes(x = m_low, y = m_high)) +
  geom_point() +
  geom_errorbar(aes(ymin = CIlow_high, ymax = CIhigh_high)) +
  geom_errorbarh(aes(xmin = CIlow_low, xmax = CIhigh_low)) +
  stat_smooth(method = "lm", color = "black") +
  geom_abline(slope = 1,intercept = 0, color = "black", linetype = 2) +
  theme_bw() +
  labs(x = "",y = "") +
  scale_x_continuous(limits = c(-40,25)) +
  scale_y_continuous(limits = c(-40,25)) +
  coord_equal() +
  theme(text = element_text(size = 24))

summary(lm(data = Nvseffect.wide %>%
     filter(site != "Total"),
   m_high ~ m_low))

Nvseffect <- Model.predictions %>%
  filter(site != "Total") %>%
  left_join(all.df %>%
              filter(dbh > 10) %>%
              group_by(site,liana.cat) %>%
              summarise(N = n(),
                        .groups = "keep") %>%
              pivot_wider(names_from = liana.cat,
                          values_from = N) %>%
              mutate(low = low,
                     high = high) %>%
              dplyr::select(-c(no)) %>%
              pivot_longer(cols = c(low,high),
                           names_to = "liana.cat",
                           values_to = "N"),
            by = c("liana.cat","site")) %>%
  arrange(desc(N))

Nvseffect %>%
  filter(liana.cat == "high")


Nvseffect %>%

  arrange(m)

Nvseffect %>%
  ungroup() %>%
  group_by(liana.cat) %>%
  filter(m < 0) %>%
  summarise(m.mean = mean(m),
            m.min = min(m),
            m.max = max(m))

Nvseffect %>%
  filter(m < 0) %>%
  group_by(liana.cat) %>%
  filter((m == min(m) | m == max(m)))

ggplot(data = Nvseffect,
       aes(x = log10(N),y = m, color = liana.cat, fill = liana.cat)) +
  geom_point() +
  stat_smooth(method = "lm", alpha = 0.2) +
  scale_x_continuous(limits = c(1,4)) +
  labs(x = "", y  = "", color = "", fill = "") +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw() +
  theme(text = element_text(size = 24),legend.position = "none") +
  scale_fill_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred")) +
  scale_color_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred"))


Nvseffect2 <- Model.predictions %>%
  filter(site != "Total") %>%
  left_join(all.df %>%
              filter(dbh >= 10) %>%
              group_by(site,liana.cat) %>%
              summarise(N = n(),
                        .groups = "keep") %>%
              pivot_wider(names_from = liana.cat,
                          values_from = N) %>%
              rowwise() %>%
              mutate(frac.low = low/sum(c(no,low,high)),
                     frac.high = high/sum(c(no,low,high))) %>%
              dplyr::select(-c(no,low,high)) %>%
              rename(low = frac.low,
                     high = frac.high) %>%
              pivot_longer(cols = c(low,high),
                           names_to = "liana.cat",
                           values_to = "N"),
            by = c("liana.cat","site")) %>%
  arrange(desc(N))


ggplot(data = Nvseffect2,
       aes(x = 1*(N),y = m, color = liana.cat, fill = liana.cat)) +
  geom_point() +
  stat_smooth(method = "lm", alpha = 0.2) +
  scale_x_continuous(limits = c(0,0.7)) +
  labs(x = "", y  = "", color = "", fill = "") +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw() +
  theme(text = element_text(size = 24),legend.position = "none") +
  scale_fill_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred")) +
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred"))

summary(lm(data = Nvseffect %>%
     filter(liana.cat == "low"),
   formula = m ~N))


# Coordinates
locations <-readRDS("./outputs/all.df.md2plot.RDS") %>%
  mutate(lat = round(lat,digits = 1),
         lon = round(lon,digits = 1),) %>%
  rename(site = site.common)

Table1 <- all.df %>%
  ungroup() %>%
  mutate(site = factor(site,
                       levels = sites.fac)) %>%
  arrange(site) %>%
  group_by(site) %>%
  summarise(minDBH = round(min(dbh),digits = 1),
            maxDBH = round(max(dbh),digits = 1),
            minH = round(min(h),digits = 1),
            maxH = round(max(h),digits = 1),
            N = length(sp),
            Nno = length(sp[liana.cat == "no"]),
            Nlow = length(sp[liana.cat == "low"]),
            Nhigh = length(sp[liana.cat == "high"])) %>%
  mutate(dbh.range = paste0(minDBH," - ",maxDBH),
         h.range = paste0(minH," - ",maxH),
         num = paste0(N," \r\n (",Nno," - ",Nlow," - ",Nhigh,")")) %>%
  dplyr::select(site,num,dbh.range,h.range) %>%
  left_join(check.all.diagnosis %>%
              rename(best.model = model.name) %>%
              dplyr::select(site,best.model),
            by = "site") %>%
  mutate(model = sub("\\_.*", "", best.model),
         fe =  sub(".*\\_", "", best.model)) %>%
  mutate(model = case_when(model == "gmm" ~ "gMM",
                           model == "weibull" ~ "Weibull",
                           model == "power" ~ "power")) %>%
  mutate(fe = case_when(fe == "none" ~ "",
                        fe == "ab" ~ "(a, b)",
                        fe == "ak" ~ "(a, k)",
                        fe == "bk" ~ "(b, k)",
                        fe == "all" ~ "(a, b, k)",
                        TRUE ~ paste0("(",fe,")"))) %>%
  mutate(best.model = case_when(fe == "" ~ model,
                                TRUE ~ paste0(model, " \r\n ",fe))) %>%
  dplyr::select(-c(model,fe)) %>%

  left_join(Model.predictions %>%
              mutate(delta_h = paste0(m," \r\n (",min(CIlow,CIhigh),", ",max(CIhigh,CIlow),")")) %>%
              dplyr::select(-c(m,CIlow,CIhigh)) %>%
              pivot_wider(names_from = liana.cat,
                          values_from = delta_h),
            by = "site")  %>%
  left_join(locations %>%
              dplyr::select(site,lat,lon) %>%
              mutate(coord = paste0(abs(lat),ifelse(sign(lat) == 1, "°N - ","°S - "),
                                    abs(lon),ifelse(sign(lon) == 1, "°E","°W"))) %>%
              mutate(coord = case_when(is.na(coord) ~ "",
                                       TRUE ~ coord)) %>%
              dplyr::select(c(site,coord,lat,lon)),
            by = "site") %>%
  dplyr::select(site,coord,num,dbh.range,h.range,best.model,low,high,lat,lon) %>%
  mutate(low = case_when(best.model %in% c("gMM","Weibull","power") ~"0",
                         TRUE ~ low),
         high = case_when(best.model %in% c("gMM","Weibull","power") ~"0",
                         TRUE ~ high)) %>%
  mutate(site = case_when(site %in% c("BCI","Loudoungou","Danum Valley") ~ paste0(site,"*"),
                          TRUE ~ site)) %>%
  mutate(site = case_when(site == "Kisangani_all" ~ "Yoko",
                          site == "ALF" ~ "Parque Cristalino",
                          site == "FLO" ~ "Fazenda Floresta",
                          site == "FRP" ~ "Fazenda Rio Preto",
                          site == "GAU" ~ "Gaúcha do Norte",
                          site == "OKU" ~ "Oku",
                          site == "PEA" ~ "Parque Estadual do Araguaia",
                          site == "POA" ~ "Porto Alegre do Norte",
                          site == "SAA" ~ "Santana do Araguaia",
                          site == "SAT" ~ "Santa Terezinha",
                          site == "SIP" ~ "Sinop",
                          site == "TAN" ~ "Tanguro",
                          site == "VCR" ~ "Vera Cruz",
                          TRUE ~ site))


sites <- unique(Table1$site)
sites.fac <- c(sites[order(sites[sites != "Total"])],"Total")


coords2country = function(points)
{
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail

  # convert our list of points to a SpatialPoints object

  # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))


  # use 'over' to get indices of the Polygons object containing each point
  indices = over(pointsSP, countriesSP)

  # return the ADMIN names of each country
  indices$ADMIN
  #indices$ISO3 # returns the ISO3 code
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}

library(sp)
library(rworldmap)

Table <- Table1 %>%
  mutate(site = factor(as.character(site),levels = sites.fac)) %>%
  arrange(site)


countries <- coords2country(Table %>%
                 filter(site != "Total") %>%
                 dplyr::select(lon,lat))
Table[["country"]] <- c(as.character(countries),NA)

stop()
write.csv(Table %>%
            mutate(site = paste0(site," \r\n (",country,")")) %>%
            dplyr::select(-c(lat,lon,country)),
          "./outputs/Table1.csv")


Model.predictions <- readRDS("./outputs/Model.predictions.RDS")

total <- Model.predictions %>%
  filter(site == "Total") %>%
  group_by(dbh,liana.cat) %>%
  mutate(id = 1:length(id)) %>%
  dplyr::select(dbh,liana.cat,id,h.pred.m)


total %>%
  pivot_wider(names_from = liana.cat,
              values_from = c(h.pred.m)) %>%
  ungroup() %>%
  pivot_longer(cols = c(low, high),
               names_to = "liana.cat") %>%
  group_by(liana.cat) %>%
  summarise(
    m = 100*mean((value - no)/no,na.rm = TRUE),
    low = 100*quantile((value-no)/no, 0.11/2,na.rm = TRUE),
    high = 100*quantile((value-no)/no, 1- 0.11/2,na.rm = TRUE),
  )

