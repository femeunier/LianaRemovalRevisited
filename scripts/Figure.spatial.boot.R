rm(list = ls())

library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggridges)
library(RColorBrewer)
library(tidyr)

N = 24

all.sum <- data.frame()
all.stats <- data.frame()
for (i in seq(1:N)){

  print(i/N)
  cdf <- readRDS(paste0("./outputs/Bootstrap_spatial.",i,".RDS")) %>%
    mutate(continent = case_when(lon <= -35 ~ "America",
                                 lon <= 50 ~ "Africa",
                                 TRUE ~ "Australasia")) %>%
    mutate(continent = factor(continent,
                              levels = c("America","Africa","Australasia","Total")))


  cdf.tot <- bind_rows(cdf %>%
                         filter(AGB.av > 0),
                       cdf %>%
                         filter(AGB.av > 0) %>%
                         ungroup() %>%
                         mutate(continent = "Total")) %>%
    mutate(Tot.loss = area*delta_AGB) # kgC/m²*m² = kgC

  cdf.stat <- cdf.tot %>%
    group_by(continent,map,ID) %>%
    summarise(m = mean(delta_bound_av,na.rm = TRUE),
              loss.area = 20*mean(delta_AGB,
                                  na.rm = TRUE),
              loss = sum(Tot.loss,na.rm = TRUE)*1000/1e15,
              Tot = sum(area*AGB.av,na.rm = TRUE)*1000/1e15,
              .groups = "keep") %>%
    mutate(diff.rel = loss/Tot)

  all.stats <- bind_rows(all.stats,
                         cdf.stat)

  cdf.sum <- cdf %>%
    group_by(lat,lon) %>%
    summarise(delta_bound_av = mean(delta_bound_av,na.rm = TRUE),
              delta_AGB = mean(delta_AGB,na.rm = TRUE),
              AGB.av = mean(AGB.av,na.rm = TRUE),
              area = mean(area,na.rm = TRUE),
              .groups = "keep")

  all.sum <- bind_rows(all.sum,
                       cdf.sum)

}

all.sum.sum <- all.sum %>%
  group_by(lat,lon) %>%
  summarise(delta_bound_av = mean(delta_bound_av,na.rm = TRUE),
            delta_AGB = mean(delta_AGB,na.rm = TRUE),
            AGB.av = mean(AGB.av,na.rm = TRUE),
            area = mean(area,na.rm = TRUE),
            .groups = "keep") %>%
  mutate(continent = case_when(lon <= -35 ~ "America",
                               lon <= 50 ~ "Africa",
                               TRUE ~ "Australasia")) %>%
  mutate(continent = factor(continent,
                            levels = c("America","Africa","Australasia")))

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
lat.max = 12 ; lat.min = -31

ggplot() +
  geom_tile(data = all.sum.sum,
              aes(x = lon, y = lat,fill = delta_AGB*20),
              alpha = 1) +

  scale_fill_gradient2(low = "darkred",mid = "white",high = "darkgreen",
                       na.value = "lightgrey",breaks = c(-50,-25,0),
                       limits = c(-2.5,0.1)*20,
                       oob=scales::squish) +
  geom_sf(data = world,
          fill = NA,
          color = "black",
          alpha = 0.5, size = 0.5) +

  # geom_point(data = readRDS("./outputs/site.loc.RDS"),
  #            aes(x = lon, y = lat, size = Nlarge),shape = 1) +

  coord_sf(xlim = c(-120, 170), ylim = c(lat.min, lat.max), expand = FALSE) +
  theme_void() +
  labs(fill = "") +
  guides(size = "none") +
  theme(text = element_text(size = 14),
        legend.position = "bottom")


cols<-brewer.pal(3,"Dark2")

ggplot() +

  geom_density(data = all.sum.sum %>%
                 filter(delta_AGB != 0),
               aes(x = delta_AGB*20,
                   fill = continent),alpha = 0.5, color = NA) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
  scale_x_continuous(limits = c(-50,5)) +
  scale_y_continuous(breaks = c(), labels = c("")) +
  labs(x = "",y = "", fill = "") +
  theme(text = element_text(size = 24)) +
  guides(fill = "none")

alpha = 0.11
all.stats %>%
  group_by(continent) %>%
  summarise(m.low = quantile(m,alpha/2),m.high = quantile(m,1-alpha/2),m = mean(m),
            loss.area.low = quantile(loss.area,alpha/2),loss.area.high = quantile(loss.area,1-alpha/2),loss.area = mean(loss.area),
            .groups = "keep")

all.stats %>%
  group_by(continent) %>%
  summarise(loss.low = quantile(loss,alpha/2),loss.high = quantile(loss,1-alpha/2),loss = mean(loss),
           diff.rel.low = quantile(diff.rel,alpha/2),diff.rel.high = quantile(diff.rel,1-alpha/2),diff.rel = mean(diff.rel),
            .groups = "keep")

load("./outputs/VarPart.RData")

mycolors = c(brewer.pal(name="Blues", n = 4)[c(3,4)],
             brewer.pal(name="Greens", n = 4)[c(1,2)],
             brewer.pal(name="Oranges", n = 4)[c(1,3)],
             brewer.pal(name="Reds", n = 4)[c(3,4)])

W = 1
ggplot() +
  geom_bar(data = data.frame(op = c("op","op2","op3"),
                             value = c(100,100,100)),
           aes(x=op, y=value, width=W),
           fill = NA, color = "black",
           stat = "identity") +
  geom_bar(data= df.all,
           aes(x=op, y=value, fill=long.name, width=W),
           stat="identity") +
  # scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_manual(values = mycolors) +
  theme_minimal() +
  facet_wrap(~ op, scales = "free") +
  labs(x = "",y = "") +
  theme(panel.grid.major.x = element_blank()) +
  guides(fill = "none") +
  theme(text = element_text(size = 20))

df2plot <- all.stats %>%
  filter(continent == "Total") %>%
  dplyr::select(map,m,loss,diff.rel) %>%
  pivot_longer(cols = c(m,loss,diff.rel),
               names_to = "variable",
               values_to = "value")

df1 <- df2plot %>%
  filter(variable == "loss") %>%
  group_by(map)

df2 <- df2plot %>%
  filter(variable != "loss")

alpha = 0.11


ggplot() +
  stat_density_ridges(data = df1,
                      aes(x = value,y = 0,fill = factor(stat(quantile))),
                      alpha = 0.1,color = NA,
                      geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      quantiles = c(alpha/2,1-alpha/2), quantile_lines = TRUE) +
  stat_density_ridges(data = df2,
                      aes(x = value,y = 0,fill = factor(stat(quantile))),
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    alpha = 0.5,color = NA,
    quantiles = c(alpha/2,1-alpha/2), quantile_lines = TRUE) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal()  +
  scale_fill_manual(values = c("lightgrey","darkgrey","lightgrey")) +
  guides(fill = "none") +
  scale_y_continuous(breaks = c(),
                     labels = c()) +
  theme(text = element_text(size = 16),
        strip.background = element_blank(),
        panel.spacing = unit(2, "lines"),
        strip.text = element_blank()) +
  labs(x = "",y = "")




