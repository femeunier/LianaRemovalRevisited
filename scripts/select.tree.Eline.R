rm(list = ls())

library(sf)
library(dplyr)
library(rgdal)
library(raster)
library(ggplot2)
library(BCI.AGB)
library(xlsx)
library(caret)

raw.data <- readRDS("./outputs/rawdata.RDS")
newCOI <- readRDS("./outputs/newCOI.RDS")

merged.ds <- readRDS("./outputs/census.RDS") %>%
  mutate(Tag = as.numeric(tag)) %>%
  mutate(dbh.census = dbh/10,
         sp.code = sp) %>%
  filter(Tag %in% raw.data[["Tag"]]) %>%
  dplyr::select(Tag,sp.code,quadrat,gx,gy,dbh.census,hom,ExactDate,status,nostems,agb,ba) %>%
  left_join(raw.data %>%
              rename(dbh.tls = dbh) %>%
              dplyr::select(-c(year,site,coi)),
            by = "Tag") %>%
  left_join(newCOI,
            by = "Tag") %>%
  mutate(liana.cat.x = factor(liana.cat.x,
                              levels = c("no","low","high")),
         liana.cat.y = factor(liana.cat.y,
                              levels = c("no","low","high")))

merged.confusion <- merged.ds %>%
  dplyr::filter(!is.na(liana.cat.x),
                !is.na(liana.cat.y))

confusionMatrix(data=merged.confusion$liana.cat.x,
                reference = merged.confusion$liana.cat.y)

plotcenters <- read.table("~/Downloads/bci_tls_2019_subplotcenters (1).txt",
                          header = TRUE)
newplotcenters <- data.frame(x = c(650,870,710),
                             y = c(450,270,230))

plot(merged.ds$dbh.census,merged.ds$dbh.tls)

ggplot(data = merged.ds) +
  geom_point(aes(x = gx,y = gy, size = dbh.census,
                 color = sp)) +
  geom_rect(data = plotcenters,
            aes(xmin = x - 20, xmax = x + 20,
                ymin = y - 20, ymax = y + 20),
            fill = NA, color = "black") +
  geom_rect(data = newplotcenters,
            aes(xmin = x - 50, xmax = x + 50,
                ymin = y - 50, ymax = y + 50),
            fill = NA, color = "red") +
  scale_x_continuous(limits = c(0,1000)) +
  scale_y_continuous(limits = c(0,500)) +
  theme_bw() +
  guides(size = "none", color = "none")

saveRDS(merged.ds,
        "./outputs/tree.position.tls.RDS")

merged.ds %>%
  dplyr::select(sp.code,sp) %>%
  distinct()

selected.trees <- remaining.trees <- data.frame()

for (i in seq(1,nrow(newplotcenters))){
  cplot <- newplotcenters[i,]

  new.trees <- merged.ds %>%
    filter(gx >= (cplot[["x"]]-50),
           gx <= (cplot[["x"]]+50),
           gy >= (cplot[["y"]]-50),
           gy <= (cplot[["y"]]+50))

  print(nrow(new.trees))
  selected.trees <- bind_rows(selected.trees,
                              new.trees)

}

remaining.trees <- merged.ds %>%
  filter(!(Tag %in% c(selected.trees[["Tag"]])))

remaining.trees %>% filter(liana.cat.x == liana.cat.y)

sp.selected <- remaining.trees %>%
  filter(liana.cat.x == liana.cat.y) %>%
  group_by(sp,liana.cat.x) %>%
  summarise(N = n(),
            dbh.min = min(dbh.tls),
            dbh.max = max(dbh.tls),
            .groups = "keep") %>%
  arrange(desc(N))

sp.selected %>%
  group_by(sp) %>%
  summarise(Ncat = n(),
            Ntrees = sum(N)) %>%
  arrange(desc(Ntrees))


merged.ds[["blackplot"]] <- NA

for (i in seq(1,nrow(plotcenters))){
  cplot <- plotcenters[i,]

  ctrees <- merged.ds %>%
    filter(gx >= (cplot[["x"]]-20),
           gx <= (cplot[["x"]]+20),
           gy >= (cplot[["y"]]-20),
           gy <= (cplot[["y"]]+20))

  merged.ds[["blackplot"]][merged.ds[["Tag"]] %in% ctrees[["Tag"]]] <- plotcenters[i,"n"]

}
