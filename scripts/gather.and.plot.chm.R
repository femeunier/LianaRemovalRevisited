rm(list = ls())

library(lidR)
library(LidarProcessoR)
library(dplyr)
library(rgl)
library(LianaBCI)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(LVLRE.long)

# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/felicien/R/OP_chm/",
#                       "./outputs/"))

df.SP <- df.chm <- data.frame()

##############################################################################
# Load data

# plots <- c(13)
# years <- c(2016)

plots <- c(5,7,8,11,13,16)
years <- c(2016,2019)

for (iplot in seq(1,length(plots))){
  print(iplot)

  for (iyear in seq(1,length(years))){

    df_chm_file <- file.path(getwd(),"outputs",paste0("df_chm","plot",sprintf("%02d",plots[iplot]),"_",years[iyear],".RDS"))

    if (file.exists(df_chm_file)){
      df.chm.tmp <- readRDS(df_chm_file)
      df.chm <- bind_rows(list(df.chm,
                               df.chm.tmp))
    }

    df_SP_file <- file.path(getwd(),"outputs",paste0("df_SP","plot",sprintf("%02d",plots[iplot]),"_",years[iyear],".RDS"))

    if (file.exists(df_SP_file)){
      df.SP.tmp <- readRDS(df_SP_file)
      df.SP <- bind_rows(list(df.SP,
                              df.SP.tmp))
    }


  }
}


df.SP.true <- bind_rows(list(
  df.SP %>% filter(!(plot %in% c(11,13) & year == 2016)),
  data.frame(plot = 11,
             year = 2016,
             X.rel = rep(seq(10,50,10),5),
             Y.rel = sort(rep(seq(10,50,10),5))),
  data.frame(plot = 13,
             year = 2016,
             X.rel = rep(seq(10,50,10),5),
             Y.rel = sort(rep(seq(10,50,10),5)))
  ))

df.chm <- df.chm %>%
  mutate(X.rel = resolution/2 + (patch_X - 1)*resolution,
         Y.rel = resolution/2 + (patch_Y - 1)*resolution)

df.chm <- df.chm %>%
  group_by(patch,patch_X,patch_Y,plot,year,resolution,X.rel,Y.rel) %>%
  slice_head(n = 1)

saveRDS("./outputs/df.SP.RDS",object = df.SP.true)
saveRDS("./outputs/df.chm.RDS",object = df.chm)

ggplot(data = df.chm %>% filter(resolution == 1),
       aes(x = X.rel, y = Y.rel,
           fill = Z)) +
  geom_tile() +
  geom_point(data = df.SP.true, color = "red", size = 1) +
  scale_fill_distiller(palette = "Greens",direction = 1) +
  facet_grid(year ~ plot) +
  coord_fixed() +
  theme_bw()


df.chm.wide <- df.chm %>%
  pivot_wider(names_from = year,
              values_from = Z) %>%
  mutate(`2019` = case_when(is.null(`2019`) ~ NA_real_,
                            TRUE ~ `2019`),
         `2016` = case_when(is.null(`2016`) ~ NA_real_,
                            TRUE ~ `2016`)) %>%
  mutate(diff = `2019` - `2016`)

ggplot(data = df.chm.wide %>% filter(resolution == 1)) +
  geom_tile(aes(x = X.rel, y = Y.rel,
                fill = diff)) +
  geom_point(data = df.SP.true,
             aes(x = X.rel, y = Y.rel,
                 color = as.factor(year)), size = 1) +
  scale_fill_distiller(palette = "RdYlGn",direction = 1) +
  scale_colour_discrete(c("blue","red")) +
  facet_grid(~ plot) +
  coord_fixed() +
  theme_bw()


ggplot(data = df.chm.wide) +
  geom_density(aes(x = diff, fill = as.factor(resolution)), alpha = 0.4) +
  facet_grid(~ plot) +
  theme_bw()


trees <- read.csv("/home/femeunier/Documents/projects/LVLRE.long/data/TREE data for felicien 220119.csv")
data.tree <- trees %>% mutate(ID = 1:nrow(trees)) %>% select(-c("Date_2008","Date_2010",
                                                                "DBH2008","DBH2010",
                                                                "BA2008","BA2010",
                                                                "H2008","H2010",
                                                                "AGB2008","AGB2010",
                                                                "AGBC2008","AGBC2010"))


data.tree.formatted <- data.tree %>% group_by(ID) %>% select(c("Treatment","Parcela","Cuadrante","Subcuadrante","WD","CC","ID",starts_with("Date"))) %>%
  pivot_longer(cols = -c("ID","Treatment","WD","CC","Parcela","Cuadrante","Subcuadrante"),
               names_to = "date",
               values_to = "Time") %>% mutate(timing = as.numeric(gsub(".*_", "", date))) %>% left_join(data.tree %>% group_by(ID) %>% select(c("ID",starts_with("DBH"))) %>%
                                                                                                          pivot_longer(cols = -c("ID"),
                                                                                                                       names_to = "date",
                                                                                                                       values_to = "DBH") %>% mutate(timing = as.numeric(gsub(".*DBH", "", date))),by = c("ID","timing")) %>%
  left_join(data.tree %>% group_by(ID) %>% select(c(-starts_with("AGBC"))) %>% select(c("ID",starts_with("AGB"))) %>%
              pivot_longer(cols = -c("ID"),
                           names_to = "date",
                           values_to = "AGB") %>% mutate(timing = as.numeric(gsub(".*AGB", "", date))),by = c("ID","timing")) %>%
  left_join(data.tree %>% group_by(ID) %>% select(c("ID",starts_with("H"))) %>%
              pivot_longer(cols = -c("ID"),
                           names_to = "date",
                           values_to = "H") %>% mutate(timing = as.numeric(gsub(".*H", "", date))),by = c("ID","timing")) %>%
  left_join(data.tree %>% group_by(ID) %>% select(c("ID",starts_with("BA"))) %>%
              pivot_longer(cols = -c("ID"),
                           names_to = "date",
                           values_to = "BA") %>% mutate(timing = as.numeric(gsub(".*BA", "", date))),by = c("ID","timing")) %>%
  left_join(data.tree %>% group_by(ID) %>% select(c("ID",starts_with("AGBC"))) %>%
              pivot_longer(cols = -c("ID"),
                           names_to = "date",
                           values_to = "AGBC") %>% mutate(timing = as.numeric(gsub(".*AGBC", "", date))),by = c("ID","timing")) %>%
  select(-c(date.x,date.y,date.x.x,date.y.y,date.x.x.x,date.y.y.y)) %>% mutate(Time = case_when(Time < 2002 ~ Time + 10,
                                                                                                TRUE ~ Time))

Tree.data <- data.tree.formatted %>% group_by(timing) %>% mutate(Time2 = mean(Time,na.rm = TRUE),
                                                                 GF = "Tree") %>% rename(plot = Parcela,
                                                                                         subplot = Cuadrante,
                                                                                         subsubplot = Subcuadrante)


Tree.data %>% filter(plot == 5,subplot == 20,timing == 10) %>% pull(DBH) %>% hist()
df.dist.DBH <- bind_rows(list(
  data.frame(dbh = Tree.data %>% filter(plot == 11,timing == 10) %>% pull(DBH),
             plot = 11),
  data.frame(dbh = Tree.data %>% filter(plot == 13,timing == 10) %>% pull(DBH),
             plot = 13)))

ggplot(data = df.dist.DBH) +
  geom_density(aes(x = dbh,fill = as.factor(plot)),alpha = 0.3) +
  facet_wrap(~ plot,scales = "free") +
  theme_bw()

Tree.data %>% filter(plot == 11,timing == 10) %>% pull(DBH) %>% summary()
Tree.data %>% filter(plot == 11,timing == 10) %>% filter(!is.na(DBH)) %>% nrow()
Tree.data %>% filter(plot == 13,timing == 10) %>% pull(DBH) %>% summary()
Tree.data %>% filter(plot == 13,timing == 10) %>% filter(!is.na(DBH)) %>% nrow()

Tree.data %>% filter(timing == 10,
                     !is.na(DBH)) %>% filter(plot %in% c(11,13)) %>%
  group_by(plot) %>%
  filter(DBH == max(DBH))
