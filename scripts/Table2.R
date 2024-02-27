rm(list = ls())

library(brms)
library(gridExtra)
library(abind)
library(reshape2)
library(ggridges)
library(stringr)
library(scales)
library(tidyr)
library(ggforce)
library(dplyr)
library(LianaRemovalRevisited)
library(cowplot)
library(ggdist)
library(ggplot2)
library(reshape2)

# Load the data
all.df <- readRDS("./outputs/All.CA.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10)

all.df %>%
  group_by(liana.cat) %>%
  summarise(N = n())

all.df <- bind_rows(all.df,
                    all.df %>% mutate(site = "Total"))

alpha = 0.11
Model.predictions.CA <-
  readRDS("./outputs/Model.predictions.CA.50.RDS") %>%
  filter(liana.cat == "high") %>%
  group_by(site,liana.cat) %>%
  summarise(m = 100*median(diff_CA/no,na.rm = TRUE),
            low = 100*quantile(diff_CA/no,alpha/2,na.rm = TRUE),
            high = 100*quantile(diff_CA/no,1-alpha/2,na.rm = TRUE),
            .groups = "keep") %>%
  mutate(delta_CA = paste0(round(m,
                                 digits = 1)," \r\n (",
                           round(min(low,high),
                                 digits = 1),", ",
                           round(max(high,low),
                                 digits = 1),")")) %>%
  dplyr::select(-c(m,low,high)) %>%
  pivot_wider(names_from = liana.cat,
              values_from = delta_CA)




best.models <-(sapply(unique(all.df$site),function(site){
  csite <- gsub(" ", "",site, fixed = TRUE)
  sub(".CA","",sub(paste0("Fit.",
                          csite,"."),"",basename(rownames(
                            readRDS(paste0("./outputs/",csite,"/Diagnostics.CA.RDS")))[1])))
}))
best.model.df <- data.frame(site = names(best.models),
                            model = best.models) %>%
  mutate(model.family = sub("\\_.*","",model),
         fe = sub(".*\\_","",model)) %>%
  mutate(fe = case_when(fe == "none" ~ "",
                        fe == "ab" ~ "(a, b)",
                        fe == "ak" ~ "(a, k)",
                        fe == "bk" ~ "(b, k)",
                        fe == "all" ~ "(a, b, k)",
                        TRUE ~ paste0("(",fe,")"))) %>%
  mutate(best.model = case_when(fe == "" ~ model.family,
                                TRUE ~ paste0(model.family, " \r\n ",fe))) %>%
  dplyr::select(-c(model,model.family,fe))


Table2 <- all.df %>%
  group_by(site,liana.cat) %>%
  summarise(N = n(),
            dbh.min = min(dbh,na.rm = TRUE),
            dbh.max = max(dbh,na.rm = TRUE),
            CA.min = min(area,na.rm = TRUE),
            CA.max = min(area,na.rm = TRUE),
            .groups = "keep") %>%
  pivot_wider(names_from = liana.cat,
              values_from = c(N,dbh.min,dbh.max,CA.min,CA.max)) %>%
  mutate(dbh.min = min(c(dbh.min_no,dbh.min_low,dbh.min_high),na.rm = TRUE),
         dbh.max = max(c(dbh.max_no,dbh.max_low,dbh.max_high),na.rm = TRUE),
         CA.min = min(c(CA.min_no,CA.min_low,CA.min_high),na.rm = TRUE),
         CA.max = max(c(CA.max_no,CA.max_low,CA.max_high),na.rm = TRUE)) %>%
  dplyr::select(site,starts_with("N_"),
                dbh.min,dbh.max,CA.min,CA.max) %>%
  mutate(N = N_no + N_low + N_high) %>%
  mutate(dbh.range = paste0(round(dbh.min,,digits = 1)," - ",round(dbh.max,,digits = 1)),
         CA.range = paste0(round(CA.min,digits = 1)," - ",round(CA.max,digits = 1)),
         num = paste0(N," \r\n (",N_no," - ",N_low," - ",N_high,")")) %>%
  dplyr::select(site,num,dbh.range,CA.range) %>%

  left_join(best.model.df,
            by = "site") %>%
  left_join(Model.predictions.CA,
            by = "site")

sites <- unique(Table2$site)
sites.fac <- c(sites[order(sites[sites != "Total"])],"Total")


write.csv(Table2 %>%
            mutate(site = factor(as.character(site),
                                 levels = sites.fac)) %>%
            arrange(site),
          "./outputs/Table2.csv")


