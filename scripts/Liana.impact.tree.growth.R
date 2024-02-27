rm(list = ls())

library(xlsx)
library(ggplot2)
library(dplyr)
library(raster)
library(ggrepel)
library(caret)

plots <- c("ALF_01","ALF_02",
           "FLO_01","FLO_02",
           "FRP_01",
           "GAU_02","GAU_04","GAU_05","GAU_06","GAU_07",
           "PEA_01","PEA_02","PEA_03","PEA_04","PEA_05","PEA_06","PEA_07","PEA_08",
           "POA_01",
           "SAA_01","SAA_02",
           "SAT_01",
           "SIP_01",
           "TAN_02","TAN_03","TAN_04",
           "VCR_02")

last.census <- c(6,5,
                 6,5,
                 3,
                 3,3,1,3,2,
                 3,3,2,2,2,2,2,2,
                 3,
                 5,4,
                 4,
                 3,
                 6,5,5,
                 7)

Dir <- "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/ForestPlots/data/"

df.all <- data.frame()

for (iplot in seq(1,length(plots))){

  print(iplot/length(plots))
  clast.census <- last.census[iplot]

  for (icensus in seq(1,(clast.census))){

    file.name <- file.path(Dir,
                           paste0(plots[iplot],
                                  "_Census_",icensus,"_PlotDump.xlsx"))
    if (file.exists(file.name)){

      data <- tryCatch(xlsx::read.xlsx(file.name,
                                       sheetName = "Data"),
                       error = function(e){
                         return(NULL)
                       })
      if (is.null(data)){
        print(paste("Error reading this site:",plots[iplot]))
        next()
      }

      data.mut <- data %>%
        mutate(COI = as.numeric(LI),
               h = as.numeric(Height),
               DBH = D4/10) %>%
        mutate(liana.cat = case_when(COI == 0 ~ "no",
                                     COI < 3 ~ "low",
                                     COI < 5 ~ "high",
                                     TRUE ~ NA)) %>%
        filter(!is.na(DBH),!is.na(liana.cat),
               !is.na(h))

      df.all <- bind_rows(list(df.all,
                               data.mut %>%
                                 dplyr::select(TreeID, Tag.No,DBH,liana.cat,COI,h,Species,Census.Date) %>%
                                 mutate(site = plots[iplot],
                                        census = icensus,
                                        last.census = clast.census)))
    }
  }
}

df.all2 <- df.all %>%
  # filter(TreeID == 326726) %>%
  group_by(site,TreeID) %>%
  mutate(N = n()) %>%
  filter(N > 1) %>%
  mutate(timing = case_when(census == min(census) ~ "init",
                            census == max(census) ~ "end",
                            TRUE ~ "intermediary")) %>%
  ungroup() %>%
  arrange(site,TreeID)

sort(unique(df.all2$site))

df.all2 %>%
  group_by(site) %>%
  summarise(N = length(unique(TreeID)))

df.all.wide <-
  df.all2 %>%
  dplyr::select(-c(census,liana.cat)) %>%
  filter(timing != "intermediary") %>%
  pivot_wider(values_from = c(DBH,COI,Census.Date,h),
              names_from = timing) %>%
  mutate(delta_DBH = 100*(DBH_end - DBH_init)/DBH_init,
         delta_t = Census.Date_end- Census.Date_init,
         dDBH.dt = delta_DBH/delta_t) %>%
  mutate(cluster = sub("\\_.*","",site))


ggplot(data = df.all.wide %>%
         filter(DBH_init >= 10) %>%
         filter(COI_init == COI_end)) +
  geom_boxplot(aes(x = as.factor(COI_init),
                   y = dDBH.dt,
                   fill = as.factor(COI_init))) +
  facet_wrap(~ cluster, scales = "free") +
  theme_bw()

CM <- confusionMatrix(data=as.factor(df.all.wide$COI_init),
                      reference = as.factor(df.all.wide$COI_end))

round(100*CM$table/sum(CM$table),digits = 2)


