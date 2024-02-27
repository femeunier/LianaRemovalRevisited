rm(list = ls())

library(xlsx)
library(ggplot2)
library(dplyr)
library(raster)
library(ggrepel)

################################################################################
# Rainfor

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
        filter(!is.na(DBH),!is.na(liana.cat))

      df.all <- bind_rows(list(df.all,
                               data.mut %>%
                                 dplyr::select(TreeID, Census.Date, Tag.No,DBH,liana.cat,COI,h,Species) %>%
                                 mutate(site = plots[iplot],
                                        census = icensus,
                                        last.census = clast.census)))
    }
  }
}

################################################################################
# Afritron

data <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/Afritron/africa_individualsdata_mainplotview_112017.csv",stringsAsFactors = FALSE)

data.filt <- data %>%
  mutate(coi = as.numeric(substr(LI,1,1)) ) %>%
  filter(!is.na(D4),!is.na(coi)) %>%
  mutate(liana.cat = case_when(coi == 0 ~ "no",
                               coi < 3 ~ "low",
                               TRUE ~ "high"))

df.sum <-
  bind_rows(list(df.all %>%
                   dplyr::select(DBH,liana.cat,site,census,Census.Date),
                 data.filt %>%
                   dplyr::select(D4,liana.cat,Plot.Code,Census.No,Census.Date) %>%
                   rename(site = Plot.Code,
                          DBH = D4,
                          census = Census.No)))%>%
  group_by(site,census,Census.Date) %>%
  summarise(N = length(DBH),
            Nno = length(which(liana.cat == "no")),
            Nlow = length(which(liana.cat == "low")),
            Nhigh = length(which(liana.cat == "high")),
            .groups = "keep")

df.ratio <- df.sum %>%
  filter(N > 100) %>%
  mutate(ratio_no = Nno/N,
         ratio_low = Nlow/N,
         ratio_high = Nhigh/N) %>%
  dplyr::select(site,census, starts_with("ratio")) %>%
  pivot_longer(cols = starts_with("ratio"),
               names_to = "liana.cat",
               values_to = "ratio") %>%
  mutate(liana.cat = factor(sub(".*\\_", "", liana.cat),
                            levels = c("no","low","high")))

ggplot(data = df.ratio) +
  geom_boxplot(aes(y = ratio, fill = liana.cat, x= liana.cat)) +
  theme_bw()

df.ratio %>%
  group_by(liana.cat) %>%
  summarise(med = median(ratio),
            mean = mean(ratio),
            min = min(ratio),
            max = max(ratio))

df.multiple <- df.sum %>%
  group_by(site) %>%
  filter(N > 100) %>%
  mutate(Ncensus = length(census)) %>%
  filter(Ncensus > 1) %>%
  mutate(census.rel = 1:Ncensus[1])

ggplot(data = df.multiple,
       aes(x = Census.Date, y = Nhigh/N)) +
  geom_line(aes(group = site)) +
  stat_smooth(method = "lm") +
  theme_bw()
