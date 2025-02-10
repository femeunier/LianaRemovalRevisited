rm(list = ls())

library(xlsx)
library(ggplot2)
library(dplyr)
library(raster)
library(ggrepel)
library(stringr)

################################################################################
# Asiafor

Dir <- "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/Asia/data/"
files <- list.files(Dir,full.names = TRUE)
Splitted <- str_split(tools::file_path_sans_ext(basename(files)),"\\_")
plots <- paste0(sapply(Splitted,"[",1),"_",
                sapply(Splitted,"[",2))
census <- sapply(Splitted,"[",4)

df.all <- data.frame()

for (iplot in seq(1,length(plots))){

  print(iplot/length(plots))
  c.census <- census[iplot]

  file.name <- file.path(Dir,
                         paste0(plots[iplot],
                                "_Census_",c.census,"_PlotDump.xlsx"))

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
                               mutate(F5 = as.numeric(F5)) %>%
                               dplyr::select(TreeID, Census.Date, Tag.No,DBH,liana.cat,COI,h,Species,F5) %>%
                               mutate(site = plots[iplot],
                                      census = c.census)))


  }
}

hist(df.all$F5)

df.all.site <- df.all %>%
  mutate(site.common = str_sub(site,1,3)) %>%
  filter(!is.na(DBH),!is.na(h),!is.na(liana.cat)) %>%
   filter(!(DBH > 50 & h < 10))  # Fishy tree (height < POM..)

AAA <- df.all.site %>%
  dplyr::select(DBH,liana.cat,COI,h,Species,site)

saveRDS(AAA,
        "./data/Asia/raw.data.all.RDS")

sites2keep <- df.all.site %>%
  group_by(site.common) %>%
  summarise(Ncat = length(unique(liana.cat)),
            Nno = length(which(liana.cat == "no")),
            Nlow = length(which(liana.cat == "low")),
            Nhigh = length(which(liana.cat == "high")),
            .groups = "keep") %>%
  mutate(Ntotal = (Nno + Nlow + Nhigh)) %>%
  filter(Ncat == 3,
         Nno > 10, Nhigh > 10, Nlow > 10)

df2keep <- df.all.site %>%
  filter(site.common %in% (sites2keep %>% pull(site.common)))

ggplot(data = df2keep,
       aes(x = DBH, y = h,
           color = liana.cat, fill = liana.cat)) +
  geom_point() +
  stat_smooth(method = "lm") +
  facet_wrap(~ site.common, nrow = 1) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw() +
  theme(legend.position = c(0.1,0.9))


# df.all.site %>%
#   filter(DBH < 15,h > 25)

plots2save <- df.all.site %>%
  filter(site.common %in% (sites2keep %>% pull(site.common))) %>%
  mutate(site = sub(pattern = "_","-",site)) %>%
  pull(site) %>% unique()

file <- "~/Documents/projects/LianaRemovalRevisited/data/Asia/PITeamEmailTracker_Felicien_AsiaAustLianas_22Oct24.xlsx"
data.plot <- read.xlsx(file,"AsiaAustPlots_with LI_Height") %>%
  filter(PlotCode %in% plots2save) %>%
  dplyr::select(PlotCode,Latitude,Longitude,
                ForestElevationName,ForestEdaphicName,ForestCompositionName,ForestStatusName) %>%
  mutate(site.common = str_sub(PlotCode,1,3))

saveRDS(data.plot,
        "./data/Asia/Plot.locations.RDS")

saveRDS(df2keep,
        "./data/Asia/raw.data.RDS")
