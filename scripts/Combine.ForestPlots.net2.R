rm(list = ls())

library(xlsx)
library(ggplot2)
library(dplyr)
library(raster)
library(stringr)
library(ggrepel)


################################################################################
# Rainfor

all.plot.files <- list.files("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/rainfor/")
plots <- unique(str_sub(all.plot.files,1,6))

last.census <- c()
for (iplot in seq(1,length(plots))){

  cplot <- plots[iplot]
  cfiles <- all.plot.files[grepl(cplot,all.plot.files)]
  last.census[iplot] <- max(as.numeric(sapply(strsplit(cfiles,split='_', fixed=TRUE),"[[",4)))
}

Dir <- "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/rainfor/"


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
        filter(F2 == 1) %>%
        mutate(COI = as.numeric(LI),
               h = as.numeric(Height),
               DBH = D4/10) %>%
        mutate(liana.cat = case_when(COI == 0 ~ "no",
                                     COI < 3 ~ "low",
                                     COI < 5 ~ "high",
                                     TRUE ~ NA_character_)) %>%
        filter(!is.na(DBH),!is.na(liana.cat),
               !is.na(h))

      if (nrow(data.mut) == 0){
        next()
      }

      df.all <- bind_rows(list(df.all,
                               data.mut %>%
                                 dplyr::select(TreeID, Tag.No,DBH,liana.cat,COI,h,Species) %>%
                                 mutate(site = plots[iplot],
                                        # lat = lats[iplot],
                                        # lon = lons[iplot],
                                        census = icensus,
                                        last.census = clast.census)))
    }
  }
}

saveRDS(df.all,
        './data/rainfor2.loc.RDS')

df.all2 <- df.all %>%
  group_by(site,TreeID) %>%
  mutate(N = n()) %>%
  arrange(desc(census)) %>%
  slice_head(n = 1) %>% # We only keep the most recent census
  mutate(group = substr(site,1,3)) %>%
  group_by(group) %>%
  mutate(group.N = paste0(group,", N = ",length(DBH)))

print(c(nrow(df.all),
        nrow(df.all2),
        nrow(df.all %>%
               filter(census == last.census))))

df.all.filt <- df.all2 %>%
  filter(h > 0) %>%
  filter(DBH >= 10)

df.site.selected <- df.all.filt %>%
  group_by(group) %>%
  summarise(.groups = "keep",
            Ntot = n(),
            liana.cats = (length(unique(liana.cat))),
            Nhigh = length(which(liana.cat == "high")),
            Nlow = length(which(liana.cat == "low")),
            Nno = length(which(liana.cat == "no"))) %>%
  mutate(keep = (liana.cats == 3) & (Nhigh > 10) & (Nlow > 10)
         & (Nno > 10))

ggplot(data = df.all.filt %>%
           filter(group %in% c(df.site.selected %>%
                                 filter(keep) %>%
                                 pull(group))) ,
         aes(x = DBH, y = h,
             color = as.factor(liana.cat))) +
  geom_point(size = 0.1) +
  scale_x_log10() +
  scale_y_log10() +
  stat_smooth(se = FALSE, method = "lm") +
  facet_wrap(~ group, scales = "free") +
  theme_bw()

final.df <- df.all.filt %>%
  filter(group %in% c(df.site.selected %>%
                        filter(keep) %>%
                        pull(group)))
Ntrees <- nrow(final.df)
Nsites <- length(unique(final.df$group))

saveRDS(df.all.filt,
        "./outputs/All.rainfor.RDS")

