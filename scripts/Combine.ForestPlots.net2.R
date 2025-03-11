rm(list = ls())

library(xlsx)
library(ggplot2)
library(dplyr)
library(raster)
library(stringr)
library(ggrepel)


################################################################################
# Rainfor

Dir <- "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/rainfor/"

all.plot.files <- list.files(Dir,
                             full.names = TRUE)
# all.plot.files <- all.plot.files[grepl("VCR",all.plot.files)]

plots <- (str_sub(basename(all.plot.files),1,6))
last.census <- as.numeric(sapply(strsplit(basename(all.plot.files),split='_', fixed=TRUE),"[[",4))

df.all <- data.frame()

for (ifile in seq(1,length(all.plot.files))){

  clast.census <- last.census[ifile]
  file.name <- all.plot.files[ifile]

  print("============================")
  print(ifile/length(all.plot.files))
  print(file.name)
  if (file.exists(file.name)){

    data <- tryCatch(xlsx::read.xlsx(file.name,
                                     sheetName = "Data"),
                     error = function(e){
                       return(NULL)
                     })
    if (is.null(data)){
      print(paste("Error reading this site:",plots[ifile]))
      next()
    }

    data.mut <- data %>%
      # filter(!grepl("b|c",F1)) %>%
      filter(F2 == 1) %>%
      mutate(COI = as.numeric(LI),
             h = as.numeric(Height),
             F5 = as.numeric(F5),
             DBH = D4/10) %>%
      mutate(liana.cat = case_when(COI == 0 ~ "no",
                                   COI < 3 ~ "low",
                                   COI < 5 ~ "high",
                                   TRUE ~ NA_character_)) %>%
      filter(!is.na(DBH),!is.na(liana.cat),!is.na(h))

    if (nrow(data.mut) == 0){
      next()
    }

    df.all <- bind_rows(list(df.all,
                             data.mut %>%
                               dplyr::select(TreeID, Tag.No,DBH,liana.cat,COI,h,Species,F1,F5) %>%
                               mutate(site = plots[ifile],
                                      F5 = as.numeric(F5),
                                      # lat = lats[iplot],
                                      # lon = lons[iplot],
                                      census = clast.census)))
  }
}

hist(df.all$F5)

# saveRDS(df.all,
#         './data/rainfor2.loc.RDS')

# df.all2 <- df.all %>%
#   group_by(site,TreeID) %>%
#   mutate(N = n()) %>%
#   arrange(desc(census)) %>%
#   slice_head(n = 1) %>% # We only keep the most recent census
#   mutate(group = substr(site,1,3)) %>%
#   group_by(group) %>%
#   mutate(group.N = paste0(group,", N = ",length(DBH)))

df.all2 <- df.all %>%
  ungroup() %>%
  mutate(group = substr(site,1,3)) %>%
  group_by(group) %>%
  mutate(group.N = paste0(group,", N = ",length(DBH)))

df.all.filt <- df.all2 %>%
  filter(h > 0) %>%
  filter(F5 > 1) %>% # remove eye measurements ...
  filter(!grepl("b|c",F1))   # remove broken and bending stems


df.site.selected <- df.all.filt %>%
  group_by(group) %>%
  summarise(.groups = "keep",
            Ntot = n(),
            liana.cats = (length(unique(liana.cat))),
            Nhigh = length(which(liana.cat == "high")),
            Nlow = length(which(liana.cat == "low")),
            Nno = length(which(liana.cat == "no"))) %>%
  mutate(keep = (liana.cats == 3) &
           (Nhigh > 10) & (Nlow > 10) & (Nno > 10))

df2keep <- df.all.filt %>%
  filter(group %in% c(df.site.selected %>%
                        filter(keep) %>%
                        pull(group))) %>%
  filter(group != "TGS") %>% # Weird site where allometries are decreasing
  mutate(h = case_when(group == "VCR" & h > 30 & DBH <=30  ~ NA,
                       TRUE ~ h)) %>%
  filter(site != "TAM_09") # Very weird flat allometry, different than all other TAM plots


ggplot(data = df2keep %>%
         filter(DBH > 10) %>%
         filter(h > 3),
         aes(x = DBH, y = h,
             color = as.factor(liana.cat))) +
  geom_point(size = 0.1) +
  scale_x_log10() +
  scale_y_log10() +
  stat_smooth(se = TRUE, method = "lm") +
  facet_wrap(~ group) +
  theme_bw()


final.df <- df2keep
Ntrees <- nrow(final.df)
Nsites <- length(unique(final.df$group))

saveRDS(df2keep,
        "./outputs/All.rainfor.RDS")



# Rainfor.df <- readRDS("./data/ForestPlots/data/df.Rainfor.RDS") %>%
#   ungroup() %>%
#   mutate(site = group) %>%
#   rename(dbh = DBH,
#          sp = Species,
#          coi = COI) %>%
#   dplyr::select(c(dbh,h,coi,sp,liana.cat,site)) %>%
#   filter(dbh > 0, h > 0)
#
#
# ggplot(data = Rainfor.df %>%
#          filter(dbh >= 10),
#        aes(x = dbh, y = h,
#            color = as.factor(liana.cat))) +
#   geom_point(size = 0.1) +
#   scale_x_log10() +
#   scale_y_log10() +
#   stat_smooth(se = FALSE, method = "lm") +
#   facet_wrap(~ site, scales = "free") +
#   theme_bw()
