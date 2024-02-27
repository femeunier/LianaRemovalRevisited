rm(list = ls())

# Height

library(dplyr)
library(rgl)
library(viewshed3d)
library(lidR)
library(sp)
library(alphashape3d)

raw.data <-  read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/2019_TLS_data.csv",stringsAsFactors = FALSE) %>%
  dplyr::select(Tag,Species,Liana,Final.DBH..cm.,CPA..m2.) %>%
  rename(sp = Species,
         liana.cat = Liana,
         DBH = Final.DBH..cm.,
         CA = CPA..m2.)

ggplot(data = raw.data) +
  geom_point(aes(x = DBH, y = CA, color = liana.cat)) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

Ntrees <- nrow(raw.data)

Dir <- "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/pointclouds/"
all.pcs <- list.files(Dir)


crs_tls<-sp::CRS("+init=epsg:26918")

chull.area.df <- data.frame()
for (itree in seq(1,Ntrees)){

  print(itree/Ntrees)

  cspecies <- raw.data$sp[itree] ; cliana <- raw.data$liana.cat[itree]
  cdbh <- raw.data$DBH[itree] ; corder <- raw.data$order[itree] ; carea <- raw.data$CA[itree] ;
  ctag <- raw.data$Tag[itree]

  files <- file.path(Dir,
                     all.pcs[grepl(paste0("_",ctag,"_"),all.pcs)])

  ctree <- data.frame()

  for (ifile in seq(1,length(files))){

    cfile <- files[ifile]
    cdata <- tryCatch(read.table(files[ifile]),
                      error = function(e) NULL)

    if (is.null(cdata)) next()

    ctree <- bind_rows(ctree,
                       as.data.frame(cdata %>%
                                       rename(X = V1, Y = V2, Z = V3) %>%
                                       mutate(organ = case_when(grepl("leaf",cfile) ~ "Leaf",
                                                                grepl("wood",cfile) ~ "Wood",
                                                                TRUE ~ "Other"))))
  }

  X <- matrix(c(ctree %>%
                  filter(organ == "Leaf" |
                           (organ == "Wood" & Z >= quantile(Z,0.5))) %>% pull(X),
                ctree %>%
                  filter(organ == "Leaf" |
                           (organ == "Wood" & Z >= quantile(Z,0.5))) %>% pull(Y)),
              ncol = 2)
  hpts <- chull(X)
  hpts <- c(hpts, hpts[1])
  conv.hull <- as.data.frame(X[hpts,])

  chull.poly <- Polygon(conv.hull, hole=F)
  chull.area <- chull.poly@area

  # Z <- matrix(c(ctree %>%
  #                 filter(organ == "Leaf" |
  #                          (organ == "Wood" & Z >= quantile(Z,0.5))) %>% pull(X),
  #               ctree %>%
  #                 filter(organ == "Leaf" |
  #                          (organ == "Wood" & Z >= quantile(Z,0.5))) %>% pull(Y),
  #             ctree %>%
  #               filter(organ == "Leaf" |
  #                        (organ == "Wood" & Z >= quantile(Z,0.5))) %>% pull(Z)),
  #             ncol = 3)
  #
  # Alpha <- ashape3d(Z, c(0))

  chull.area.df <- bind_rows(chull.area.df,
                             data.frame(id = itree, tag = ctag, liana.cat = cliana,
                                        dbh = cdbh, species = cspecies,
                                        area = carea,
                                        charea = chull.area))


}

ggplot(data = chull.area.df,
       aes(x = area, y = charea)) +
  geom_point() +
  stat_smooth(method = "lm",
              se = FALSE, color = "black") +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  theme_bw()


ggplot(data = chull.area.df,
       aes(x = dbh, y = charea, color = as.factor(liana.cat))) +
  geom_point() +
  stat_smooth(method = "lm",
              se = FALSE, color = "black") +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

saveRDS(chull.area.df,
        "./data/ch.data.correction.RDS")

temp <- readRDS(  "./data/ch.data.correction.RDS")
temp.long <- temp %>%
  pivot_longer(cols = c(area,charea),
               values_to = "area",
               names_to = "Type")

ggplot(data = temp.long,
       aes(x = dbh, y = area, color = as.factor(liana.cat),
           fill = as.factor(liana.cat))) +
  geom_point() +
  stat_smooth(method = "lm",
              se = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ Type) +
  theme_bw()
