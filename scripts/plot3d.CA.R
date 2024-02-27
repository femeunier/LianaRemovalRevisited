rm(list = ls())

# CA

library(dplyr)
library(rgl)
library(viewshed3d)
library(plot3D)
library(ggplot2)
library(lidR)

raw.data <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/2019_TLS_data.csv",stringsAsFactors = FALSE) %>%
  rename(dbh = Final.DBH..cm.,
         CA = CPA..m2.) %>%
  mutate(liana.cat = case_when(Liana == 0 ~ "no",
                               Liana == 1 ~ "low",
                               Liana == 2 ~ "high"))

ggplot(data = raw.data) +
  geom_point(aes(x = dbh, y = CA, color = liana.cat)) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

colors <- c("no" = "darkgreen",
            "low" = "orange",
            "high"= "darkred")

raw.data.cat <- raw.data %>%
  mutate(dbh.cat = round(dbh/10,0)) %>%
  group_by(liana.cat,dbh.cat) %>%
  mutate(CA.m = mean(CA)) %>%
  mutate(diff = round(abs(CA.m - CA),digits = 2))  %>%
  ungroup()

raw.data.selected <- raw.data.cat %>%
  group_by(dbh.cat,liana.cat) %>%
  arrange(diff,CA) %>%
  slice_head(n = 1) %>%
  group_by(dbh.cat) %>%
  arrange(CA) %>%
  mutate(order = 1:length(dbh)) %>%
  ungroup()

raw.data.selected <- raw.data.selected
# %>%  slice_sample(n = 2)

dbh.sum <- summary(raw.data.selected$dbh)

Ntrees <- nrow(raw.data.selected)
thr.hold = 1
BoxSize = 0.25
Select = 0.5

Dir <- "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/pointclouds/"
all.pcs <- list.files(Dir)

options(rgl.printRglwidget = TRUE)

all.trees <- data.frame()
crs_tls<-sp::CRS("+init=epsg:26918")

ch.all <- data.frame()

for (itree in seq(1,Ntrees)){

  print(itree/Ntrees)

  cspecies <- raw.data.selected$Species[itree] ; cliana <- raw.data.selected$Liana[itree]
  cdbh <- raw.data.selected$dbh[itree] ; cCA <- raw.data.selected$CA[itree] ; corder <- raw.data.selected$order[itree]

  files <- file.path(Dir,
                     all.pcs[grepl(paste0("_",raw.data.selected$Tag[itree],"_"),all.pcs)])

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

  # We must first calculate the convex hull
  X <- matrix(c(ctree %>%
                  filter(organ == "Leaf" |
                           (organ == "Wood" & Z >= quantile(Z,0.75))) %>% pull(X),
                ctree %>%
                  filter(organ == "Leaf" |
                           (organ == "Wood" & Z >= quantile(Z,0.75))) %>% pull(Y)),
              ncol = 2)
  hpts <- chull(X)
  hpts <- c(hpts, hpts[1])
  conv.hull <- as.data.frame(X[hpts,])

  # plot(X, cex = 0.5)
  # lines(X[hpts, ], color = "red")

  las <-LAS(ctree, proj4string = crs_tls)
  las.dw <- downsample_scene(downsample_scene(las, method = "space", BoxSize),
                             method = "random",Select)

  ctree.dw <- as.data.frame(payload(las.dw))

  ctree.rescaled <- ctree.dw %>%
    mutate(base = Z <= (min(Z) + thr.hold)) %>%
    mutate(delta_x =  mean(X[organ == "Leaf"]),
           delta_y =  mean(Y[organ == "Leaf"])) %>%
    mutate(X = X - delta_x,
           Y = Y - delta_y,
           Z = Z - min(Z[base]))

  # with(ctree %>% filter(organ == "Leaf"),plot3d(X, Y, Z, type = "p", size = 1,xlab = "",ylab = "",zlab="",
  #                      aspect = TRUE))

  ctree.position <- ctree.rescaled %>%
    dplyr::mutate(species = cspecies,
             CA = cCA,
             dbh = cdbh,
             liana.cat = cliana,
             id = itree) %>%
    mutate(Delta_X = dbh - max(0,(dbh-150))/1.5,
    # mutate(Delta_X = dbh,
           # Delta_Y = (corder-1)*0 + runif(n = 1,-1,1)*5) %>%
           # Delta_Y = 0) %>%
           Delta_Y = (-as.numeric(cliana)+1)*20 + runif(n = 1,-1,1)*5) %>%
    mutate(X = X + Delta_X,
           Y = Y + Delta_Y)

  all.trees <- bind_rows(all.trees,
                         ctree.position %>%
                           ungroup())




  ch.all <- bind_rows(ch.all,
                      bind_rows(conv.hull %>%
                                  rename(X = V1, Y = V2) %>%
                                  mutate(X = X + unique(ctree.position$Delta_X) - unique(ctree.position$delta_x),
                                         Y = Y + unique(ctree.position$Delta_Y) - unique(ctree.position$delta_y)) %>%
                        mutate(id = itree,
                               liana.cat = cliana,
                               Z = mean(ctree.position %>% filter(organ == "Leaf" |
                                                                    (organ == "Wood" & Z >= quantile(Z,0.75))) %>% pull(Z))),
                        data.frame(V1 = NA, V2 = NA)))

}

all.trees.colored <- all.trees %>%
  mutate(liana.cat.num = as.numeric(factor(liana.cat,
                                           levels = c(0,1,2)))) %>%
  mutate(color = case_when(organ == "Leaf" ~ colors[liana.cat.num],
                           organ == "Wood" ~ colors[liana.cat.num],
                           TRUE ~ "black"))

ch.all <- ch.all %>%
  mutate(liana.cat.num = as.numeric(factor(liana.cat,
                                           levels = c(0,1,2)))) %>%
  mutate(color = colors[liana.cat.num])


bg3d(color = "white")
view3d(theta = 0, phi = 0)
options(rgl.printRglwidget = TRUE)


with(all.trees.colored %>%
       filter(organ == "Leaf" |
                    (organ == "Wood" & Z >= 15)) , plot3d(X,Y, Z, col = color, alpha = 0.5,
                              type = "p", size = 1,xlab = "",ylab = "",zlab="",
                 aspect = TRUE,main = "", sub = "", ann = FALSE, axes = FALSE))

with(ch.all,
     plot3d(X,Y, Z, add = TRUE,
            type = "l",lw = 4,xlab = "",ylab = "",zlab="",
            col = color, alpha = 0.5,main = "", sub = "", ann = FALSE, axes = FALSE))

saveRDS(raw.data.selected,
        "./outputs/plot3d.CA.RDS")

