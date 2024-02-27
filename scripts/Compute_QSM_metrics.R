rm(list = ls())

library(dplyr)
library(ITSMe)
library(ggplot2)
library(tidyr)
library(LianaRemovalRevisited)

ref.dir <- "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/pointclouds/"

raw.files <- list.files(path = ref.dir,pattern = "*.txt")
files <- unique(sub("_wood.txt","",sub("_leaf.txt", "", raw.files)))


# for (i in seq(1,length(files))){
#   print(i)
#   ctree <- files[i]
#   leaf.file <- paste0("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/pointclouds/",
#                       ctree,"_leaf.txt")
#   wood.file <- paste0("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/pointclouds/",
#                       ctree,"_wood.txt")
#   lsize <- file.info(leaf.file)$size
#   wsize <- file.info(wood.file)$size
#   if (lsize > 0 & wsize > 0){
#     pc <- bind_rows(read.csv(leaf.file,  sep = " ", header = FALSE),
#                     read.csv(wood.file,  sep = " ", header = FALSE))
#   } else if (wsize > 0){
#     pc <- read.csv(wood.file,  sep = " ", header = FALSE)
#   } else if (lsize > 0){
#     pc <- read.csv(leaf.file,  sep = " ", header = FALSE)
#   } else{
#     next()
#   }
#   fwrite(pc,
#          file = paste0("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/pointclouds/QSM2/pc/",
#                        ctree,".mat_pc.txt"),
#          sep = " ", row.names = FALSE, col.names = FALSE)
# }

A <- LianaRemovalRevisited::summary_qsm_metrics(QSMs_path = "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/pointclouds/QSM2/",
                                                PCs_path = "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/pointclouds/QSM2/pc/",
                                                OUT_path = "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/pointclouds/summary")

A <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/pointclouds/summary.csv")

A <- A %>%
  mutate(Tag = as.integer(sub(".*\\_","",sub(".mat","",tree_id))))
data <-  read.csv("./data/2019_TLS_data.csv",stringsAsFactors = FALSE) %>%
  dplyr::select(Tag,Species,Final.DBH..cm.,Height..m.,Liana,CPA..m2.,WSG,Total.vol..L.,Biomass..kg.) %>%
  rename(dbh = Final.DBH..cm.,
         h = Height..m.,
         sp = Species,
         CA = CPA..m2.,
         Vol = Total.vol..L.,
         Biomass = Biomass..kg.)

data.merged <-
  data %>%
  left_join(A,
            by = "Tag")

plot(data.merged$dbh,data.merged$dbh_m*100)
plot(data.merged$h,data.merged$tree_height_m)
plot(data.merged$Vol,data.merged$tree_vol_L,log = "xy")
abline(a = 0,b = 1,
       col = "red")


data.merged.long <- data.merged %>%
  mutate(branch_vol_L = tree_vol_L - trunk_vol_L) %>%
  # mutate(tree_vol_L = log10(tree_vol_L),
  #        trunk_vol_L = log10(trunk_vol_L),
  #        branch_len = log10(branch_len),
  #        CA = log10(CA),
  #        Vol = log10(Vol)) %>%
  pivot_longer(cols = -c(Tag,sp,Liana,tree_id,X_position, Y_position,dbh,dbh_m),
               names_to = "variable")

ggplot(data = data.merged.long) +
  geom_boxplot(aes(x = Liana, y = value, fill = as.factor(Liana))) +
  theme_bw() +
  facet_wrap(~ variable, scales = "free")


ggplot(data = data.merged.long %>%
         filter(variable %in%
                  c("branch_len","tree_biomass","trunk_biomass",
                    "tree_height_m","branch_biomass","trunk_h","h",
                    "crown_base_height","crown_length","crown_area_conv",
                    "crown_area_alpha","branch_area",
                    "crown_vol","Nbranches")),
       aes(x = dbh,
           y = value,
           color = as.factor(Liana))) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  stat_smooth(method = "lm") +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()

saveRDS(data.merged,"./outputs/QSM_metrics.RDS")

