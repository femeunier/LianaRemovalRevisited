rm(list = ls())

library(dplyr)
library(ggplot2)
library(BCI.AGB)
library(LianaBCI)

raw.data <-
  read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/new_height_liana_data_2015_no_outliers_q20.csv",stringsAsFactors = FALSE) %>%
    dplyr::select(Tag,Species,DBH_cm,Height,Lianas) %>%
    mutate(liana.cat = case_when(Lianas == 0 ~ "no",
                                 Lianas < 3 ~ "low",
                                 TRUE ~ "high")) %>%
    rename(tag = Tag)

tags <- raw.data %>% pull(tag)

trees.selected <- BCI.AGB::tree.BCI %>%
  filter(census.time == 2015) %>%
  mutate(tag = as.numeric(tag)) %>%
  filter(tag %in% tags)

data.merged <-
  raw.data %>%
  left_join(trees.selected %>%
              dplyr::select(tag,sp,patch,gx,gy,dbh),
            by = "tag") %>%
  mutate(species.agreement = (Species == sp),
         diff_DBH = dbh/10 - DBH_cm)

# plot(data.merged$dbh/10,data.merged$DBH_cm)

table(data.merged$species.agreement)
data.merged %>% filter(!species.agreement)


ggplot(data = data.merged,
       aes(x = gx, y = gy, color = as.factor(Lianas))) +
  geom_point() +
  theme_bw()

################################################################################
# Lianas

data.file <- "/home/femeunier/Documents/projects/LianaBCI/data/2021-09-22_liana_bci_data_V9_0_felicien.csv"
data <- read.csv(data.file)

threshold <- 3*10 # 1cm = 10mm
data.filtered <- data %>% filter(DBH >= threshold)

Delta_dist <-  5
equivalent_L <- Delta_dist # pi/4*D² ≃ D*D

data.patch <- data.filtered %>% filter(PX >= 0 & PX < 1000 &
                                         PY >= 0 & PY < 500,
                                       Status %in% c("L","N")) %>%
  mutate(patch = LianaBCI::patchnumber_from_position(PX,PY,
                                                     patch_X = equivalent_L,patch_Y = equivalent_L,
                                                     extr_x = c(0,1000),
                                                     extr_y = c(0,500))[["patch"]]) %>%
  filter(Census == 2)

data.merged[["BAliana"]] <- 0

compt = 0
for (itree in seq(1,nrow(data.merged))){

  print(itree/nrow(data.merged))

  cgx <- data.merged[itree,"gx"] ; cgy <- data.merged[itree,"gy"]

  if (cgx < Delta_dist | cgx > (1000 - Delta_dist) |
      cgy < Delta_dist | cgy > (500 - Delta_dist) ){
    compt = compt + 1
    next()
  }

  BA.liana <- data.patch %>%
    mutate(dist = sqrt((PX - cgx)**2 + (PY - cgy)**2)) %>%
    filter(dist <= Delta_dist)

  cDBH <- (BA.liana %>% pull(DBH))/10
  cBA <- sum(pi/4*cDBH*cDBH)/(pi*(Delta_dist**2)/4) # cm²/m²
  data.merged[["BAliana"]][itree] <- cBA

}


summary(lm(data = data.merged,
           formula = BAliana ~ as.factor(Lianas)))

data.merged.sum <- data.merged %>% group_by(Lianas) %>%
  summarise(BAliana.m = mean(BAliana))

data.patch.BA <- data.patch %>% group_by(patch) %>%
  summarise(BAliana = sum(pi/4*DBH*DBH/100)/(equivalent_L**2)) %>%
  mutate(liana.cat = case_when(BAliana <= data.merged.sum$BAliana.m[1] ~ 0,
                               BAliana <= data.merged.sum$BAliana.m[3] ~ 1,
                               TRUE ~ 2))

table(data.patch.BA$liana.cat)

p1 <- ggplot(data = data.patch.BA) +
  geom_density(aes(x = 1 + BAliana)) +
  scale_x_log10(limits = c(1,100)) +
  # scale_y_continuous(limits = c(0,150)) +
  geom_vline(data = data.merged.sum,
             aes(xintercept = 1 + BAliana.m, color = as.factor(Lianas))) +
  theme_bw()

ggplot(data = data.merged) +
  geom_boxplot(aes(x = as.factor(Lianas), y = 1 + (BAliana))) +
  # scale_y_log10() +
  scale_y_log10(limits = c(1,100)) +
  coord_flip() +
  theme_bw()

ggplot(data = data.patch.BA) +
  geom_boxplot(aes(y = BAliana)) +
  # scale_x_log10() +
  # scale_y_continuous(limits = c(0,25)) +
  geom_hline(data = data.merged.sum,
             aes(yintercept = BAliana.m, color = as.factor(Lianas))) +
  theme_bw()

