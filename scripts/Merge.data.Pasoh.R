rm(list = ls())

library(ggplot2)
library(dplyr)

height.data <- read.table("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/Pasoh/Pasoh.crown.leaf.by.ind.txt",
                          header = TRUE)
ggplot(data = height.data) +
  geom_point(aes(x = dbh, y = height))

hist(height.data$dbh)


# ########################################################################################################################################
#
# h.data <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/Pasoh/PasohCrown600FieldDataForm(completed)-1.csv") %>%
#   mutate(Angle1 = as.numeric(Angle1),
#          Dist1 = as.numeric(Dist1),
#          Dist2 = as.numeric(Dist2),
#          Angle2 = as.numeric(Angle2))
#
# h.data[["height"]] <- 1.6+pmax(h.data$Dist1*sin(h.data$Angle1*pi/360),h.data$Dist2*sin(h.data$Angle2*pi/360),na.rm=T)
# h.data[["height"]][h.data[["Method"]] == "P"] <-
#   h.data$Dist1[h.data[["Method"]] == "P"]
#
#
# merged.h <- height.data %>%
#   left_join(h.data %>%
#               rename(h.calc = height,
#                      tag = TAG),
#             by = "tag")
#
# summary(height.data$height)
#
# plot(merged.h$height,merged.h$h.calc)


COI.data <- read.table("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/Pasoh/Wright_etal_2015_Ecology_Dryad_Data.txt",
                       header = TRUE) %>%
  mutate(liana.cat.2002 = case_when(L2002 == 0 ~ "no",
                                    L2002 <= 2 ~ "low",
                                    L2002 <= 4 ~ "high"),
         liana.cat.2014 = case_when(L2014 == 0 ~ "no",
                                    L2014 <= 2 ~ "low",
                                    L2014 <= 4 ~ "high")) %>%
  mutate(change.cat = (liana.cat.2014 != liana.cat.2002))



COI.data.filtered <- COI.data %>%
  filter(!change.cat)

table(height.data$tag %in% c(COI.data$TAG))

merged.data <- height.data %>%
  dplyr::select(sp,tag,dbh,height) %>%
  left_join(COI.data %>%
              rename(tag = TAG),
            by = "tag") %>%
  filter(!is.na(dbh),!is.na(height)) %>%
  rowwise() %>%
  mutate(Lmean = mean(c(L2002,L2014),na.rm = TRUE))
         # Lmean = sqrt((1+L2002)*(1+L2014)) - 1)

# plot(merged.data$Lmean,merged.data$Lmean.geom)

merged.data.filtered <- merged.data %>%
  filter(dbh >= 10) %>%
  filter(!is.na(L2002) | !is.na(L2014)) %>%
  mutate(liana.cat.consensus = case_when(liana.cat.2002 == liana.cat.2014 ~ liana.cat.2002,
                                         (is.na(liana.cat.2002) & !is.na(liana.cat.2014)) ~ liana.cat.2014,
                                         (is.na(liana.cat.2014) & !is.na(liana.cat.2002)) ~ liana.cat.2002,
                                         Lmean == 0 ~ "no",
                                         Lmean <= 2 ~ "low",
                                         Lmean <= 4 ~ "high",
                                         TRUE ~ NA_character_),
         coi  = case_when(liana.cat.2002 == liana.cat.2014 ~ L2002,
                          (is.na(liana.cat.2002) & !is.na(liana.cat.2014)) ~ L2014,
                          (is.na(liana.cat.2014) & !is.na(liana.cat.2002)) ~ L2002,
                          Lmean == 0 ~ Lmean,
                          Lmean <= 2 ~ Lmean,
                          Lmean <= 4 ~ Lmean,
                          TRUE ~ NA_real_),
         cat  = case_when(liana.cat.2002 == liana.cat.2014 ~ 1,
                          (is.na(liana.cat.2002) & !is.na(liana.cat.2014)) ~ 2,
                          (is.na(liana.cat.2014) & !is.na(liana.cat.2002)) ~ 3,
                          Lmean == 0 ~ 4,
                          Lmean <= 2 ~ 5,
                          Lmean <= 4 ~ 6,
                          TRUE ~ 7))
table(merged.data.filtered %>%
        pull(cat))

table(merged.data.filtered %>%
        pull(liana.cat.consensus))


(merged.data.filtered %>%
    filter(cat > 3) %>%
    dplyr::select(L2002,L2014,Lmean) %>%
    group_by(L2002,L2014,Lmean) %>%
    summarise(N = n()) %>% arrange(desc(N)))

ggplot(data = merged.data.filtered %>%
         filter(dbh >= 10,
                !is.na(liana.cat.consensus)),
       aes(x = dbh, y = height, color = as.factor(liana.cat.consensus))) +
  geom_point() +
  stat_smooth(method = "lm") +
  # scale_x_log10() +
  # scale_y_log10() +
  theme_bw()

saveRDS(merged.data.filtered %>%
          dplyr::select(sp,dbh,height,coi,liana.cat.consensus),
        file = "./data/Pasoh/data.merged.RDS")

nrow(merged.data.filtered %>% filter(!is.na(liana.cat.consensus)))
