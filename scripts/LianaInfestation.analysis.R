rm(list = ls())

library(dplyr)
library(ggplot2)
library(LianaRemovalRevisited)



unique(Removal.data$subplot)

Liana.ssp <- Liana.data %>% group_by(Time2,plot,subplot,subsubplot) %>% summarise(BA = sum(BA/100,na.rm = TRUE)/25, # cm²/m²
                                                                                  .groups = "keep")
boxplot(Liana.ssp$BA)

Liana.sp <- Liana.data %>% group_by(Time2,plot,subplot) %>% summarise(BA = sum(BA/100,na.rm = TRUE)/400, # cm²/m²
                                                                      .groups = "keep")
hist(Liana.sp$BA)


Liana.p <- Liana.data %>% group_by(Time2,plot) %>% summarise(BA = sum(BA/100,na.rm = TRUE)/3600, # cm²/m²
                                                              .groups = "keep")
boxplot(Liana.p$BA)
