rm(list = ls())

library(ggplot2)
library(dplyr)
library(dplyr)
library(data.table)
library(BCI.AGB)
library(LidarProcessoR)
library(rgl)
library(RColorBrewer)
library(pracma)

directory <- "/media/femeunier/Seagate Backup Plus Drive2/Data_Sruthi/"
directory <- "/media/femeunier/Seagate/Felicien/"

file.names <- c("plot5_pt.txt","plot7_pt.txt","plot8_pt.txt","plot16_pt.txt",
                "plot12_pt.txt","plot13_pt.txt","plot14_pt.txt","plot11_pt.txt")

Treatments <- c("R","R","C","C",
                "C","C","R","R")

plot.num <- c(5,7,8,16,12,13,14,11)

data.files <- file.path(directory,file.names)

df.all <- list()

Delta_X = 40 ; Delta_Y = 40
for (ifile in seq(1,length(data.files))){

  cdata <- fread(data.files[ifile])

  cdata.filter <- cdata %>% filter(V1 >= -Delta_X/2, V1 < Delta_X/2,
                                   V2 >= -Delta_X/2, V2 < Delta_X/2,
                                   V3 >= 0)

  cdata.filter.renamed <- cdata.filter %>% rename(X = V1, Y = V2, Z = V3) %>%
    mutate(plot = plot.num[ifile],
           Treatment = Treatments[ifile])

  df.all[[ifile]] <- cdata.filter.renamed

}

df.hmax <- data.frame()

gridcell.size = 1
Npatch_X = (Delta_X/gridcell.size); Npatch_Y = (Delta_Y/gridcell.size)
all.patches <- 1:(Npatch_X*Npatch_Y)


for (ifile in seq(1,length(data.files))){

  print(ifile/length(data.files))

  patches <- patchnumber_from_position(df.all[[ifile]][["X"]],df.all[[ifile]][["Y"]],
                                       gridcell.size,gridcell.size,
                                       X0 = -Delta_X/2,Y0 = -Delta_X/2)

  chmax <- df.all[[ifile]] %>% mutate(patch = patches[["patch"]],
                                      patch_x = patches[["patch_X"]],
                                      patch_y = patches[["patch_Y"]])%>% group_by(plot,Treatment,patch) %>% summarise(hmax = max(Z),
                                                                                                                      patch_x = patch_x[1],
                                                                                                                      patch_y = patch_y[1],
                                                                                                                      .groups = "keep")

  temp <- data.frame(plot = plot.num[ifile],
                     Treatment = Treatments[ifile],
                     patch = all.patches,
                     patch_x = rep(1:Npatch_X,Npatch_Y),
                     patch_y = sort(rep(1:Npatch_Y,Npatch_X)))


  df.hmax <- bind_rows(list(df.hmax,
                            temp %>% left_join(chmax,
                                               by = c("plot","Treatment","patch","patch_x","patch_y")) %>% mutate(hmax = case_when(is.na(hmax) ~ 0.,
                                                                                                                                   TRUE ~ hmax))))
}


df.hmax[["plot"]] <- factor(df.hmax[["plot"]],levels = c(sort(plot.num[Treatments == "C"]),
                                                         sort(plot.num[Treatments == "R"])))

# display.point.cloud( df.all[[1]] %>% filter(abs(X) <= 5,
#                                             abs(Y) <= 5))
pal<- (brewer.pal(n = 9,name = "Greys"))
ggplot(df.hmax) +
  geom_raster(aes(x = (patch_x - 1)*gridcell.size + gridcell.size/2 - Delta_X/2,
                  y = (patch_y - 1)*gridcell.size + gridcell.size/2 - Delta_Y/2,
                  fill = hmax)) +
  facet_wrap(~ as.factor(plot),nrow = 2) +
  scale_fill_gradientn(colours = pal[seq(length(pal),1,-1)]) +
  labs(x = "x (m)",y = "y (m)", fill = "Canopy height (m)") +
  theme_bw() +
  theme(text = element_text(size = 24))



ggplot(data = df.hmax) +
  geom_boxplot(aes(x = Treatment, y = hmax, fill = Treatment)) +
  labs(x = "",y = "Canopy mean height (m)") +
  theme_bw() +
  theme(text = element_text(size = 24),
        legend.position = c(0.1,0.92)) +
  guides(fill = FALSE)


df.hmax %>% group_by(Treatment) %>% summarise(m = mean(hmax),
                                              median = median(hmax))

summary(aov(lm(data = df.hmax, formula = hmax ~ Treatment)))

df.hmax$Treatment <- factor(df.hmax$Treatment,levels = c("C","R"))
levels(df.hmax$Treatment) <- c("Control","Removal")
ggplot(data = df.hmax) +
  geom_boxplot(aes(x = as.factor(plot), y = hmax, fill = Treatment)) +
  labs(x = "Plot",y = "Canopy mean height (m)") +
  scale_fill_manual(values = c('lightgrey',"black")) +
  theme_bw() +
  theme(text = element_text(size = 24),
        legend.position = c(0.1,0.92))

