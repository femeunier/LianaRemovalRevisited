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

directory <- "/home/femeunier/Downloads/"

file.names <- c("N_raster(0.5)_statis_plot5.txt","N_raster(0.5)_statis_plot8.txt")

Treatments <- c("R","C")

plot.num <- c(5,8)

data.files <- file.path(directory,file.names)

df.all <- list()

Delta_X = 65 ; Delta_Y = 65
gridcell.size = 1                                          # Size of the binning gricell (in meter)

############################################################
# User-defined functions

extremum <- function(vector, ...){
  return(c(min(vector, ...), max(vector, ...)))
}

patchnumber_from_position <- function(x,y,patch_X,patch_Y,X0 = 0,Y0 = 0){

  extr_x <- (extremum(x,na.rm = TRUE))
  extr_y <- (extremum(y,na.rm = TRUE))

  N_X <- ceiling((extr_x[2]-extr_x[1])/patch_X)
  N_Y <- ceiling((extr_y[2]-extr_y[1])/patch_Y)

  N_x_float <- (extr_x[2]-extr_x[1])/patch_X
  N_y_float <- (extr_y[2]-extr_y[1])/patch_Y

  x <- (x - X0)
  y <- (y - Y0)

  x[x < 0] <- x[x < 0] + N_x_float*patch_X
  y[y < 0] <- y[y < 0] + N_y_float*patch_Y

  N <- N_X*N_Y

  ix = 1 + (x - x%%patch_X)/patch_X
  iy = 1 + (y - y%%patch_Y)/patch_Y

  patch = (iy-1)*N_X + ix

  patch.size <- rep(patch_X*patch_Y,length(x))

  if (N_x_float != N_X) {
    patch.size[ix == N_X] <- (N_x_float - (N_X - 1))*patch.size[ix == N_X]
  }

  if (N_y_float != N_Y) {
    patch.size[iy == N_Y] <- (N_y_float - (N_Y - 1))*patch.size[iy == N_Y]
  }

  return(list(patch = patch,
              patch_X = ix,
              patch_Y = iy,
              patch_size = patch.size))
}

############################################################

data.files <- file.path(directory,file.names)
df.all <- list()

# Loop over all files
for (ifile in seq(1,length(data.files))){

  # Reading
  cdata <- fread(data.files[ifile])
  colnames(cdata) <- c("V1","V2","V3")

  # Filtering the data points
  cdata.filter <- cdata %>% filter(V1 >= -Delta_X/2, V1 < Delta_X/2,
                                   V2 >= -Delta_Y/2, V2 < Delta_Y/2,
                                   V3 >= 0)
  # Rename and add the treatments
  cdata.filter.renamed <- cdata.filter %>% rename(X = V1, Y = V2, Z = V3) %>%
    mutate(plot = plot.num[ifile],
           Treatment = Treatments[ifile])

  # Supplement the list of outpts
  df.all[[ifile]] <- cdata.filter.renamed

}

# Binning and computing the maximum height
df.hmax <- data.frame()

# Number of gricells in the X (Npatch_X), Y (Npatch_Y) directions, and total number of pixels (all.patches)

# Loop over all files
for (ifile in seq(1,length(data.files))){

  print(ifile/length(data.files))

  # Group the data point into the corresponding gridcell
  patches <- patchnumber_from_position(x = df.all[[ifile]][["X"]],
                                       y = df.all[[ifile]][["Y"]],
                                       patch_X = gridcell.size,
                                       patch_Y = gridcell.size,
                                       X0 = -Delta_X/2,
                                       Y0 = -Delta_Y/2)


  Npatch_X = abs(ceiling((diff(extremum(df.all[[ifile]][["X"]]))/gridcell.size)))
  Npatch_Y = abs(ceiling((diff(extremum(df.all[[ifile]][["Y"]]))/gridcell.size)))
  all.patches <- 1:(Npatch_X*Npatch_Y)

  # Get the maximum height for each grifcell
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

  # Combine all plots
  df.hmax <- bind_rows(list(df.hmax,
                            temp %>% left_join(chmax,
                                               by = c("plot","Treatment","patch","patch_x","patch_y")) %>% mutate(hmax = case_when(is.na(hmax) ~ 0.,
                                                                                                                                   TRUE ~ hmax))))
}

# Sort by treatment
df.hmax[["plot"]] <- factor(df.hmax[["plot"]],levels = c(sort(plot.num[Treatments == "C"]),
                                                         sort(plot.num[Treatments == "R"])))

# Color palette = greys
pal<- brewer.pal(n = 9,name = "Greys")

# Plot topview
ggplot(df.hmax) +
  geom_raster(aes(x = (patch_x - 1)*gridcell.size + gridcell.size/2 - Delta_X/2,
                  y = (patch_y - 1)*gridcell.size + gridcell.size/2 - Delta_Y/2,
                  fill = hmax)) +
  facet_wrap(~ as.factor(plot),nrow = 2) +
  scale_fill_gradientn(colours = pal[seq(length(pal),1,-1)]) +
  labs(x = "x (m)",y = "y (m)", fill = "Canopy height (m)") +
  theme_bw() +
  theme(text = element_text(size = 24))


# Boxplot ~ Treatment
ggplot(data = df.hmax) +
  geom_boxplot(aes(x = Treatment, y = hmax, fill = Treatment)) +
  labs(x = "",y = "Canopy mean height (m)") +
  theme_bw() +
  theme(text = element_text(size = 24),
        legend.position = c(0.1,0.92)) +
  guides(fill = FALSE)

# Stats and p-value
df.hmax %>% group_by(Treatment) %>% summarise(m = mean(hmax),
                                              median = median(hmax))

summary(aov(lm(data = df.hmax, formula = hmax ~ Treatment)))

df.hmax$Treatment <- factor(df.hmax$Treatment,levels = c("C","R"))
levels(df.hmax$Treatment) <- c("Control","Removal")

# Boxplot ~ plot
ggplot(data = df.hmax) +
  geom_boxplot(aes(x = as.factor(plot), y = hmax, fill = Treatment)) +
  labs(x = "Plot",y = "Canopy mean height (m)") +
  scale_fill_manual(values = c('lightgrey',"black")) +
  theme_bw() +
  theme(text = element_text(size = 24),
        legend.position = c(0.1,0.92))


ggplot(data = df.hmax) +
  geom_density(aes(x = hmax, fill = Treatment), alpha = 0.4) +
  theme_bw()
