rm(list = ls())

library(lidR)
library(dplyr)
library(ggplot2)
library(ggridges)
library(zoo)
library(Hmisc)

all.las <- readLAScatalog(paste0("~/Downloads/plot_squ/"))
# all.chm <- rasterize_canopy(all.las, 1)
# plot(all.chm, col = gray(1:50/50))

df <- df.z <- df.metrics <- data.frame()

tr <- c("C","R","R","C","R","C","R","C",
        "R","C","R","C","C","R","R","C")
Delta_z = 1
AllZ <- seq(0,45,Delta_z)
Delta_XY = 20

for (i in seq(1,16)){

  print(i)

  las <- readLAS(paste0("~/Downloads/plot_squ/plot",sprintf("%02d",i),"_squ.las"))

  chm <- rasterize_canopy(las, res = 1, algorithm = p2r())
  df <- bind_rows(list(df,
                       data.frame(h = as.vector(chm),
                                  plot = i,
                                  method = "ALS",
                                  treatment = tr[i])))

  cdf <- data.frame(z = floor(las$Z*Delta_z)/Delta_z) %>%
    group_by(z) %>%
    summarise(N = n()) %>%
    ungroup()

  cz <- cdf$z
  cN <- cdf$N

  allz <- AllZ
  allN <- approx(cz,cN,xout = allz,rule = 1)[["y"]]
  allN[is.na(allN)] <- 0

  df.z <- bind_rows(df.z,
                data.frame(z = allz,
                           N = allN) %>%
                      mutate(plot = i,
                             w = 1,
                             method = "ALS",
                             treatment = tr[i]))

  all.points <- data.frame(x = las$X,
                           y = las$Y,
                           z = las$Z) %>%
    mutate(x = x -min(x),
           y = y - min(y)) %>%
    filter(x < Delta_XY, y < Delta_XY) %>%
    mutate(patch = BCI.AGB::patchnumber_from_position(x,y,20,20)[["patch"]])

  df.metrics <- bind_rows(df.metrics,

                          all.points %>%
                            group_by(patch) %>%
                            summarise(vci = VCI(z, by = Delta_z, zmax = 20),
                                      RI = rumple_index(x,y,z),
                                      entropy = entropy(z, by = 1),
                                      .groups = "keep") %>%
                            mutate(plot = i,
                                   method = "ALS",
                                   treatment = tr[i]))

  # plot(gapFraction, type="l", xlab="Elevation", ylab="Gap fraction")


}

# TLS
plots <- c(5,7,8,11,12,13,14,16)
files <- c("plot05_squ_40m.las","plot07_squ_60m.las","plot08_squ_60m.las","plot11_squ_60m.las",
           "plot12_squ_40m.las","plot13_squ_60m.las","plot14_squ_40m.las","plot16_squ_40m.las")
w <- c(40*40,60*60,60*60,60*60,
       40*40,60*60,40*40,40*40)

for (ifile in seq(1,length(files))){
  cfile <- files[ifile] ; cplot <- plots[ifile] ; ctreatment <- tr[cplot] ; cw <- w[ifile]
  print(paste(cplot,ctreatment))
  tls <- readLAS(paste0("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/",cfile))

  chm <- rasterize_canopy(tls, res = 1, algorithm = p2r())
  df <- bind_rows(list(df,
                       data.frame(h = as.vector(chm),
                                  plot = cplot,
                                  treatment = ctreatment,
                                  method = "TLS")))

  cdf <- data.frame(z = floor(tls$Z*Delta_z)/Delta_z) %>%
    group_by(z) %>%
    summarise(N = n()) %>%
    ungroup()

  cz <- cdf$z
  cN <- cdf$N

  allz <- AllZ
  allN <- approx(cz,cN,xout = allz,rule = 1)[["y"]]
  allN[is.na(allN)] <- 0

  df.z <- bind_rows(df.z,
                    data.frame(z = allz,
                               N = allN) %>%
                      mutate(plot = cplot,
                             treatment = ctreatment,
                             w = cw,
                             method = "TLS"))

  all.points <- data.frame(x = tls$X,
                           y = tls$Y,
                           z = tls$Z) %>%
    mutate(x = x - min(x),
           y = y - min(y)) %>%
    filter(x < sqrt(cw), y < sqrt(cw)) %>%
    mutate(patch = BCI.AGB::patchnumber_from_position(x,y,Delta_XY,Delta_XY)[["patch"]])

  df.metrics <- bind_rows(df.metrics,

                          all.points %>%
                            group_by(patch) %>%
                            summarise(vci = VCI(z, by = Delta_z, zmax = 20),
                                      RI = rumple_index(x,y,z),
                                      entropy = entropy(z, by = 1),
                                      .groups = "keep") %>%
                            mutate(plot = cplot,
                                   method = "TLS",
                                   treatment = ctreatment))


}

ggplot(data = df.metrics) +
  geom_boxplot(aes(x = treatment, y = RI,
                   fill = method)) +
  theme_bw()

ggplot(data = df.metrics) +
  geom_boxplot(aes(x = method, y = vci,
                   fill = treatment)) +
  theme_bw()

ggplot(data = df.metrics) +
  geom_boxplot(aes(x = method, y = entropy,
                   fill = treatment)) +
  theme_bw()

df.metrics %>%
  group_by(method) %>%
  summarise(pval = summary(aov(lm(entropy ~ treatment)))[[1]][["Pr(>F)"]][1])

df.metrics %>%
  group_by(method) %>%
  summarise(pval = summary(aov(lm(vci ~ treatment)))[[1]][["Pr(>F)"]][1])

df.metrics %>%
  group_by(method) %>%
  summarise(pval = summary(aov(lm(RI ~ treatment)))[[1]][["Pr(>F)"]][1])


df.z <- df.z %>%
  mutate(N = case_when(z == 0 ~ 0,
                       z > 0 ~ N)) %>%
  group_by(plot,method) %>%
  mutate(d = N/sum(N)) %>%
  arrange(z) %>%
  mutate(N.cum = cumsum(N),
         d.cum = cumsum(d))

GEDI.shots <- readRDS("~/Documents/projects/LianaGEDI/data/outputs/Shots.RDS")
keep <- GEDI.shots %>%
  group_by(shot_number) %>%
  summarise(keep = !all(value_norm == 0)) %>% filter(keep) %>% pull(shot_number)
GEDIsum <- GEDI.shots %>%
  filter(shot_number %in% keep) %>%
  group_by(plotnum,Treatment,shot_number) %>%
  mutate(d = c(0,1/(diff(value_norm)))) %>%
  mutate(d = d/sum(d)) %>%
  rename(plot = plotnum)

# Interpolation/extrapolation
all.GEDI <- data.frame()
for (cplot in unique(GEDIsum$plot)){

  cdf <- GEDIsum %>%
    filter(plot == cplot)
  shotnumbers <- cdf$shot_number

  for (cshot in shotnumbers){
    ccdf <- cdf %>%
      filter(shot_number == cshot)

    cz <- ccdf$value_norm
    cd <- ccdf$d

    allz <- AllZ
    alld <- approx(cz,cd,xout = allz,rule = 1)[["y"]]
    alld[is.na(alld)] <- 0

    all.GEDI <- bind_rows(all.GEDI,
                          data.frame(z = c(allz,ceiling(max(ccdf$value_norm))+Delta_z),
                                     d = c(alld/sum(alld),0),
                                     w = 1,
                                     plot = cplot,
                                     shot_number = cshot,
                                     Treatment = unique(cdf$Treatment)))
  }
}

ggplot(data = df.z) +
  geom_line(aes(y = d,x = z, group = as.factor(plot),
                color = treatment)) +
  coord_flip() +
  facet_wrap(~ method) +
  theme_bw()

ggplot(data = df.z %>%
         filter(plot %in% GEDIsum[["plot"]])) +
  geom_line(aes(y = d,x = z, group = interaction(plot,method),
                linetype = method,
                color = treatment)) +
  geom_line(data = all.GEDI ,
            aes(y = d,x = z,
                group = interaction(shot_number,as.factor(plot)),
                color = Treatment),
            linetype = 3) +
  coord_flip() +
  facet_wrap(~ plot) +
  theme_bw()

comparison <-
  bind_rows(df.z,
            all.GEDI %>%
              mutate(method = "GEDI") %>%
              # dplyr::select(-shot_number) %>%
              rename(treatment = Treatment)) %>%
  mutate(shot_number = case_when(is.na(shot_number) ~ "1",
                                 TRUE  ~ shot_number))

comparison.sum <- comparison %>%
  group_by(treatment,method,z) %>%
  summarise(d.m = weighted.mean(d,w,
                       na.rm = TRUE),
            d.sd = sqrt(wtd.var(d,w,
                      na.rm = TRUE)),
            .groups = "keep") %>%
  group_by(treatment,method) %>%
  mutate(d.m = d.m/sum(d.m))

ggplot(data = comparison.sum) +
  geom_ribbon(aes(y = d.m,ymin = pmax(0,d.m - d.sd),
                  ymax = pmax(0,d.m + d.sd),
                  x = z, fill = treatment),
              color = NA,  alpha = 0.3) +
  geom_line(aes(y = d.m,x = z,
                color = treatment)) +

  coord_flip() +
  facet_grid(~ method) +
  theme_bw()



ggplot(data = comparison %>%
         filter(plot %in% c(7,12))) +
  geom_line(aes(y = d,x = z, group = interaction(plot,method,shot_number),
                linetype = shot_number,
                color = treatment),show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ method) +
  theme_bw()

ggplot(data = df.z) +
  geom_line(aes(y = d.cum,x = z,
                group = plot,
                color = treatment)) +
  coord_flip() +
  facet_wrap(~ method) +
  theme_bw()

ggplot(data = df.z) +
  geom_line(aes(y = d,x = z,
                group = plot,
                color = treatment)) +
  coord_flip() +
  facet_wrap(~ method) +
  theme_bw()

ggplot(data = df.z) +
  geom_line(aes(y = N,x = z, group = as.factor(plot),
                color = treatment)) +
  coord_flip() +
  facet_wrap(~ method) +
  theme_bw()

ggplot(data = df.z) +
  geom_line(aes(y = N.cum,x = z, group = as.factor(plot),
                color = treatment)) +
  coord_flip() +
  facet_wrap(~ method) +
  scale_y_log10() +
  theme_bw()


df <- df %>%
  mutate(plot = factor(plot,
                       levels = c(1,4,6,8,10,12,13,16,
                                  2,3,5,7,9,11,14,15)))

ggplot(df) +
  geom_density(aes(x = h,fill = treatment),color = NA,
               alpha = 0.5) +
  facet_wrap(~ method) +
  theme_bw()


ggplot(df) +
  geom_density(aes(x = h,fill = treatment,
                   group = as.factor(plot)),color = "black",
               alpha = 0.2) +
  facet_wrap(~ method) +
  theme_bw()

ggplot(df,
       aes(x = h,fill = treatment,
           y = plot)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 0.5) +
  facet_wrap(~ method) +
  labs(fill = "") +
  theme_bw() +
  theme(legend.position = c(0.9,0.1))

plot.commons <- as.numeric(as.character(df %>%
  group_by(plot) %>%
  summarise(N = length(unique(method)),
            .groups = "keep") %>%
  filter(N >= 2) %>%
  pull(plot)))

ggplot(df %>%
         filter(plot %in% plot.commons),
       aes(x = h,fill = treatment, group = interaction(method,plot),
           y = interaction(method,plot))) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 0.5) +
  # facet_wrap(~ treatment) +
  theme_bw()


ggplot(df,
       aes(x = treatment,y = h,fill = treatment)) +
  geom_boxplot(alpha = 0.5) +
  # geom_violin(fill = NA) +
  facet_wrap(~ method) +
  theme_bw()

ggplot(df,
       aes(x = as.factor(plot),y = h,fill = treatment)) +
  geom_boxplot(alpha = 0.5,
               outlier.shape = NA) +
  facet_wrap(~ method) +
  # geom_violin(fill = NA) +
  theme_bw()
