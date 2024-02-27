rm(list = ls())

library(dplyr)
library(geodata)
library(stringr)
library(plotbiomes)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(YGB)
library(xlsx)
library(reshape2)
library(raster)

Nbootstrap = 4000

# op.delta.loss <- readRDS("./outputs/Bootstrap.RDS")
op.delta.loss <- data.frame()
###############################################################################

days <- c(31,28,31,30,31,30,31,31,30,31,30,31)
all.vars <- c("t.sd","MCWD")

# Boundaries for interpolation
lat.max = 12 ; lat.min = -31
temp.min = 17 ; temp.max = 29
MCWD.min = 450 ; MCWD.max = 0
t.sd.max = 0.2

DBHtargets <- c(seq(10,50,10),100,200)
data.dis <- read.xlsx("~/Downloads/nph17995-sup-0002-datasets2.xlsx",
                      sheetName = "datas2") %>%
  filter(variable == "AGB",
         site %in% c("BCI","Danum Valley","Pasoh",
                      "Lenda","Edoro","Amacayacu",
                      "Lambir","Korup","Wanang","Sinharaja",
                      "Cocoli","San Lorenzo","Mudumalai",
                      "Luquillo","Palamanui","Laupahoehoe","Fushan")) %>%
  group_by(site) %>%
  mutate(dbh.max = (c(5,10,20,30,40,50,100,200,500))[1:n()]) %>%
  mutate(total = as.numeric(total))

alpha = 0.11

##############################################################################

# Avitabile map
Avi.repojected <- raster("/home/femeunier/Documents/projects/YGB/data/Avitabile_AGB_Map/Avitabile_AGB_Map_reproj.tif")

# Climate
r.tavg <- worldclim_global("tavg",
                           path = "./data/WorldClim/", version="2.1",
                           res = 10)

r.tavg.cropped <- crop(r.tavg,extent(Avi.repojected))

df.tavg <- as.data.frame(r.tavg.cropped, xy = TRUE) %>%
  pivot_longer(cols = starts_with("wc2.1_10m_tavg_")) %>%
  mutate(month = as.numeric(sub(".*\\_tavg_", "", name))) %>%
  group_by(x,y) %>%
  summarise(MAT = weighted.mean(value,days),
            t.sd = sd(value)/weighted.mean(value,days),
            .groups = "keep") %>%
  mutate(x = round(x,digits = 3),
         y = round(y,digits = 3))

r.prec <- worldclim_global("prec",
                           path = "./data/WorldClim/", version="2.1",
                           res = 10)
r.prec.cropped <- crop(r.prec,extent(Avi.repojected))

df.prec <- as.data.frame(r.prec.cropped,xy = TRUE) %>%
  pivot_longer(cols = starts_with("wc2.1_10m_prec_")) %>%
  mutate(month = as.numeric(sub(".*\\_prec_", "", name))) %>%
  group_by(x,y) %>%
  summarise(MAP = sum(value),
            MCWD = -calc.MCWD(value - days*3.33),
            .groups = "keep") %>%
  mutate(x = round(x,digits = 3),
         y = round(y,digits = 3))


##################
# Carefull: needs to update the maps (all latitudes +/- 31°!!)
#################

A <- raster("/home/femeunier/Documents/projects/YGB/data/Avitabile_AGB_Map/Avitabile_AGB_Map_reproj.tif")
names(A) <- "AGB"
B <- raster("./data/GEDI.repojected.tif")/20
names(B) <- "AGB"
C <- raster("./data/ESA.reprojected.tif")/20
names(C) <- "AGB"
D <- raster("./data/all.grid.tif")
names(D) <- "AGB"

Avi.repojected <- list(A,
                       B,
                       C,
                       D)

# models <- TrENDY.analyses::get.model.names.TRENDY()
# compt <- length(Avi.repojected) + 1
# for (cmodel in models){
#
#   cfile <- paste0("./data/",cmodel,".grid.tif")
#   if (file.exists(cfile)){
#     cgrid <- raster(cfile)
#     names(cgrid) <- "AGB"
#     Avi.repojected[[compt]] <- cgrid
#     compt <- compt + 1
#   }
# }


df.map <- bind_rows(
  data.frame(source = "Avitabile",
             value = as.data.frame(A,xy = TRUE)),
  data.frame(source = "GEDI",
             value = as.data.frame(B,xy = TRUE)),
  data.frame(source = "ESA",
             value = as.data.frame(C,xy = TRUE)),
  data.frame(source = "Trendy",
             value = as.data.frame(D,xy = TRUE))) %>%
  filter(value.AGB > 0)

ggplot(data = df.map) +
  geom_density(aes(x = value.AGB, fill = source), alpha = 0.5, color = NA) +
  # scale_x_log10() +
  theme_bw()


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = df.map) +
  geom_raster(aes(x=value.x,y = value.y,
                  fill = value.AGB),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(ylim = c(-32, 15), expand = FALSE) +
  scale_fill_gradient(low = "white", high = "darkgreen",na.value = NA) +
  labs(x = "",y = "") +
  facet_wrap(~ source) +
  theme_bw()

##############################################################################
# Main loop

for (ibootstrap in seq((max(op.delta.loss$i)+1),Nbootstrap)){

  print(paste0("Bootstrapping: ",ibootstrap,"/",Nbootstrap))

  # Choose one map

  selected.map <- sample(1:length(Avi.repojected),1)
  df.Avi <- as.data.frame(Avi.repojected[[selected.map]],xy = TRUE) %>%
    mutate(x = round(x,digits = 3),
           y = round(y,digits = 3))

  df.var <- df.prec %>%
    left_join(df.tavg,
              by = c("x","y")) %>%
    left_join(df.Avi,
              by = c("x","y"))

  lats <- seq(min(df.var$y)-1/6,max(df.var$y)+1/6,1/6)
  lons <- seq(min(df.var$x)-1/6,max(df.var$x)+1/6,1/6)

  Gridarea <- RCMIP5:::calcGridArea(lon = lons,
                                    lat = lats) %>%
    melt() %>% mutate(Var1 = lons[Var1],
                      Var2 = lats[Var2]) %>%
    rename(x = Var1,
           y = Var2,
           area = value)


  df.var.area <- df.var %>%
    mutate(x = round(x,digits = 3),
           y = round(y,digits = 3)) %>%
    left_join(Gridarea %>%
                mutate(x = round(x,digits = 3),
                       y = round(y,digits = 3)),
              by = c("x","y"))


  Weights <- c()

  data.dis.select <- data.dis %>%
    ungroup() %>%
    filter(site == sample(site,size = 1))

  cdata.dis.select <- data.dis.select

  cDBHtargets <- DBHtargets[sort(sample(1:length(DBHtargets),
                                        size = sample(1:length(DBHtargets),1)))]


  for (iDBHtarget in seq(1,length(cDBHtargets))){

    cdf <- cdata.dis.select %>%
      filter(dbh.max <= cDBHtargets[iDBHtarget])

    cdata.dis.select <- cdata.dis.select %>%
      filter(dbh.max > cDBHtargets[iDBHtarget])

    Weights <- c(Weights,
                sum(cdf %>% pull(total)) )
  }

  Weights[length(Weights)] <-   Weights[length(Weights)] + sum(cdata.dis.select$total)

  Weights <- Weights/sum(Weights)


  ############################################################################
  # Step 1 relationship

  print("- Fit climate/liana effect")

  site.loc <- readRDS("./outputs/site.loc.RDS") %>%
    mutate(continent = case_when(lon <= -35 ~ "America",
                                 lon <= 50 ~ "Africa",
                                 TRUE ~ "Australasia"))

  t.sd <- MCWD <- MAT <- c()

  for (isite in seq(1,nrow(site.loc))){

    clon <- site.loc$lon[isite] ; clat <- site.loc$lat[isite]
    xy <- rbind(c(clon,clat))
    p <- vect(xy, crs="+proj=longlat +datum=WGS84")

    r.tavg <- worldclim_tile("tavg",
                             clon,clat,
                             path = "./data/WorldClim/", version="2.1",res = 0.5)
    t.avg <- as.numeric(as.vector(raster::extract(r.tavg, xy)))
    MAT[isite] <- weighted.mean(t.avg,days)
    t.sd[isite] <- sd(t.avg)/MAT[isite]

    r.prec <- worldclim_tile("prec",
                             clon,clat,
                             path = "./data/WorldClim/", version="2.1",res = 0.5)
    prec.avg <- as.numeric(as.vector(raster::extract(r.prec, xy)))
    e.avg <- days*3.33
    MCWD[isite] <- calc.MCWD(prec.avg - e.avg)

  }

  site.loc$MCWD <- NA ; site.loc$t.sd <- NA
  site.loc$MCWD[1:length(MCWD)] <- -MCWD ; site.loc$t.sd[1:length(t.sd)] <- t.sd


  df.all.effects <- df.r2 <- df.r2.single <- df.model.var <-
    df.model.all <-
    data.frame()

  LM.all <- list() ; m.effect <- c()

  Var1 = all.vars[1] ; Var2 = all.vars[2]

  for (iDBHtarget in seq(1,length(cDBHtargets))){

    cDBHtarget <- cDBHtargets[iDBHtarget]

    Main.OP <- readRDS(paste0("./outputs/Main.OP.",cDBHtarget,".RDS"))

    diff.H.sites <- Main.OP %>%
      group_by(site,liana.cat) %>%
      summarise(m = median(diff_h/no*100,na.rm = TRUE),
                sd = sd(diff_h/no*100,na.rm = TRUE),
                Qlow = quantile(diff_h/no*100,alpha/2,na.rm = TRUE),
                Qhigh = quantile(diff_h/no*100,1-alpha/2,na.rm = TRUE),

                m.abs = median(diff_h,na.rm = TRUE),
                low.abs = quantile(diff_h,alpha/2,na.rm = TRUE),
                high.abs = quantile(diff_h,1-alpha/2,na.rm = TRUE),
                .groups = "keep") %>%
      pivot_wider(names_from = liana.cat,
                  values_from = -c(site,liana.cat))

    effect.site <- site.loc %>%
      rename(site = site.common) %>%
      left_join((A <- diff.H.sites %>%
                  group_by(site) %>%
                  mutate(m_high_sample = rnorm(n = 1,
                                               mean = m_high,
                                               sd = sd_high)) %>%
                  dplyr::select(site,m_high_sample,m_high)),
                by = "site") %>%
      mutate(continent = factor(continent,
                                levels = c("America","Africa","Australasia"))) %>%
      ungroup() %>%
      mutate(w = Nlarge) %>%
      filter(dbh.max >= cDBHtarget)

    m.effect[iDBHtarget] <- weighted.mean(effect.site$m_high_sample,
                                          w = effect.site$Nlarge)

    LM.all[[iDBHtarget]] <- lm(data = effect.site,
                               formula = (m_high_sample) ~ get(Var1) + get(Var2),
                               weights = w)

  }

  rownames(df.model.var) <- rownames(df.model.all) <- NULL

  ###############################################################################
  # Step 2: upscaling Delta_H
  print("- Upscaling Delta_H")

  df.var.area.all <- data.frame()
  df.coef <- data.frame()

  for (iDBHtarget in seq(1,length(cDBHtargets))){
    cDBHtarget <- cDBHtargets[iDBHtarget]

    best.model <- LM.all[[iDBHtarget]]

    newdata = data.frame(MAT = df.var.area$MAT,
                         t.sd = df.var.area$t.sd,
                         MAP = df.var.area$MAP,
                         MCWD = df.var.area$MCWD)

    df.coef <- bind_rows(df.coef,
                         data.frame(intercept = as.numeric(coef(best.model)[1]),
                                    var1 = as.numeric(coef(best.model)[2]),
                                    var2 = as.numeric(coef(best.model)[3]),
                                    dbh = cDBHtargets[iDBHtarget],
                                    m.high = m.effect[iDBHtarget]))

    df.var.area$delta <- predict(best.model,
                                 newdata = newdata)

    df.var.area <- df.var.area %>%
      ungroup() %>%
      mutate(delta_bound = case_when(MAT < temp.min | MAT > temp.max | MCWD > MCWD.min | MCWD < MCWD.max |
                                       t.sd > t.sd.max ~ NA_real_,
                                     TRUE ~ delta),
             AGB.bound = case_when(MAT < temp.min | MAT > temp.max | MCWD > MCWD.min | MCWD < MCWD.max |
                                     t.sd > t.sd.max ~ NA_real_,
                                   TRUE ~ AGB)) %>%
      mutate(delta_AGB = delta_bound/100*AGB)


    df.var.area.all <- bind_rows(df.var.area.all,
                                 df.var.area %>%
                                   mutate(target = cDBHtarget))

  }

  ###############################################################################
  # Step 3: upscaling Delta_Biomass

  print("- Computing Delta Biomass")


  df.link <- data.frame(target = cDBHtargets,
                        W = Weights)

  df.average <- df.var.area.all %>%
    left_join(df.link,
              by = "target") %>%
    filter(y <= lat.max,
           y >= lat.min) %>%
    group_by(x,y) %>%
    summarise(delta_bound_av = sum(delta_bound/100*W),
              delta_AGB = sum(delta_bound/100*AGB*W),
              AGB.av = AGB.bound[1],
              MAP = MAP[1],MCWD = MCWD[1],
              MAT = MAT[1],t.sd = t.sd[1],
              area = area[1],
              .groups = "keep")


  # stop()
  # ggplot() +
  #
  #   geom_raster(data = df.var.area.all %>%
  #                 filter(!is.na(delta_bound)) %>%
  #               filter((y) <= lat.max, y >= lat.min),
  #             aes(x = x, y = y,fill = delta_bound),
  #             alpha = 1) +
  #   scale_fill_gradient2(low = "darkred",mid = "white",high = "darkgreen",na.value = "lightgrey",
  #                        limits = c(-30,1),
  #                        oob=scales::squish) +
  #   geom_sf(data = world,
  #           fill = NA,
  #           color = "black",
  #           alpha = 0.5) +
  #   coord_sf(xlim = c(-120, 170), ylim = c(lat.min, lat.max), expand = FALSE) +
  #   facet_wrap(~ target, ncol = 1) +
  #   # theme_map() +
  #   guides(size = "none")


  op <- sum(df.average$delta_AGB*df.average$area,na.rm = TRUE)*1000/1e15
  op2 <-   sum(df.average$delta_AGB*df.average$area,na.rm = TRUE)*1000/1e15 /
    (sum(df.average$AGB.av*df.average$area,na.rm = TRUE)*1000/1e15)
  op3 <- mean(df.average$delta_bound_av,na.rm = TRUE)
  op.delta.loss <- bind_rows(op.delta.loss,
                             data.frame(i = ibootstrap,
                                        op,op2,op3,
                                        target = paste(cDBHtargets,collapse = "_"),
                                        map = selected.map,
                                        w = unique(data.dis.select$site),
                                        df.coef))

  # df.average %>%
  #   ungroup() %>%
  #   mutate(continent = case_when(x <= -35 ~ "America",
  #                                x <= 50 ~ "Africa",
  #                                TRUE ~ "Australasia")) %>%
  #   group_by(continent) %>%
  #   summarise(AGBloss = sum(area*delta_AGB*1000/1e15,na.rm = TRUE),
  #             forest = sum(area*AGB.av*1000/1e15,na.rm = TRUE),
  #             .groups = "keep") %>%
  #   mutate(frac = 100*AGBloss/forest)
  #
  #
  # df.average %>%
  #   ungroup() %>%
  #   mutate(continent = case_when(x <= -35 ~ "America",
  #                                x <= 50 ~ "Africa",
  #                                TRUE ~ "Australasia")) %>%
  #   group_by(continent) %>%
  #   filter(delta_AGB != 0) %>%
  #   summarise( m = 20*mean(delta_AGB,na.rm = TRUE),
  #              med = 20*median(delta_AGB,na.rm = TRUE),
  #              min = 20*min(delta_AGB,na.rm = TRUE),
  #              max = 20*max(delta_AGB,na.rm = TRUE),
  #              .groups = "keep")
  #
  # df.var.area.all %>%
  #   mutate(continent = case_when(x <= -35 ~ "America",
  #                                x <= 50 ~ "Africa",
  #                                TRUE ~ "Australasia")) %>%
  #   mutate(continent = factor(continent,
  #                             levels = c("America","Africa","Australasia"))) %>%
  #   filter((y) <= lat.max, y >= lat.min,
  #          target == 50,
  #          delta_AGB != 0) %>%
  #   group_by(continent) %>%
  #   summarise(m = mean(delta_bound,na.rm = TRUE),
  #             med = median(delta_bound,na.rm = TRUE),
  #             Min = min(delta_bound,na.rm = TRUE),
  #             Max = max(delta_bound,na.rm = TRUE))

}

hist(op.delta.loss$op)
hist(op.delta.loss$op2)
hist(op.delta.loss$op3)

ggplot(data = op.delta.loss) +
  geom_boxplot(aes(x = as.factor(map), y = op)) +
  theme_bw()

ggplot(data = op.delta.loss) +
  geom_boxplot(aes(x = as.factor(w), y = op2)) +
  theme_bw()

ggplot(data = op.delta.loss) +
  geom_boxplot(aes(x = as.factor(target), y = op2)) +
  theme_bw()

ggplot(data = op.delta.loss,
       aes(x = m.high, y = op2, color = as.factor(map))) +
  geom_point() +
  stat_smooth(method = "lm") +
  facet_wrap(~ as.factor(dbh)) +
  theme_bw()

summary(lm(data = op.delta.loss,
   op2 ~ target + map + w + var1 + var2 ))
# saveRDS(op.delta.loss,
#         file = "./outputs/Bootstrap.RDS")


