rm(list = ls())

library(dplyr)
library(geodata)
library(stringr)
library(plotbiomes)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(raster)
library(YGB)
library(terra)
library(SPEI)

site.loc <- readRDS("./outputs/site.loc.RDS") %>%
  mutate(continent = site.group)
continents <- unique(site.loc$continent)

days <- c(31,28,31,30,31,30,31,31,30,31,30,31)

MAT <- t.sd <-
  MAP <- MCWD <- Prec.sd <-
  VPD <- VPD.sd <-
  srad <- srad.sd <- c()

rastlist <- list.files(path = "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/WorldClim/vapr", pattern='.tif$',
                       all.files=TRUE, full.names=TRUE)
rastlist2 <- list.files(path = "/home/femeunier/Documents/projects/LianaRemovalRevisited/data/WorldClim/srad", pattern='.tif$',
                       all.files=TRUE, full.names=TRUE)

#import all raster files in folder using lapply
r.vapr <- rast(lapply(rastlist, rast))
r.srad <- rast(lapply(rastlist2, rast))

for (isite in seq(1,nrow(site.loc))){

  print(isite)
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
  e.avg <- SPEI::thornthwaite(t.avg,clat,verbose = FALSE)

  MCWD[isite] <- calc.MCWD(prec.avg - e.avg)

  MAP[isite] <- sum(prec.avg)
  Prec.sd[isite] <- sd(prec.avg)/MAP[isite]

  ##############################################################################
  # Incident solar radiation

  # r.srad <- worldclim_tile("srad",
  #                          clon,clat,
  #                          path = "./data/WorldClim/", version="2.1",res = 0.5)
  r.srad.avg <- as.numeric(as.vector(raster::extract(r.srad, xy)))
  srad[isite] <- weighted.mean(r.srad.avg*days,days)
  srad.sd[isite] <- sd(r.srad.avg)/srad[isite]

  # VPD
  r.vapr.avg <- as.numeric(as.vector(raster::extract(r.vapr, xy)))
  vpsat = 610.7*10**((7.5*t.avg)/(237.3 + t.avg))/1000
  vpd <- vpsat - r.vapr.avg

  VPD[isite] <- weighted.mean(vpd,days)
  VPD.sd[isite] <- sd(vpd)/VPD[isite]

}

site.loc$MAP <- NA ; site.loc$MAT <- NA ; site.loc$MCWD <- NA ; site.loc$t.sd <- NA
site.loc$VPD <- NA ; site.loc$VPD.sd <- NA ; site.loc$Prec.sd <- NA
site.loc$srad <- NA ; site.loc$srad.sd <- NA

site.loc$MAP[1:length(MAP)] <- MAP ; site.loc$MAT[1:length(MAT)] <- MAT
site.loc$MCWD[1:length(MCWD)] <- -MCWD ; site.loc$t.sd[1:length(t.sd)] <- t.sd
site.loc$VPD[1:length(VPD)] <- VPD ; site.loc$VPD.sd[1:length(VPD.sd)] <- VPD.sd
site.loc$Prec.sd[1:length(Prec.sd)] <- Prec.sd
site.loc$srad[1:length(srad)] <- srad ; site.loc$srad.sd[1:length(srad.sd)] <- srad.sd

site.loc <- site.loc %>%
  mutate(forest.type = case_when(MCWD <= 250 ~ "Rainforest",
                                 MAP <  1000 ~ "Woodland",
                                 TRUE ~ "Dry forest"))

Whittaker_biomes.text <- plotbiomes::Whittaker_biomes %>%
  group_by(biome) %>%
  summarise(temp_c = mean(temp_c),
            precp_cm = mean(precp_cm),
            .groups = "keep") %>%
  mutate(temp_c = case_when(biome == "Woodland/shrubland" ~ 13,
                            biome == "Temperate seasonal forest" ~ 10,
                            biome == "Temperate rain forest" ~ 15,
                            biome == "Subtropical desert" ~ 25,
                            biome == "Tropical rain forest" ~ 25,
                            biome == "Temperate grassland/desert" ~ 8,
                            biome == "Tropical seasonal forest/savanna" ~ 25,
                            TRUE ~ temp_c),
         precp_cm = case_when(biome == "Tundra" ~ 20,
                              biome == "Woodland/shrubland" ~ 70,
                              biome == "Temperate seasonal forest" ~ 150,
                              biome == "Temperate rain forest" ~ 250,
                              biome == "Subtropical desert" ~ 35,
                              biome == "Temperate grassland/desert" ~ 15,
                              biome == "Tropical rain forest" ~ 325,
                              biome == "Tropical seasonal forest/savanna" ~ 150,
                              TRUE ~ precp_cm),
         biome = case_when(biome == "Tropical seasonal forest/savanna" ~ "Tropical seasonal \r\n forest/savanna",
                           TRUE ~ biome))


ggplot() +

  geom_polygon(data = plotbiomes::Whittaker_biomes,
               aes(x = temp_c, y = precp_cm, fill = biome),
               color = "black",
               alpha = 0.5, linewidth = 0.5) +
  geom_point(data = site.loc,
             aes(x=MAT, y=MAP/10), size = 0.1) +
  geom_text(data = Whittaker_biomes.text,
            aes(x = temp_c, y = precp_cm, label = biome),
            size = 3.5)  +
  scale_fill_manual(name = "Whittaker biomes",
                    breaks = names(Ricklefs_colors),
                    labels = names(Ricklefs_colors),
                    values = Ricklefs_colors) +

  labs(x = "Temperature (°C)",
       y = "Precipitation (cm)") +
  theme_light() +
  theme(legend.position = c(0.08,0.75)) +
  guides(fill = "none")

ggplot(data = site.loc %>%
         dplyr::select(site.common,continent,
                       MAP,MAT,Prec.sd,MCWD,t.sd,VPD,VPD.sd,srad,srad.sd) %>%
         pivot_longer(cols = - c(site.common,continent))) +
  geom_density(aes(x = value, fill = continent),
               alpha = 0.5) +
  facet_wrap(~ name, scales = "free") +
  theme_bw()

site.loc <- site.loc %>%
  mutate(AI = abs(MAP)/-(MCWD +1e-10)) %>%
  mutate(CZ = case_when(AI < -3.8 & MAP > 1700 ~ "Humid1700",
                        AI < -3.8 ~ "Humid",
                        AI < -1.8 ~ "Humid_seasonal",
                        TRUE ~ "Else"))

ggplot(data = site.loc) +
  geom_point(aes(y = MAP, x = -MCWD,
                 color = CZ), alpha = 1) +
  geom_abline(intercept = c(0,0),
              slope = c(-1.8,-3.8),
              linetype = 2,
              color = "darkgrey") +
  geom_segment(data = data.frame(x = -1700/3.8,
                                 xend = 0,
                                 y = 1700,
                                 yend = 1700),
               aes(x = x, xend = xend, y = y, yend = yend),
               linetype = 2,
               color = "darkgrey") +
  # geom_hline(yintercept = 1000,linetype = 2, color = "black") +
  # geom_vline(xintercept = -250,linetype = 2, color = "black") +
  scale_x_continuous(limits = c(-500,0),
                     expand = c(0,10)) +
  scale_y_continuous(limits = c(0,4000)) +
  labs(x = "",y = "", color = "") +
  theme_bw() +
  theme(text = element_text(size = 24)) +
  scale_color_manual(values = c(Humid1700 = "#1C2C04",
                                Humid = "#345C0C",
                                Humid_seasonal = "#4C8C14")) +
  guides(color = "none")

site.loc %>%
  group_by(CZ) %>%
  summarise(N = n(),
            .groups = "keep")


################################################################################

DBHtargets <- c(25,50,100,150)

df.all.effects <- df.r2 <- df.r2.single <- df.model.var <-
  df.model.all <-
  data.frame()
all.vars <- c("t.sd","MAP","MCWD","MAT","VPD","VPD.sd",
              "Prec.sd","srad","srad.sd")

Var1 = "VPD" ; Var2 = "MCWD"

for (iDBHtarget in seq(1,length(DBHtargets))){

  cDBHtarget <- DBHtargets[iDBHtarget]

  print(cDBHtarget)

  Main.OP <- readRDS(paste0("./outputs/Main.OP.",cDBHtarget,".RDS"))

  alpha = 0.11
  diff.H.sites <- Main.OP %>%
    group_by(site,liana.cat) %>%
    summarise(m = median(diff_h/no*100,na.rm = TRUE),
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
    left_join(diff.H.sites %>%
                dplyr::select(site,m_high,m.abs_high,
                              Qlow_high,Qhigh_high),
              by = "site") %>%
    mutate(continent = factor(continent,
                              levels = continents)) %>%
    ungroup() %>%
    mutate(w = Nlarge) %>%
    filter(dbh.max >= cDBHtarget)

  df.all.effects <- bind_rows(df.all.effects,
                              effect.site %>%
                                mutate(target = cDBHtarget))

  for (var in all.vars){

    lm.var <- lm(data = effect.site,
                 formula = m_high ~ get(var), weights = w)

    df.r2.single <- bind_rows(df.r2.single,
                              data.frame(target = cDBHtarget,
                                         variable = var,
                                         r2 = summary(lm.var)[["adj.r.squared"]],
                                         p.value = summary(lm.var)$coefficients[2,4],
                                         N = nrow(effect.site)))

    vars <- seq(min(effect.site[[var]]),
                max(effect.site[[var]]),
                length.out=1000)
    newdata <- data.frame(vars)
    colnames(newdata) <- var
    predictions <- predict(lm.var,newdata = newdata,
                           interval = "confidence")
    df.predictions <- as.data.frame(predictions) %>%
      mutate(target = cDBHtarget) %>%
      mutate(variable = var)

    df.predictions[["value"]] <- vars


    df.model.var <- bind_rows(df.model.var,
                              df.predictions)
  }


  LM.all <- lm(data = effect.site,
               formula = (m_high) ~ get(Var1) + get(Var2),
               weights = w)

  Sum <- summary(LM.all)

  df.r2 <- bind_rows(df.r2,
                     data.frame(target = cDBHtarget,
                                r2 = Sum[["adj.r.squared"]],
                                intercept = Sum[["coefficients"]][1,"Estimate"],
                                slope1 = Sum[["coefficients"]][2,"Estimate"],
                                slope2 = Sum[["coefficients"]][3,"Estimate"],
                                pval1 = Sum[["coefficients"]][2,"Pr(>|t|)"],
                                pval2 = Sum[["coefficients"]][3,"Pr(>|t|)"],
                                N = nrow(effect.site)))

  vars1 <- seq(min(effect.site[[Var1]]),
              max(effect.site[[Var1]]),
              length.out=1000)
  varm2 <- weighted.mean(effect.site[[Var2]],
                         effect.site[["Nlarge"]])

  vars2 <- seq(min(effect.site[[Var2]]),
               max(effect.site[[Var2]]),
               length.out=1000)
  varm1 <- weighted.mean(effect.site[[Var1]],
                         effect.site[["Nlarge"]])


  newdata <- bind_rows(data.frame(A = vars1,
                                  B = varm2,
                                  constant = Var2),
                       data.frame(A = varm1,
                                  B = vars2,
                                  constant = Var1)
                       )
  colnames(newdata)[1:2] <- c(Var1,Var2)

  predictions <- predict(LM.all,newdata = newdata,
                         interval = "confidence")
  df.predictions <- as.data.frame(predictions) %>%
    mutate(target = cDBHtarget) %>%
    mutate(variable1 = newdata[[Var1]],
           variable2 = newdata[[Var2]],
           constant = newdata[["constant"]])

  df.model.all <- bind_rows(df.model.all,
                            df.predictions)


  plot(LM.all$fitted.values,residuals(LM.all))
  abline(h = 0)
  hist(residuals(LM.all))
  qqnorm(residuals(LM.all))
  qqline(residuals(LM.all))

  saveRDS(LM.all,paste0("./outputs/best.model.DeltaH.",cDBHtarget,".RDS"))

}

df.r2

rownames(df.model.var) <- rownames(df.model.all) <- NULL

df.model.all.long <- df.model.all %>%
  pivot_longer(cols = c(variable1,variable2),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = case_when(variable == "variable1" ~ Var1,
                              variable == "variable2" ~ Var2,
                              TRUE ~ "Error404"))

df.all.effects.long <- df.all.effects %>%
  pivot_longer(cols = c(MAP,Prec.sd,MCWD,
                        MAT,t.sd,
                        VPD,VPD.sd,
                        srad,srad.sd),
               names_to = "variable",
               values_to = "value")

p.values <- df.all.effects.long %>%
  group_by(variable,target) %>%
  summarise(p.value = summary(lm(formula = m_high ~ value,
                                     weights = w))[["coefficients"]][2,4],
            r2 = summary(lm(formula = m_high ~ value,
                            weights = w))[["r.squared"]],
            .groups = "keep")


ggplot(data = df.all.effects) +
  geom_boxplot(aes(y = m_high,
                   x = continent, fill = continent)) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  labs(x = "") +
  facet_wrap(~ target,nrow = 1) +
  theme_bw()


ggplot(data = df.all.effects) +
  geom_boxplot(aes(y = m_high, x = forest.type, fill = forest.type)) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  facet_wrap(~ target,nrow = 1) +
  theme_bw()

ggplot(data = df.all.effects %>%
         mutate(CZ = factor(CZ,
                            levels = c("Humid1700","Humid","Humid_seasonal","Else"))) %>%
         filter(target == DBHtargets[1])) +
  geom_boxplot(aes(y = m_high, x = CZ, fill = CZ),
               alpha = 0.8) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  # facet_wrap(~ target,nrow = 1) +
  theme_bw() +
  labs(x = "",y = "") +
  guides(fill = "none") +
  theme(text = element_text(size = 24)) +
  scale_fill_manual(values = c(Humid1700 = "#1C2C04",
                                Humid = "#345C0C",
                                Humid_seasonal = "#4C8C14",
                               Else = "lightyellow"))

summary(lm(data = df.all.effects %>%
         filter(target == DBHtargets[1]),
       formula = m_high ~ CZ))

df.all.effects %>%
  group_by(target,CZ) %>%
  summarise(N = n(),
            .groups = "keep")


cols<-brewer.pal(length(continents),"Dark2")


ctarget <- DBHtargets[2]
ggplot() +
  geom_errorbar(data = df.all.effects.long %>%
                  filter(target == ctarget),
                aes(x = value, y = m_high,color = continent,
                    ymin = Qlow_high, ymax = Qhigh_high)) +

  geom_point(data = df.all.effects.long %>%
               filter(target == ctarget),
             aes(x = value, y = m_high,color = continent,size = (Nlarge)),
             alpha = 0.7) +

  # stat_smooth(data = df.all.effects.long %>%
  #               filter(target == ctarget),
  #             aes(x = value, y = m_high),
  #             method = "lm", se = TRUE, fill = "lightblue") +

  geom_hline(yintercept = 0,linetype = 2, color = "black") +

  geom_ribbon(data = df.model.var %>%
                filter(target == ctarget),
              aes(x = value, y = fit, ymin = lwr, ymax = upr), color = NA, fill = "lightgrey", alpha = 0.5) +
  geom_line(data = df.model.var %>%
              filter(target == ctarget),
            aes(x = value, y = fit), color = "black") +
  labs(x = "",y = "", color = "") +
  scale_color_manual(values = cols) +
  theme_bw() + theme(legend.position = c(0.9,0.1)) +
  facet_wrap(~ variable, scales = "free") +
  # facet_wrap(~ target,nrow = 1) +
  guides(size = "none", fill = "none") +
  theme(text = element_text(size = 24))


ggplot() +
  geom_errorbar(data = df.all.effects.long %>%
                  filter(variable %in% c(Var1)),
                aes(x = value, y = m_high,color = continent,
                    ymin = Qlow_high, ymax = Qhigh_high)) +

  geom_point(data = df.all.effects.long %>%
               filter(variable %in% c(Var1)),
             aes(x = value, y = m_high,color = continent,size = (Nlarge)),
             alpha = 0.7) +

  geom_hline(yintercept = 0,linetype = 2, color = "black") +

  geom_ribbon(data = df.model.all.long %>%
                filter(constant != variable,
                       variable == Var1),
              aes(x = value, y = fit, ymin = lwr, ymax = upr), color = NA, fill = "lightgrey", alpha = 0.5) +
  geom_line(data = df.model.all.long %>%
              filter(constant != variable,
                     variable == Var1),
            aes(x = value, y = fit), color = "black") +
  labs(x = "",y = "", color = "") +
  scale_color_manual(values = cols) +
  theme_bw() + theme(legend.position = c(0.9,0.95)) +
  facet_wrap(variable  ~ target, ncol = 4) +
  # scale_x_continuous(limits = c(0,0.21)) +
  # facet_wrap(~ target,nrow = 1) +
  guides(size = "none", fill = "none", color = "none") +
  theme(text = element_text(size = 24),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.x =  unit(0.8,"cm"))



ggplot() +
  geom_errorbar(data = df.all.effects.long %>%
                  filter(variable %in% c(Var2)),
                aes(x = value, y = m_high,color = continent,
                    ymin = Qlow_high, ymax = Qhigh_high)) +

  geom_point(data = df.all.effects.long %>%
               filter(variable %in% c(Var2)),
             aes(x = value, y = m_high,color = continent,size = (Nlarge)),
             alpha = 0.7) +

  geom_hline(yintercept = 0,linetype = 2, color = "black") +

  geom_ribbon(data = df.model.all.long %>%
                filter(constant != variable,
                       variable == Var2),
              aes(x = value, y = fit, ymin = lwr, ymax = upr), color = NA, fill = "lightgrey", alpha = 0.5) +
  geom_line(data = df.model.all.long %>%
              filter(constant != variable,
                     variable == Var2),
            aes(x = value, y = fit), color = "black") +
  labs(x = "",y = "", color = "") +
  scale_color_manual(values = cols) +
  theme_bw() + theme(legend.position = c(0.9,0.95)) +
  facet_wrap(variable  ~ target, ncol = 4) +
  # scale_x_continuous(limits = c(0,0.21)) +
  # facet_wrap(~ target,nrow = 1) +
  guides(size = "none", fill = "none", color = "none") +
  theme(text = element_text(size = 24),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.x =  unit(0.8,"cm"))

################################################################################
# Reformat
df.all.effects.long2plot <-
  df.all.effects.long %>%
  mutate(variable = relevel(as.factor(variable),ref = Var1))

df.model.all.long2plot <-
  df.model.all.long %>%
  mutate(variable = relevel(as.factor(variable),ref = Var1))

Target <- DBHtargets[2]
ggplot() +
  geom_errorbar(data = df.all.effects.long2plot %>%
                  filter(target == Target,
                         variable %in% c(Var1,Var2)),
                aes(x = value, y = m_high,color = continent,
                    ymin = Qlow_high, ymax = Qhigh_high)) +

  geom_point(data = df.all.effects.long2plot %>%
               filter(target == Target,
                      variable %in% c(Var1,Var2)),
             aes(x = value, y = m_high,color = continent,size = (Nlarge)),
             alpha = 1) +

  geom_hline(yintercept = 0,linetype = 2, color = "black") +

  geom_ribbon(data = df.model.all.long2plot %>%
                filter(target == Target,
                       constant != variable),
              aes(x = value, y = fit, ymin = lwr, ymax = upr), color = NA, fill = "lightgrey", alpha = 0.5) +
  geom_line(data = df.model.all.long2plot %>%
              filter(target == Target,
                     constant != variable),
            aes(x = value, y = fit), color = "black") +
  labs(x = "",y = "", color = "") +
  scale_color_manual(values = cols) +
  theme_bw() + theme(legend.position = c(0.9,0.9)) +
  facet_wrap(~ variable, scales = "free_x", ncol = 4) +
  # facet_wrap(~ target,nrow = 1) +
  guides(size = "none", fill = "none", color = "none") +
  theme(text = element_text(size = 24),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing = unit(2,"cm"))



