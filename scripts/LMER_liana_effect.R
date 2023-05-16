rm(list = ls())

library(tidyverse)
library(brms)
library(dplyr)
library(ggplot2)
library(BayesianTools)
library(bayesplot)
library(bayestestR)
library(tidyr)
library(ggpubr)
library(minpack.lm)
library(tidybayes)
library(lme4)
library(lmerTest)
library(cowplot)
library(coefplot2)

####################################################################################################################################
# Panama

census.data.new <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/c97to18.csv",stringsAsFactors = FALSE)
census.data.new.liana <-
  census.data.new %>%
  rename(dbh = dbhtot18,
         h  = hght18) %>%
  mutate(liana.cat = case_when(liana18 %in% c(0) ~ "no",
                               liana18 %in% c(1,2) ~ "low",
                               TRUE ~ "high")) %>%
  filter(dbh >= 100,
         !is.na(h),
         !is.na(dbh),
         !is.na(liana18)) %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high")))

ggplot(data = census.data.new.liana %>%
         filter(dbh > 100),
       aes(x = dbh/10, y = h, color = as.factor(liana.cat))) +
  geom_point(alpha = 0.5, size = 0.25) +
  stat_smooth(se = FALSE,method = "lm") +
  scale_x_log10() +
  scale_y_log10(limits = c(3,50)) +
  labs(x = "DBH (cm)", y = "Height (m)", color = "Liana infestation") +
  theme_bw() +
  theme(legend.position = c(0.14,0.85),
        text = element_text(size = 20))

summary(lm(data = census.data.new.liana,
           formula = log(h) ~ log(dbh/10)))

summary(lm(data = census.data.new.liana,
           formula = log(h) ~ log(dbh/10) + as.factor(liana.cat)))


sp2keep <- census.data.new.liana %>%
  group_by(sp18, liana.cat) %>%
  summarise(n =n(),
            .groups = "keep") %>%
  pivot_wider(names_from = liana.cat,
              values_from = n) %>%
  mutate(ntot = sum(c(low,no,high))) %>%
  arrange(desc(ntot)) %>%
  ungroup() %>%
  filter(ntot >= 100 & low > 5 & no > 5 & high > 5) %>%
  slice_head(n = 15)

abundant.sp <- census.data.new.liana %>%
  group_by(sp18, liana.cat) %>%
  summarise(n =n(),
            .groups = "keep") %>%
  pivot_wider(names_from = liana.cat,
              values_from = n) %>%
  mutate(ntot = sum(c(low,no,high))) %>%
  arrange(desc(ntot)) %>%
  # filter(high > 5 & low > 5) %>%
  ungroup() %>%
  slice_head(n = 50)


mix.int.liana <- lmer(log(h) ~ log(dbh/10) + liana.cat + (1 + liana.cat | sp18),
                data = census.data.new.liana)
mix.int.null <- lmer(log(h) ~ log(dbh/10) + (1 | sp18),
                      data = census.data.new.liana)

AIC(mix.int.liana,mix.int.null)
anova(mix.int.liana,mix.int.null)
bbmle::AICtab(mix.int.liana,mix.int.null)

summary(mix.int.liana)
coefplot2(mix.int.liana)
# confint(mix.int.liana)

census.data.new.liana$fit <- exp(predict(mix.int.liana))

plot((census.data.new.liana$h),
     (census.data.new.liana$fit))
abline(a = 0, b = 1, col = "red",lwd = 2)
hist(census.data.new.liana$h-(census.data.new.liana$fit))

plot((census.data.new.liana$h),
     (census.data.new.liana$h)-(census.data.new.liana$fit))
abline(a = 0, b = 0, col = "red",lwd = 2)


randoms<-ranef(mix.int.liana, condVar = TRUE)
qq <- attr(ranef(mix.int.liana, condVar = TRUE)[[1]], "postVar")
rand.interc<-randoms$sp18

df<-data.frame(Intercepts=randoms$sp18[,3],
               sd.interc=2*sqrt(qq[3,3,1:(dim(qq)[3])]),
               lev.names=rownames(rand.interc)) %>%
  filter(lev.names %in% abundant.sp[["sp18"]]) %>%
  left_join(abundant.sp %>%
              dplyr::select(sp18,ntot) %>%
              rename(lev.names = sp18),
            by = "lev.names") %>%
  arrange(desc(ntot)) %>%
  mutate(signif = case_when(Intercepts + sd.interc < 0 ~ "negative",
                            Intercepts - sd.interc < 0 ~ "positive",
                            TRUE ~ "unsignificant"))


df$lev.names<-factor(df$lev.names,levels=df$lev.names[order(df$ntot,
                                                            decreasing = FALSE)])
ggplot(df ,
       aes(lev.names,Intercepts, color = signif)) +
  geom_hline(yintercept=0) +
  geom_errorbar(aes(ymin=Intercepts-sd.interc,
                    ymax=Intercepts+sd.interc),width = 0) +
  geom_point(aes(size=2)) +
  guides(size=FALSE,shape=FALSE) +
  # scale_shape_manual(values=c(1,1,1,16,16,16)) +
  theme_bw() +
  scale_color_manual(values = c(scales::muted("darkred"),
                                "black",
                                scales::muted("darkblue"))) +
  xlab("") + ylab("Slope") +
  theme(axis.text.x=element_text(size=rel(1.2)),
        axis.title.x=element_text(size=rel(1.3)),
               axis.text.y=element_text(size=rel(1.2)),
               panel.grid.minor=element_blank(),
               panel.grid.major.x=element_blank()) +
  guides(color = "none") +
  coord_flip()



census.data.new.liana <- census.data.new.liana %>%
  mutate(new.species = case_when(sp18 %in% sp2keep[["sp18"]] ~ sp18,
                                 TRUE ~ "Other"))

ggplot(census.data.new.liana %>% filter(new.species != "Other"),
       aes(x = dbh/10, y = h, group=interaction(liana.cat),
           color = liana.cat)) +
  facet_wrap(~ new.species,
             nrow = 3) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y=fit), size=0.8) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw()


####################################################################################################################################
# Congo(s)

census.data.new <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/Congo/database.csv",
                            stringsAsFactors = FALSE)

census.data.new %>%
  filter(DBH_cm >= 10) %>%
  group_by(Site) %>%
  summarise(n())

# We only keep the most abundant site
census.data.new.liana <-
  census.data.new %>%
  # dplyr::filter(Site == "Sand-F") %>%
  rename(dbh = DBH_cm,
         h  = Ht_m,
         sp = Species) %>%
  mutate(liana.cat = case_when(Liana %in% c(0) ~ "no",
                               Liana %in% c(1,2) ~ "low",
                               TRUE ~ "high")) %>%
  filter(!is.na(h),
         !is.na(dbh),
         !is.na(liana.cat)) %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high")))

ggplot(data = census.data.new.liana %>%
         filter(dbh > 10),
       aes(x = dbh, y = h, color = as.factor(liana.cat))) +
  geom_point(alpha = 0.5, size = 0.25) +
  stat_smooth(se = FALSE,method = "lm") +
  scale_x_log10() +
  scale_y_log10(limits = c(3,50)) +
  labs(x = "DBH (cm)", y = "Height (m)", color = "Liana infestation") +
  facet_wrap(~ Site) +
  theme_bw() +
  theme(legend.position = c(0.1,0.15),
        text = element_text(size = 20))

sites <- unique(census.data.new.liana$Site)
df.sum <- data.frame()

for (isite in seq(1,length(sites))){

  csite <- sites[isite]
  cdata <- census.data.new.liana %>%
    filter(Site == csite)

  a <- summary(lm(data = cdata,
             formula = log(h) ~ log(dbh)))

  b <- summary(lm(data = cdata,
                  formula = log(h) ~ log(dbh) + as.factor(liana.cat)))

  df.sum <- bind_rows(list(
    df.sum,
    data.frame(r2.null = a[["r.squared"]],
               r2.mod = b[["r.squared"]],
               adj.r2.null = a[["adj.r.squared"]],
               adj.r2.mod = b[["adj.r.squared"]],
               site = csite)
  ))

}


sp2keep <- census.data.new.liana %>%
  group_by(Site,sp, liana.cat) %>%
  summarise(n =n(),
            .groups = "keep") %>%
  pivot_wider(names_from = liana.cat,
              values_from = n,values_fill = 0) %>%
  mutate(ntot = sum(c(low,no,high)),na.rm = TRUE) %>%
  group_by(Site) %>%
  arrange(Site,desc(ntot)) %>%
  filter(ntot >= 25 & no >= 5 & high >= 5)

plot.ls <- list()

for (isite in seq(1,length(sites))){
  csite <- sites[isite]

  mix.int.liana <- lmer(log(h) ~ log(dbh) + liana.cat + (1 + liana.cat | sp),
                        data = census.data.new.liana %>% filter(Site == csite))
  mix.int.null <- lmer(log(h) ~ log(dbh) + (1 | sp),
                       data = census.data.new.liana %>% filter(Site == csite))

  print(AIC(mix.int.liana,mix.int.null))
  summary(mix.int.liana)
  coefplot2(mix.int.liana)
  # anova(mix.int.liana,mix.int.null)
  # bbmle::AICtab(mix.int.liana,mix.int.null)

  census.data.new.liana$fit[census.data.new.liana$Site == csite] <- exp(predict(mix.int.liana))
  census.data.new.liana$fit.null[census.data.new.liana$Site == csite] <- exp(predict(mix.int.null))

  randoms<-ranef(mix.int.liana, condVar = TRUE)
  qq <- attr(ranef(mix.int.liana, condVar = TRUE)[[1]], "postVar")
  rand.interc<-randoms$sp


  df<-data.frame(Intercepts=randoms$sp[,1],
                 sd.interc=2*sqrt(qq[3,3,1:(dim(qq)[3])]),
                 lev.names=rownames(rand.interc))

  df$lev.names<-factor(df$lev.names,levels=df$lev.names[order(df$Intercepts)])
  plot.ls[[isite]] <- ggplot(df %>%
           filter(lev.names %in% sp2keep[["sp"]]),
         aes(lev.names,Intercepts)) +
    geom_hline(yintercept=0) +
    geom_errorbar(aes(ymin=Intercepts-sd.interc,
                      ymax=Intercepts+sd.interc),
                  width=0,color="black") +
    geom_point(aes(size=2)) +
    guides(size=FALSE,shape=FALSE) +
    # scale_shape_manual(values=c(1,1,1,16,16,16)) +
    theme_bw() +
    xlab("Levels") + ylab("") +
    theme(axis.text.x=element_text(size=rel(1.2)),
          axis.title.x=element_text(size=rel(1.3)),
          axis.text.y=element_text(size=rel(1.2)),
          panel.grid.minor=element_blank(),
          panel.grid.major.x=element_blank()) +
    coord_flip()


}


plot_grid(plot.ls[[1]],
          plot.ls[[2]],nrow = 1)

plot((census.data.new.liana$h),
     (census.data.new.liana$fit))
lines(census.data.new.liana$h,
      (census.data.new.liana$fit.null), col = "red", type = "p")
abline(a = 0, b = 1, col = "red",lwd = 2)

hist(census.data.new.liana$h-(census.data.new.liana$fit))
hist(census.data.new.liana$h-(census.data.new.liana$fit.null))

plot(census.data.new.liana$h,
     (census.data.new.liana$h) - (census.data.new.liana$fit.null), col = "red", type = "p")
lines((census.data.new.liana$h),
     (census.data.new.liana$h)-(census.data.new.liana$fit), type = "p")

abline(a = 0, b = 0, col = "red",lwd = 2)




census.data.new.liana <- census.data.new.liana %>%
  mutate(new.species = case_when(sp %in% sp2keep[["sp"]] ~ sp,
                                 TRUE ~ "Other"))

ggplot(census.data.new.liana %>% filter(new.species != "Other"),
       aes(x = dbh, y = h, group=interaction(liana.cat),
           color = liana.cat)) +
  facet_grid(Site ~ new.species) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y=fit), size=0.8) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw()
