rm(list = ls())

library(lidR)
# library(LidarProcessoR)
library(dplyr)
library(rgl)
library(LianaBCI)
library(ggplot2)


system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/df.chm.RDS",
                      "./outputs/"))
system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/df.SP.RDS",
                      "./outputs/"))

df.chm <- readRDS("./outputs/df.chm.RDS")
df.SP <- readRDS("./outputs/df.SP.RDS")

plots = unique(df.chm$plot)


dist.thrs <- c(5,10,20,30)
df.chm.mod.all <- data.frame()

for (ithr in seq(1,length(dist.thrs))){

  dist.thr <- dist.thrs[ithr]
  for (iplot in seq(1,length(plots))){

    cplot = plots[iplot]

    SP.center.2016 <- df.SP %>% filter(plot == cplot,
                                       year == 2016) %>% ungroup() %>%
      summarise(X = mean(X),
                Y = mean(Y))

    SP.center.2019 <- df.SP %>% filter(plot == cplot,
                                       year == 2019) %>% ungroup() %>%
      summarise(X = mean(X),
                Y = mean(Y))

    df.chm.mod <- df.chm %>% filter(plot == cplot) %>%
      dplyr::select(patch,X.patch,Y.patch,plot,year) %>%
      mutate(X.cm.2016 = SP.center.2016[["X"]],
             Y.cm.2016 = SP.center.2016[["Y"]],
             X.cm.2019 = SP.center.2019[["X"]],
             Y.cm.2019 = SP.center.2019[["Y"]]) %>%
      mutate(dist.2016 = sqrt((X.patch - X.cm.2016)**2 + (Y.patch - Y.cm.2016)**2),
             dist.2019 = sqrt((X.patch - X.cm.2019)**2 + (Y.patch - Y.cm.2019)**2)) %>%
      mutate(thr.2016 = (dist.2016 <= dist.thr),
             thr.2019 = (dist.2019 <= dist.thr)) %>%
      mutate(thr = thr.2016 & thr.2019)

    df.chm.mod.all <- bind_rows(list(df.chm.mod.all,
                                     df.chm.mod %>% mutate(threshold = dist.thr)))

  }
}


ggplot(data = df.chm.mod.all %>% filter(year == 2016)) +
  geom_tile(aes(x = X.patch, y = Y.patch,
      fill = thr)) +
  geom_point(data = df.SP  %>% filter(year == 2016),
             aes(x = X, y = Y), color = "red") +
  geom_point(data = df.SP  %>% filter(year == 2019),
             aes(x = X, y = Y), color = "blue") +
  facet_grid(threshold ~ plot) +
  coord_fixed()


df.chm.mod.all %>% group_by(plot,
                            threshold) %>%
  filter(thr,
         year == 2016) %>%
  summarise(N = length(thr)) %>%
  mutate(Req = sqrt(N/pi))
