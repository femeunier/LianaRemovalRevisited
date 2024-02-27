rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(DescTools)

load("~/Downloads/year_example.rda")
dim(plottot_ept_year)

y_year <- seq(1,357,7)

names <- c(paste0('AGBC',seq(1,11)),
           paste0('dAGBC',seq(1,10)),
           paste0('BGRC',seq(1,10)),
           paste0('RECRC',seq(1,10)),
           paste0('MORTC',seq(1,10)))

perc <- y_year + 6
temp <-(plottot_ept_year[,perc])
temp2 <- (as.matrix(temp[,grepl("BGRC",names)]))
class(temp2) <- "numeric"
df.perc <- as.data.frame(temp2)
colnames(df.perc) <- paste0(seq(1,10))
df.final <- cbind(df.perc,
                  plottot_ept_year[,358:359])

df.final.long <- df.final %>%
  pivot_longer(cols = - c(method.input,iter),
               names_to = "year",
               values_to = "perc") %>%
  mutate(year = as.numeric(year))

df.sum <- df.final.long %>%
  group_by(method.input, year) %>%
  summarise(perc.m = mean(perc,na.rm = TRUE),
            perc.low = quantile(perc,0.025, na.rm = TRUE),
            perc.high = quantile(perc,0.975, na.rm = TRUE),
            .groups = "keep")


ggplot(data = df.sum %>%
         filter(method.input %in% c("orig","h.pred"))) +
  geom_bar(aes(x = year, fill = method.input,
               y = perc.m),
           stat = "identity",
           position = position_dodge()) +
  theme_bw()

ggplot(data = df.final.long %>%
         filter(method.input %in% c("orig","h.pred")),
       aes(x = perc,
           y = factor(year,
                      levels = seq(10,1,-1)),
           fill = method.input)) +
  # geom_density_ridges(aes(x = perc,
  #                         y = as.factor(year),
  #                         fill = method.input),
  #                     alpha = 0.5) +
  stat_density_ridges(quantile_lines = TRUE,
                      quantiles = c(0.025, 0.975), alpha = 0.7,height = 0.5,
                      scale = 1) +
  theme_bw()


ggplot(data = df.final.long %>%
         filter(method.input %in% c("orig","h.pred")),
       aes(x = perc,
           y = 0,
           fill = method.input)) +
  stat_density_ridges(quantile_lines = TRUE,
                      quantiles = c(0.025, 0.975), alpha = 0.7,height = 0.5,
                      scale = 1) +
  theme_bw()

library(viridis)

ggplot(data = df.final.long %>%
         filter(method.input %in% c("orig","h.pred")),
       aes(x = perc,
           y = as.factor(year),
           group = method.input,
           fill = factor(stat(quantile))))  +

  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.025, 0.975)) +
  # scale_fill_manual(values = c(0.2, 0.5,0.2)) +
  scale_fill_viridis(discrete = TRUE
                     , name = "Quantile"
                     , alpha = 0.3
                     , option = "cividis") +
  theme_bw()








df <- df.final.long %>%
  na.omit()

df.final.long %>%
  group_by(year,method.input) %>%
  summarise(N = n())

df.final.long %>%
  filter(iter == "1",
         year == 1,
         method.input == "h.pred")

auc <- c()
for (cyear in 1:10){

  d1dens <- with(df, density(perc[method.input == "orig" &
                                  year == cyear],
                             from = min(perc),
                             to = max(perc)))
  d2dens <- with(df, density(perc[method.input == "h.pred" &
                                  year == cyear],
                             from = min(perc),
                             to = max(perc)))
  joint <- pmin(d1dens$y, d2dens$y)

  df2 <- data.frame(x = rep(d1dens$x, 3),
                    y = c(d1dens$y, d2dens$y, joint),
                    Data = rep(c("D1", "D2", "overlap"), each = length(d1dens$x)))

  ggplot(df2, aes(x, y, fill = Data)) +
    geom_area(position = position_identity(), color = "black") +
    scale_fill_brewer(palette = "Pastel2") +
    theme_bw()

  print(paste0(cyear," - ", AUC(d1dens$x,joint)))

  auc <- c(auc,AUC(d1dens$x,joint))

}

ggplot(data = data.frame(year = 1:10,
                         auc),
       aes(x = year, y = auc)) +
  geom_point() +
  stat_smooth(method = "lm") +
  scale_y_continuous(limits = c(0,0.5)) +
  geom_hline(yintercept = 0.05, linetype = 2) +
  theme_bw()
