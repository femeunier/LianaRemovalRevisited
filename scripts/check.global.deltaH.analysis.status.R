rm(list = ls())

library(brms)
library(gridExtra)
library(abind)
library(reshape2)
library(ggridges)
library(stringr)
library(scales)
library(tidyr)
library(ggforce)
library(dplyr)
library(LianaRemovalRevisited)
library(cowplot)
library(ggdist)
library(ggplot2)

# Load the data
all.df <- bind_rows(readRDS("./outputs/COI.mixed.RDS") %>%
                      mutate(sp = str_squish(sp)) %>%
                      filter(dbh >= 10))

all.df %>%
  group_by(liana.cat) %>%
  summarise(N = n())

all.df.title <- all.df %>%
  group_by(site) %>%
  mutate(site.N = paste0(site,", N = ", length(site)," (",length(site[which(liana.cat == "no")]), "-",
                         length(site[which(liana.cat == "low")]), "-",
                         length(site[which(liana.cat == "high")]), ")"),
         N.low = length(site[which(liana.cat == "low")]),
         N.high = length(site[which(liana.cat == "high")]),
         N.tot = length(site))

sites <- unique(all.df.title$site)

transfer.files(c("Model.predictions.comp.RDS",
                 paste0("Main.OP.",
                        c(25,50,100,150),
                        ".comp.RDS")),
               base = "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/",
               source = "outputs",
               destination = file.path("./outputs/"),
               show.progress = TRUE)

temp.title <- readRDS("./outputs/Model.predictions.comp.RDS") %>%
  mutate(group = substr(site,1,3))
DBH2test <- 50
temp3.title <- readRDS(paste0("./outputs/Main.OP.",DBH2test,".comp.RDS")) %>%
  mutate(group = substr(site,1,3))
alpha <- 0.11

ggplot(data = temp.title) +
  geom_point(data = all.df.title %>%
               filter(site %in% sites),
             aes(x = dbh,y = h, color = as.factor(liana.cat)),
             size = 0.5, alpha = 0.25) +
  # geom_ribbon(aes(x = dbh, y = h.pred.m, fill = as.factor(liana.cat),
  #                 ymin = h.pred.low, ymax = h.pred.high), color = NA, alpha = 0.5) +
  geom_line(aes(x = dbh,y = h.pred.m, color = as.factor(liana.cat))) +

  # geom_ribbon(aes(x = dbh, y = h.null.pred.m,
  #                 ymin = h.null.pred.low, ymax = h.null.pred.high), color = NA, alpha = 0.5, fill = "darkgrey") +
  geom_line(aes(x = dbh,y = h.null.pred.m), color = "black") +
  facet_wrap(~ site.N, scales = "free") +
  scale_x_log10(limits = c(10,300),
                breaks = c(10,20,50,100,200)) +
  scale_y_log10(limits = c(1,60)) +
  labs(x = "DBH (cm)", y = 'Height (m)', color = "Liana infestation", fill = "Liana infestation") +
  theme_bw() +
  theme(text = element_text(size = 20))


################################################################################

mean.cat <- temp3.title %>%
  group_by(liana.cat) %>%
  summarise(diff_h_m = mean(diff_h,na.rm = TRUE),
            diff_h_m_rel = 100*mean(diff_h/no,na.rm = TRUE),
            .groups = "keep")


temp3.title %>%
  group_by(liana.cat) %>%
  summarise(m = median(diff_h/no*100,na.rm = TRUE))

ggplot(data = temp3.title %>%
         filter(site %in% sites),
       aes(x = diff_h/no*100,
           y = site.tot,
           color = liana.cat,
           fill = liana.cat,
           alpha = signif_rel)) +
  geom_vline(xintercept = 0,linetype = 1) +
  stat_halfeye(color = NA) +

  stat_pointinterval(aes(alpha = signif_rel2),
                     .width = c(1-alpha),
                     position = position_dodge(width = 0)) +
  facet_wrap(~ liana.cat) +
  scale_x_continuous(limits = c(-45,25)) +
  labs(y = "", color = "", fill = "") +
  theme_bw() +
  facet_wrap(~ group, scales = "free_y") +
  guides(alpha = "none") +
  theme(legend.position = c(0.1,0.2))
