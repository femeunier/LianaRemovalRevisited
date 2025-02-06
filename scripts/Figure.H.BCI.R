rm(list = ls())

library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(ggdist)

selected.years <- c(2011,2015,2019,2023)
# Load the data
all.df <- readRDS("./outputs/BCI.COI.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10) %>%
  filter(year %in% selected.years)


# Fits

Model.Predictions.BCI <- readRDS(paste0("./outputs/Model.Predictions.BCI",".RDS")) %>%
  filter(year %in% selected.years)



ggplot() +
  geom_point(data = all.df %>%
               group_by(Tag) %>%
               slice_head(n = 1),
             aes(x = dbh,y = h, color = as.factor(liana.cat)),
             size = 0.1, alpha = 0.5) +
  geom_line(data = Model.Predictions.BCI,
            aes(x = dbh,y = h.pred.m,
                linetype = as.factor(year),
                color = as.factor(liana.cat))) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "", y = '', color = "Liana infestation", fill = "Liana infestation") +
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        strip.background = element_blank(),strip.text = element_blank()) +
  guides(color = "none")

ggplot() +
  geom_point(data = all.df,
             aes(x = dbh,y = h, color = as.factor(liana.cat)),
             size = 0.5, alpha = 0.5) +
  # geom_ribbon(aes(x = dbh, y = h.pred.m, fill = as.factor(liana.cat),
  #                 ymin = h.pred.low, ymax = h.pred.high), color = NA, alpha = 0.5) +
  geom_line(data = Model.Predictions.BCI,
            aes(x = dbh,y = h.pred.m, color = as.factor(liana.cat))) +

  # geom_ribbon(aes(x = dbh, y = h.null.pred.m,
  #                 ymin = h.null.pred.low, ymax = h.null.pred.high), color = NA, alpha = 0.5, fill = "darkgrey") +
  geom_line(data = Model.Predictions.BCI,
            aes(x = dbh,y = h.null.pred.m), color = "black") +
  facet_wrap(~ year) +
  scale_x_continuous(limits = c(10,250)) +
  # scale_x_log10(breaks = c(1,10,100),limits = c(1,100)) +
  # scale_y_log10(breaks = c(1,10,100),limits = c(1,100)) +
  # scale_x_continuous(limits = c(10,150)) +
  labs(x = "", y = '', color = "Liana infestation", fill = "Liana infestation") +
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        strip.background = element_blank(),strip.text = element_blank(),
        panel.spacing = unit(2, "lines")) +
  guides(color = "none")

Main.OP.BCI <- readRDS(paste0("./outputs/Main.OP.BCI50.RDS")) %>%
  mutate(signif_rel = case_when(signif_rel > 0.5 ~ 0.3,
                                TRUE ~ signif_rel)) %>%
  filter(year %in% selected.years)

alpha = 0.11

ggplot(data = Main.OP.BCI,
       aes(x = diff_h/no*100,
           y = "O",
           color = liana.cat,
           fill = liana.cat)) +
  geom_vline(xintercept = 0,linetype = 1) +
  stat_halfeye(aes(alpha = signif_rel),
               color = NA) +
  stat_pointinterval(aes(alpha = signif_rel2),
                     .width = c(1-alpha),
                     position = position_dodge(width = 0)) +
  # scale_x_continuous(limits = c(-15,5)) +
  labs(y = "", color = "", fill = "",x = "") +
  theme_minimal() +
  scale_y_discrete(breaks = "", expand = c(0,0)) +
  # scale_x_continuous(limits = c(-30,20)) +
  facet_wrap(~ year, scales = "free_y") +
  guides(alpha = "none", fill = "none", color = "none") +
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  scale_fill_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred")) +
  theme(legend.position = c(0.1,0.9),
        strip.text = element_blank(),
        text = element_text(size = 24))



df.residuals <- all.df %>%
  mutate(dbh = round(dbh)) %>%
  left_join(Model.Predictions.BCI %>%
              dplyr::select(dbh,liana.cat,
                            h.pred.m,h.null.pred.m,year),
            by = c("dbh","liana.cat","year")) %>%
  mutate(res_null = (h.null.pred.m-h),
         res_best = (h.pred.m-h)) %>%
  mutate(delta.res = abs(res_best) - abs(res_null)) %>%
  # dplyr::select(liana.cat,res_null,res_best) %>%
  pivot_longer(cols = c(res_null,res_best)) %>%
  mutate(model = sub(".*\\_", "", name)) %>%
  mutate(liana.cat = case_when(model == "null" ~ "null",
                               TRUE ~ liana.cat))

df.residuals %>%
  group_by(year,name) %>%
  summarise(med = median(value),
            RSE = sqrt(1/(length(value - 3))*sum((value)**2)),
            RSE2 = sqrt(1/(length(value - 8))*sum((value)**2)),
            m =  mean(value),
            m.abs = mean(abs(value)))

all.df %>%
  group_by(year) %>%
  summarise(N = n())

Main.OP.BCI %>%
  group_by(year,liana.cat) %>%
  summarise(m = 100*median(diff_h/no,na.rm = TRUE),
            m.low = 100*quantile(diff_h/no,alpha/2,na.rm = TRUE),
            m.high = 100*quantile(diff_h/no,1-alpha/2,na.rm = TRUE),

            m.abs = median(diff_h,na.rm = TRUE),
            m.abs.low = quantile(diff_h,alpha/2,na.rm = TRUE),
            m.abs.high = quantile(diff_h,1-alpha/2,na.rm = TRUE))
