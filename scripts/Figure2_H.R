rm(list = ls())

library(brms)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggdist)

# Data
alpha = 0.11

all.df <- readRDS("./outputs/All.COI.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10) %>%
  mutate(site = "Total")


all.df.title <- all.df %>%
  group_by(site) %>%
  mutate(site.N = paste0(site,", N = ", length(site)," (",length(site[which(liana.cat == "no")]), "-",
                         length(site[which(liana.cat == "low")]), "-",
                         length(site[which(liana.cat == "high")]), ")"),
         N.low = length(site[which(liana.cat == "low")]),
         N.high = length(site[which(liana.cat == "high")]),
         N.tot = length(site))

Model.predictions <- readRDS("./outputs/Model.predictions.RDS") %>%
  ungroup() %>%
  filter(site == "Total")


df.residuals <- all.df %>%
  mutate(site.group = "Total.re") %>%
  mutate(dbh = round(dbh)) %>%
  left_join(Model.predictions %>%
               dplyr::select(dbh,liana.cat,
                             h.pred.m,h.null.pred.m,site),
            by = c("dbh","liana.cat","site")) %>%
  mutate(res_null = (h.null.pred.m-h),
         res_best = (h.pred.m-h)) %>%
  mutate(delta.res = abs(res_best) - abs(res_null)) %>%
  # dplyr::select(liana.cat,res_null,res_best) %>%
  pivot_longer(cols = c(res_null,res_best)) %>%
  mutate(model = sub(".*\\_", "", name)) %>%
  mutate(liana.cat = case_when(model == "null" ~ "null",
                               TRUE ~ liana.cat))

ggplot(data = df.residuals,
       aes(y = (value), x = model, alpha = 0.5)) +
  # geom_violin(alpha = 0.5) +
  geom_boxplot(alpha = 0.5) +
  # ggdist::stat_halfeye() +
  # geom_boxplot(
  #   width = .15,
  #   outlier.shape = NA
  # ) +
  # geom_boxplot(position = position_dodge(),
  #              outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = 2) +
  # scale_fill_manual(values = c("no" = "darkgreen",
  #                              "low" = "orange",
  #                              "high"= "darkred")) +
  # facet_grid(~model) +
  # scale_y_continuous(limits = c(-1,1)) +
  # scale_fill_manual(values = c("best" = "darkblue",
  #                              "null" = "black")) +
  # facet_wrap(~site) +
  theme_bw()

df.residuals %>%
  group_by(model) %>%
  summarise(med = median((value),na.rm = TRUE),
            N = n(),
            RMSE = sqrt(1/(n())*(sum(value**2))),
            MSA = 100*exp(median(abs(value)) - 1))


ggplot(data = df.residuals %>%
         filter(model == "best")) +
  geom_density(aes(x = (delta.res)/h*100, fill = liana.cat),
             alpha = 0.5, color = NA) +
  # geom_vline(xintercept = 0, linetype = 2) +
  # scale_fill_manual(values = c("no" = "darkgreen",
  #                              "low" = "orange",
  #                              "high"= "darkred")) +
  # facet_grid(~model) +
  # scale_x_continuous(limits = c(-30,15)) +
  geom_vline(xintercept = 0,linetype = 2) +
  theme_bw()

ggplot(data = df.residuals) +
  geom_point(aes(x = dbh, y = value, color = model),
               alpha = 0.5) +
  # geom_vline(xintercept = 0, linetype = 2) +
  # scale_fill_manual(values = c("no" = "darkgreen",
  #                              "low" = "orange",
  #                              "high"= "darkred")) +
  # facet_grid(~model) +
  # scale_x_continuous(limits = c(-20,20)) +
  theme_bw()


# Feldspacuh 2011

D <- seq(10,250,length.out = 1000)
H <- exp(2.4478 + 0.532*log(D/10))

df.Feldspauch <- data.frame(dbh = D,
                            h = H)

ggplot() +
  geom_point(data = all.df.title,
             aes(x = dbh,y = h, color = as.factor(liana.cat)),
             size = 0.5, alpha = 0.5) +
  # geom_ribbon(aes(x = dbh, y = h.pred.m, fill = as.factor(liana.cat),
  #                 ymin = h.pred.low, ymax = h.pred.high), color = NA, alpha = 0.5) +
  geom_line(data = Model.predictions,
            aes(x = dbh,y = h.pred.m, color = as.factor(liana.cat))) +
  # geom_line(data = df.Feldspauch,
  #           aes(x = dbh, y = h),
  #           color = "black", linetype = 2) +

  # geom_ribbon(aes(x = dbh, y = h.null.pred.m,
  #                 ymin = h.null.pred.low, ymax = h.null.pred.high), color = NA, alpha = 0.5, fill = "darkgrey") +
  geom_line(data = Model.predictions,
            aes(x = dbh,y = h.null.pred.m), color = "black") +
  # facet_wrap(~ site.N, scales = "free") +
  # scale_x_log10(limits = c(10,250),
  #               breaks = c(10,20,50,100,200), expand = c(0,0)) +
  # scale_y_log10(limits = c(1,80)) +
  labs(x = "DBH (cm)", y = 'Tree height (m)', color = "Liana infestation", fill = "Liana infestation") +
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  theme_bw() +
  # scale_x_log10() +
  # scale_y_log10() +
  theme(text = element_text(size = 20),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  guides(color = "none")

Main.OP <- bind_rows(readRDS("./outputs/Main.OP.SG.25.RDS") %>%
                       mutate(target = 25),
                     readRDS("./outputs/Main.OP.SG.50.RDS") %>%
                       mutate(target = 50),
                     readRDS("./outputs/Main.OP.SG.100.RDS") %>%
                       mutate(target = 100),
                     readRDS("./outputs/Main.OP.SG.150.RDS") %>%
                       mutate(target = 150)) %>%
  mutate(site = "Total")

ggplot(data = Main.OP,
       aes(x = diff_h/no*100,
           y = 0,
           color = liana.cat,
           fill = liana.cat,
           alpha = 0.7)) +
  geom_vline(xintercept = 0,linetype = 1) +
  stat_halfeye(color = NA,
               alpha = 0.3) +
  stat_pointinterval(aes(alpha = signif_rel2),
                     .width = c(1-alpha),
                     position = position_dodge(width = 0)) +
  # scale_x_continuous(limits = c(-10,5)) +
  labs(y = "", color = "", fill = "",x = "") +
  theme_minimal() +
  facet_wrap(~ target, scales = "free_y",nrow = 1) +
  guides(alpha = "none", fill = "none", color = "none") +
  theme(legend.position = c(0.1,0.9),strip.text = element_blank(),
        text = element_text(size = 24),
        panel.spacing = unit(2, "lines")) +

  scale_y_continuous(breaks = c(0),limits = c(0,1),labels = c("")) +
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  scale_fill_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred"))


diff.H <- Main.OP %>%
  filter(target == 50) %>%
  group_by(site,liana.cat) %>%
  summarise(m = median(diff_h/no*100,na.rm = TRUE),
            low = quantile(diff_h/no*100,alpha/2,na.rm = TRUE),
            high = quantile(diff_h/no*100,1-alpha/2,na.rm = TRUE),

            m.abs = median(diff_h,na.rm = TRUE),
            low.abs = quantile(diff_h,alpha/2,na.rm = TRUE),
            high.abs = quantile(diff_h,1-alpha/2,na.rm = TRUE),
            .groups = "keep")


diff.H <- Main.OP %>%
  group_by(target,liana.cat) %>%
  summarise(m = median(diff_h/no*100,na.rm = TRUE),
            low = quantile(diff_h/no*100,alpha/2,na.rm = TRUE),
            high = quantile(diff_h/no*100,1-alpha/2,na.rm = TRUE),

            m.abs = median(diff_h,na.rm = TRUE),
            low.abs = quantile(diff_h,alpha/2,na.rm = TRUE),
            high.abs = quantile(diff_h,1-alpha/2,na.rm = TRUE),
            .groups = "keep")


diff.H %>% filter(liana.cat == "low")
diff.H %>% filter(liana.cat == "high")
