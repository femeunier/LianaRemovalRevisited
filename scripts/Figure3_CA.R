rm(list = ls())

library(ggthemes)
library(ggdist)

alpha = 0.11

all.df <- readRDS("./outputs/All.CA.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10)

nrow(all.df)
all.df %>%
  group_by(liana.cat) %>%
  summarise(n())

Main.OP <- bind_rows(readRDS("./outputs/Model.predictions.CA.50.RDS") %>%
                               mutate(target = 50),
                     readRDS("./outputs/Model.predictions.CA.100.RDS") %>%
                               mutate(target = 100),
                     readRDS("./outputs/Model.predictions.CA.150.RDS") %>%
                               mutate(target = 150)) %>%
  mutate(site = factor(site,
                       level = c("Total",
                                 "BCI",
                                 "Loundoungou")))


ggplot(data = Main.OP %>%
         filter(site == "Total"),
       aes(x = diff_CA/no*100,
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
  scale_x_continuous(limits = c(-40,40)) +
  facet_wrap(~ target, scales = "free_y") +
  guides(alpha = "none", fill = "none", color = "none") +
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  scale_fill_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred")) +
  theme(legend.position = c(0.1,0.9),strip.text = element_blank(),
        text = element_text(size = 24),panel.spacing = unit(2, "lines"))


Main.OP %>%
  filter(site == 'Total') %>%
  group_by(target,
           liana.cat) %>%
  summarise(m = median(diff_CA/no*100,na.rm = TRUE),
            low = quantile(diff_CA/no*100,alpha/2,na.rm = TRUE),
            high = quantile(diff_CA/no*100,1-alpha/2,na.rm = TRUE),

            CA.m = median(no,na.rm = TRUE),

            m.abs = median(diff_CA,na.rm = TRUE),
            low.abs = quantile(diff_CA,alpha/2,na.rm = TRUE),
            high.abs = quantile(diff_CA,1-alpha/2,na.rm = TRUE),
            .groups = "keep")

Model.predictions.CA <- readRDS("./outputs/Model.predictions.CA.RDS")

ggplot() +

  geom_point(data = all.df,
              aes(x = dbh, y = area,
                  color = as.factor(liana.cat),
                  fill = as.factor(liana.cat)),
             size = 0.5, alpha = 0.5) +

  geom_line(data = Model.predictions.CA %>%
              filter(site.group == 'Total'),
            aes(x = dbh,y = CA.pred.m, color = liana.cat)) +

  geom_line(data = Model.predictions.CA %>%
              filter(site.group == 'Total'),
            aes(x = dbh,y = CA.null.pred.m, color = liana.cat), color = "black") +

  scale_x_log10() +
  scale_y_log10() +
  theme_bw() +
  labs(x = "",y = "") +
  guides(alpha = "none", fill = "none", color = "none") +
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  scale_fill_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred")) +
  theme(legend.position = c(0.1,0.9),strip.text = element_blank(),
        text = element_text(size = 24))



ggplot(data = Main.OP %>%
         filter(site != "Total",
                target == 50),
       aes(x = diff_CA/no*100,
           y = site,
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
  theme_bw() +
  scale_y_discrete(expand = c(0,0),position = "right") +
  scale_x_continuous(limits = c(-30,20)) +
  guides(alpha = "none", fill = "none", color = "none") +
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  scale_fill_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred")) +
  theme(legend.position = c(0.1,0.9),strip.text = element_blank(),
        text = element_text(size = 24))

Main.OP %>%
  filter( target == 50,
          site != "Total") %>%
  group_by(site,liana.cat) %>%
  summarise(m = median(diff_CA/no*100,na.rm = TRUE),
            low = quantile(diff_CA/no*100,alpha/2,na.rm = TRUE),
            high = quantile(diff_CA/no*100,1-alpha/2,na.rm = TRUE),

            CA.m = median(no,na.rm = TRUE),

            m.abs = median(diff_CA,na.rm = TRUE),
            low.abs = quantile(diff_CA,alpha/2,na.rm = TRUE),
            high.abs = quantile(diff_CA,1-alpha/2,na.rm = TRUE))



Main.OP %>%
  filter(site == "Total") %>%
  group_by(target,liana.cat) %>%
  summarise(m = median(diff_CA/no*100,na.rm = TRUE),
            low = quantile(diff_CA/no*100,alpha/2,na.rm = TRUE),
            high = quantile(diff_CA/no*100,1-alpha/2,na.rm = TRUE),

            # CA.m = median(no,na.rm = TRUE),

            m.abs = median(diff_CA,na.rm = TRUE),
            low.abs = quantile(diff_CA,alpha/2,na.rm = TRUE),
            high.abs = quantile(diff_CA,1-alpha/2,na.rm = TRUE))


