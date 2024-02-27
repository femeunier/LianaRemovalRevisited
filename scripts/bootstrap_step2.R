rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)

load("~/Downloads/agb_all.rda")

df <- agb.all1000 %>%
  filter(veg.type == "tree") %>%
  filter(timing %in% seq(1,21,2)) %>%
  ungroup()

ggplot(data = df %>%
         filter(timing %in%
                  c(min(timing),max(timing)))) +
  geom_density(aes(x = AGBC,
                   fill = Treatment, group = Parcela), color = NA, alpha = 0.5) +
  facet_grid(timing ~ method) +
  theme_bw()

# df %>% group_by(Parcela) %>%
#   summarise(AGBC.low = quantile(AGBC,0.025,na.rm = TRUE),
#             AGBC.median = quantile(AGBC,0.5,na.rm = TRUE),
#             AGBC.high = quantile(AGBC,0.975,na.rm = TRUE))
#
# df %>%
#   group_by(Treatment) %>%
#   summarise(AGBC.low = quantile(AGBC,0.025,na.rm = TRUE),
#             AGBC.median = quantile(AGBC,0.5,na.rm = TRUE),
#             AGBC.high = quantile(AGBC,0.975,na.rm = TRUE))

Nbootstraps = 1000
df.sum <- data.frame()

plots <- list(C = sort(unique(df %>% filter(Treatment == "C") %>%
                                pull(Parcela))),
              R = sort(unique(df %>% filter(Treatment == "R") %>%
                                pull(Parcela))))

for (i in seq(1,Nbootstraps)){
  print(i/Nbootstraps)

  df.build <- df %>%
    group_by(Treatment,method,timing) %>%
    # filter(Parcela %in% sample(unique(Parcela),1)) %>%
    # filter(iterations %in% sample(1:1000,
    #                               size = 1,
    #                               replace = TRUE)) %>%
    slice_sample(n = 8)

  # ggplot(data = df.build) +
  #   geom_density(aes(x = AGBC, fill = Treatment),
  #                color = NA, alpha = 0.5) +
  #   theme_bw()

  df.sum <- bind_rows(df.sum,
                      df.build %>%
                        group_by(Treatment,method,timing) %>%
                        summarise(AGBC = mean(AGBC),
                                  .groups = "keep") %>%
                        group_by(Treatment,method) %>%
                        mutate(diff = c(NA,diff(AGBC))) %>%
                        mutate(iter = i))


}

ggplot(data = df.sum %>%
         filter(timing %in%
                  c(min(timing),max(timing)))) +
  geom_density(aes(x = AGBC,
                   fill = Treatment), color = NA, alpha = 0.5) +
  facet_grid(method ~ timing) +
  theme_bw()

stop()

ggplot(data = df.sum %>%
         dplyr::select(-AGBC) %>%
         filter(timing %in% c(3)) %>%
         pivot_wider(values_from = diff,
                     names_from = Treatment) %>%
         mutate(liana = (R-C)/R) %>%
         filter(is.finite(liana))) +
  geom_density(aes(x = liana), color = NA, alpha = 0.5) +
  facet_wrap( ~ method) +
  theme_bw()


df.bootstrapped <- df.sum %>%
  group_by(Treatment,method,timing) %>%
  summarise(AGBC.low = quantile(AGBC,0.025,na.rm = TRUE),
            AGBC.median = quantile(AGBC,0.5,na.rm = TRUE),
            AGBC.mean = mean(AGBC,na.rm = TRUE),
            AGBC.high = quantile(AGBC,0.975,na.rm = TRUE),
            .groups = "keep") %>%
  left_join(df %>%
              dplyr::select(Time,timing) %>%
              distinct(),
            by = "timing")

ggplot() +
  geom_ribbon(data = df.bootstrapped,
              aes(x = Time, y = AGBC.median,
                  ymin = AGBC.low, ymax = AGBC.high,
                  fill = interaction(Treatment,method)), alpha = 0.4,
              color = NA) +
  geom_line(data = df.bootstrapped,
            aes(x = Time, y = AGBC.median, #linetype = method,
                color = interaction(Treatment,method))) +
  # facet_wrap(~ method) +
  theme_bw()

ggplot() +
  geom_ribbon(data = df.bootstrapped %>%
                filter(method == "updated"),
              aes(x = Time, y = AGBC.median,
                  ymin = AGBC.low, ymax = AGBC.high,
                  fill = Treatment), alpha = 0.4,
              color = NA) +
  geom_line(data = df.bootstrapped,
            aes(x = Time, y = AGBC.median, color = Treatment, linetype = method)) +
  theme_bw()



df.effect <- df.sum %>%
  filter(timing %in% c(3,5),
         method == "original") %>%
  pivot_wider(values_from = AGBC,
              names_from = Treatment) %>%
  pivot_wider(names_from = timing,
              values_from = c(C,R)) %>%
  mutate(C = C_5 - C_3,
         R = R_5 - R_3) %>%
  mutate(liana.effect = R - C) %>%
  mutate(liana.effect.rel = (R - C)/R)

df.effect %>%
  ungroup() %>%
  dplyr::select(liana.effect,liana.effect.rel) %>%
  pivot_longer(cols = -c(),
               names_to = "variable",
               values_to = "value") %>%
  group_by(variable) %>%
  summarise(low = quantile(value,0.025,na.rm = TRUE),
            med = quantile(value,0.5,na.rm = TRUE),
            m = mean(value,na.rm = TRUE),
            high = quantile(value,0.975,na.rm = TRUE),
            .groups = "keep")
