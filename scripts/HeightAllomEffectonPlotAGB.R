rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(LVLRE.long)


# href       b1Ht       b2Ht          a          b          c
# 5.896e+02  9.522e-03  3.586e-01  1.207e+02 -8.872e-04  4.093e-03

newTrees <- Tree.data %>% filter(DBH >= 100) %>%
  mutate(H = 37.9832*(1 -exp(-0.1098*((DBH/10)**0.5761)))) %>%
  mutate(AGBC = 0.0673*(WD*DBH/10*DBH/10*H)**0.976*CC/100) %>%
  mutate(H2 = case_when(Treatment == "C" ~  5.896e+02*(1 -exp(-9.522e-03*((DBH/10)**3.586e-01))),
                        Treatment == "R" ~ (5.896e+02 + 1.207e+02 )*(1 -exp(-(9.522e-03 -8.872e-04)*((DBH/10)**(3.586e-01 + 4.093e-03)))))) %>%
  # mutate(H2 = case_when(Treatment == "C" ~  39.34987*(1 -exp(-0.11052*((DBH/10)**0.54309))),
  #                       Treatment == "R" ~ (39.34987 +439.21558)*(1 -exp(-(0.11052 -0.09733)*((DBH/10)**(0.54309 + -0.18523)))))) %>%
  mutate(AGBC2 = 0.0673*(WD*DBH/10*DBH/10*H2)**0.976*CC/100) %>%
  mutate(diff_AGBC = 100*(AGBC2 - AGBC)/AGBC,
         diff_H = 100*(H2 - H)/H)

ggplot(data = newTrees %>% arrange(DBH)) +
  geom_line(aes(x = DBH/10, y = H)) +
  geom_line(aes(x = DBH/10, y = H2, color = Treatment)) +
  theme_bw()

df.diff <- newTrees %>% dplyr::select(Treatment,timing,diff_AGBC,diff_H) %>%
  pivot_longer(cols = c(diff_AGBC,diff_H),
               names_to = "type",
               values_to = "diff")

ggplot(data = df.diff) +
  geom_density(aes(x = diff, fill = Treatment), color = NA, alpha = 0.5) +
  facet_wrap(~ type, scales = "free") +
  geom_vline(xintercept = 0,linetype = 2, color = "black") +
  theme_bw()

df.diff %>% group_by(Treatment,type) %>%
  summarise(diff.m = mean(diff))


times <- newTrees %>% filter(ID == 1) %>% dplyr::select(timing,Time2)


cAGB <- newTrees %>% group_by(Treatment,plot) %>%
  filter(timing == 1) %>%
  mutate(plot.rel = round((plot + 1/2)/2)) %>%
  arrange(DBH) %>%
  mutate(IDsort = 1:length(AGBC),
         cumAGBC = cumsum(AGBC),
         cumAGBC2 = cumsum(AGBC2)) %>%
  mutate(diffcumAGBC = cumAGBC2 - cumAGBC)

ggplot(data = cAGB) +
  geom_hline(yintercept = 0) +
  # geom_line(aes(x = IDsort, y = cumAGBC, color = Treatment, group = plot)) +
  # geom_line(aes(x = IDsort, y = cumAGBC2, color = Treatment, group = plot),linetype = 2) +
  geom_line(aes(x = DBH/10,y = diffcumAGBC,color = Treatment, group = plot)) +

  # facet_grid(Treatment ~ plot.rel) +
  # scale_x_log10() +
  # scale_y_log10() +
  theme_bw()

data.plot <- bind_rows(list(newTrees,
                            Liana.data %>% mutate(DBH >= 50) %>% mutate(AGBC2 = AGBC))) %>%
  group_by(Treatment,plot,timing) %>%
  summarise(AGB.tot = sum(AGBC, na.rm = TRUE)/(60*60),
            AGB.tot2 = sum(AGBC2, na.rm = TRUE)/(60*60),
            .groups = "keep") %>%
  group_by(Treatment,timing) %>% filter(timing %in% seq(1,21,2)) %>%
  left_join(times,
            by = "timing")

data.plot.diff <- data.plot %>%
  group_by(Treatment,plot) %>%
  summarise(dagb = diff(AGB.tot),
            dagb2 = diff(AGB.tot2),
            year = seq(1,(length(AGB.tot2) - 1)),
            dt = diff(Time2),
            .groups = "keep") %>%
  mutate(dagb.dt = dagb/dt,
         dagb.dt2 = dagb2/dt)

ggplot(data = data.plot) +
  geom_line(aes(x = Time2, y = AGB.tot2 - AGB.tot, color = Treatment, group = as.factor(plot))) +
  # geom_line(aes(x = Time2, y = AGB.tot2, color = Treatment, group = as.factor(plot)), linetype = 2) +
  theme_bw()

data.treatment <- data.plot %>%
  group_by(Treatment,Time2) %>%
  summarise(AGBm_orig = mean(AGB.tot),
            AGBm_mod = mean(AGB.tot2),
            AGBmed_orig = median(AGB.tot),
            AGBmed_mod = median(AGB.tot2),
            AGBsd_orig = sd(AGB.tot),
            AGBsd_mod = sd(AGB.tot2),
            .groups = "keep") %>%
  pivot_longer(cols = -c(Treatment,Time2),
               names_to = "var",
               values_to = "value") %>%
  mutate(allom = sub(".*\\_", "", var),
         type = sub("AGB","",sub("\\_.*", "", var))) %>%
  dplyr::select(-var) %>%
  pivot_wider(values_from = value,
              names_from = type) %>%
  group_by(Treatment, allom) %>%
  mutate(m.rel = m - m[1],
         med.rel = med - med[1])

ggplot(data = data.treatment,
       aes(x = Time2, y = med, color = Treatment,fill = Treatment,
       ymin = med - sd, ymax = med + sd)) +
  geom_ribbon(color = NA, alpha = 0.5) +
  geom_line() +
  facet_wrap(~allom) +
  theme_bw()

ggplot(data = data.treatment,
       aes(x = Time2, y = med, color = Treatment,fill = Treatment,
           ymin = med - sd, ymax = med + sd,
           linetype = allom)) +
  geom_line() +
  theme_bw()

data.diff.treatment <- data.treatment %>% pivot_wider(values_from = c(m,med,sd,m.rel,med.rel),
                                                      names_from = Treatment) %>%
  mutate(diff.m = ((m_R) - (m_C)),
         diff.med = (med_R) - (med_C)) %>%
  group_by(allom) %>%
  mutate(diff.m.rel = diff.m - diff.m[1],
         diff.med.rel = diff.med - diff.med[1])


ggplot(data = data.diff.treatment) +
  geom_line(aes(x = Time2, y = diff.med, color = as.factor(allom))) +
  theme_bw()


ggplot(data = data.plot) +
  geom_line(aes(x = Time2, y = AGB.tot, color = Treatment, group = as.factor(plot))) +
  geom_line(aes(x = Time2, y = AGB.tot2, color = Treatment, group = as.factor(plot)), linetype = 2) +
  theme_bw()

ggplot(data = data.plot.diff) +
  geom_line(aes(x = year, y = dagb.dt, color = Treatment, group = as.factor(plot))) +
  geom_line(aes(x = year, y = dagb.dt2, color = Treatment, group = as.factor(plot)),linetype = 2) +
  theme_bw()


data.plot.diff.m <- data.plot.diff %>%
  group_by(Treatment,year) %>%
  summarise(m = median(dagb/dt)*10,
            m2 = median(dagb2/dt)*10,
            .groups = "keep")

ggplot(data = data.plot.diff.m,
       aes(x = m, y = m2, color = Treatment, fill = Treatment)) +
  geom_point() +
  stat_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0, color = "black",linetype = 2) +
  theme_bw()

ggplot(data = data.plot.diff.m) +
  geom_line(aes(x = year, y = m, color = Treatment)) +
  stat_smooth(aes(x = year, y = m, color = Treatment, fill = Treatment),method = "lm", se = FALSE) +
  geom_line(aes(x = year, y = m2, color = Treatment),linetype = 2) +
  stat_smooth(aes(x = year, y = m2, color = Treatment, fill = Treatment),method = "lm", se = FALSE,linetype = 2) +
  geom_hline(yintercept = 0, color = "black",linetype = 2) +
  theme_bw()

data.reduc <- data.plot.diff.m  %>% pivot_wider(names_from = Treatment,
                                                values_from = c(m,m2)) %>%
  mutate(reduc = (m_R - m_C)/m_R,
         reduc2 = (m2_R - m2_C)/m2_R)

data.reduc2plot <- data.reduc %>%
  dplyr::select(year,reduc,reduc2) %>%
  pivot_longer(cols = c(reduc,reduc2),
               values_to = "value",
               names_to = "type")

ggplot(data = data.reduc2plot) +
  geom_bar(aes(x = as.factor(year), y = value, fill = type),stat = "identity", position=position_dodge()) +
  theme_bw()

data.reduc2plot %>% group_by(type) %>%
  summarise(v.m = mean(value),
            v.med = median(value))

data.reduc2plot %>% pivot_wider(names_from = type,
                                values_from = value) %>%
  mutate(diff = reduc2 - reduc) %>%
  ungroup() %>%
  summarise(diff.min = min(diff),
            diff.m = mean(diff),
            diff.med = median(diff),
            diff.max = max(diff),
            diff.sd = sd(diff))



