rm(list = ls())

library(reshape2)
library(dplyr)
library(ggplot2)
library(stringr)
library(brms)
library(tidyr)
library(RColorBrewer)
library(cowplot)

all.df <- readRDS("./outputs/All.COI.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10)


df.species <- (all.df %>%
  group_by(site,sp) %>%
  summarise(N = n(),
            Nno = length(which(liana.cat == "no")),
            Nlow = length(which(liana.cat == "low")),
            Nhigh = length(which(liana.cat == "high")),
            .groups = "keep") %>%
  arrange(desc(N)) %>%
  filter(!(tolower(sp) %in% c("","other","indet indet"))) %>%
  filter((Nno + Nhigh >= 50) & (min(Nno,Nhigh)>=10)))

sites <- unique(df.species$site)
sites.H <- sites
df.all <- data.frame()
for (isite in seq(1,length(sites))){

  csite <- sites[isite]
  csite.corrected <- gsub(" ", "",csite, fixed = TRUE)

  print(csite)

  diagnosis.file <- file.path("./outputs/",csite.corrected,"Diagnostics.RDS")
  if (!file.exists(diagnosis.file)) next()

  raw.diagnosis <- readRDS(diagnosis.file)

  Diagnstocis.Bayesian.site <- raw.diagnosis %>%
    arrange((waic)) %>%
    filter(rhat.max < 1.05)

  best.model <- Diagnstocis.Bayesian.site %>%
    group_by(site) %>%
    filter(waic == min(waic))

  if (best.model$fe == "none") next()

  best.model.file <- best.model %>%
    mutate(file = paste0("Fit.",csite.corrected,".",model.name,".RDS")) %>%
    pull(file)

  cfile <- file.path("./outputs/",csite.corrected,best.model.file)

  if (!file.exists(cfile)) next()

  cmodel <- readRDS(cfile)

  species <- unique(df.species %>%
                      filter(site == csite) %>%
                      pull(sp))

  newdata <- bind_rows(data.frame(dbh = 50,
                                  liana.cat = "no",
                                  sp = c(species)),
                       data.frame(dbh = 50,
                                  liana.cat = "high",
                                  sp = c(species)))  %>%
    mutate(id = 1:length(dbh))

  ccoef <- as.numeric(exp( (summary(cmodel)[["spec_pars"]][1]**2)/2))

  newnewdata <- bind_rows(data.frame(dbh = 50,
                                     liana.cat = "no",
                                     sp = c(NA)),
                          data.frame(dbh = 50,
                                     liana.cat = "high",
                                     sp = c(NA))) %>%
    mutate(id = 1:length(dbh))

  A <- posterior_epred(cmodel,
                       newdata = newdata)
  B <- posterior_epred(cmodel,
                       newdata = newnewdata,
                       re_formula = NA)

  pep <- melt(A) %>%
    rename(rep = Var1,
           id = Var2) %>%
    group_by(id) %>%
    filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
    mutate(h.pred = ccoef*exp(value)) %>%
    left_join(newdata,
              by = "id") %>%
    ungroup() %>%
    dplyr::select(-c(id,value)) %>%
    pivot_wider(names_from = liana.cat,
                values_from = h.pred) %>%
    mutate(diff.h = high - no,
           diff.h_rel = 100*(high - no)/no)

  pep2 <- melt(B) %>%
    rename(rep = Var1,
           id = Var2) %>%
    group_by(id) %>%
    filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
    mutate(h.pred = ccoef*exp(value)) %>%
    left_join(newnewdata,
              by = "id") %>%
    ungroup() %>%
    dplyr::select(-c(id,value)) %>%
    pivot_wider(names_from = liana.cat,
                values_from = h.pred) %>%
    mutate(diff.h = high - no,
           diff.h_rel = 100*(high - no)/no)

  df.all <- bind_rows(df.all,
                      pep %>%
                        mutate(site = csite),
                      pep2 %>% mutate(site = csite))
}

df.all2plot <-
  df.all %>%
  group_by(site) %>%
  filter(length(unique(sp))>2) # NA + one species


ggplot() +
  geom_density(data = df.all2plot %>% filter(!is.na(sp)),
               aes(x = diff.h_rel,group = sp),color = "darkgrey", fill = NA,
               alpha = 0.1) +
  geom_density(data = df.all2plot %>% filter(is.na(sp)),
               aes(x = diff.h_rel),color = "black", fill = NA,
               alpha = 0.1) +
  theme_bw() +
  facet_wrap(~ site, scales = "free") +
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  guides(fill = "none", color = "none")



alpha = 0.11
df.all2plot.sum <- df.all2plot %>%
  group_by(sp, dbh,site) %>%
  summarise(delta_H = median(diff.h,na.rm = TRUE),
            delta_H_low = quantile(diff.h,alpha/2,na.rm = TRUE),
            delta_H_high = quantile(diff.h,1 - alpha/2,na.rm = TRUE),
            delta_H_tilde = median(diff.h_rel,na.rm = TRUE),
            delta_H_tilde_low = quantile(diff.h_rel,alpha/2,na.rm = TRUE),
            delta_H_tilde_high = quantile(diff.h_rel,1 - alpha/2,na.rm = TRUE),
            .groups = "keep") %>%
  mutate(site = case_when(site == "Kisangani_all" ~ "Kisangani",
                          site == "ALF" ~ "Parque Cristalino",
                          site == "FLO" ~ "Fazenda Floresta",
                          site == "FRP" ~ "Fazenda Rio Preto",
                          site == "GAU" ~ "Gaúcha do Norte",
                          site == "OKU" ~ "Oku",
                          site == "PEA" ~ "Parque Estadual do Araguaia",
                          site == "POA" ~ "Porto Alegre do Norte",
                          site == "SAA" ~ "Santana do Araguaia",
                          site == "SAT" ~ "Santa Terezinha",
                          site == "SIP" ~ "Sinop",
                          site == "TAN" ~ "Tanguro",
                          site == "VCR" ~ "Vera Cruz",
                          TRUE ~ site))

df.all2plot.wide <- df.all2plot.sum %>%
  filter(!is.na(sp)) %>%
  left_join(df.all2plot.sum %>%
              filter(is.na(sp)),
            by = c("dbh","site")) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(delta_H_tilde.y_jitter =
           rnorm(n = 1,
                 mean = delta_H_tilde.y,
                 sd = max(0.25,(abs(delta_H_tilde.y))/20)))


cols <- brewer.pal(
  length(unique(df.all2plot.wide$site)),"Paired")

Aplot <- ggplot(data = df.all2plot.wide,
       aes(x = (delta_H_tilde.y_jitter),
           y = delta_H_tilde.x, color = site)) +
  geom_point() +
  # geom_jitter() +
  geom_errorbar(aes(ymin = delta_H_tilde_low.x, ymax = delta_H_tilde_high.x),
                linewidth = 0.2, width = 0) +
  geom_errorbarh(aes(xmin = delta_H_tilde_low.y, xmax = delta_H_tilde_high.y),
                 linewidth = 0.2, height = 0) +
  geom_abline(slope = 1, intercept = 0, linetype = 1) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_color_manual(values = cols) +
  labs(x = "", y = "", color = "") +
  theme_bw() +
  theme(text = element_text(size = 24),
        legend.position = c(0.25,0.8)) +
  guides(color = guide_legend(ncol=1,
                              override.aes=list(fill=NA))) +
  scale_x_continuous(limits = c(-30,40)) +
  scale_y_continuous(limits = c(-30,40)) +
  coord_equal() + guides(color = "none")

Aplot

View(df.all2plot.wide %>% filter(site == "Campinas") %>%
  filter(delta_H_tilde.x < 0))

ggplot(data = all.df %>%
         filter(site == "Campinas",
                sp %in% c("Lonchocarpus muehlbergianus",
                          "Aspidosperma polyneuron",
                          "Urera baccifera")),
       aes(x = dbh,
           y = h,
           color = liana.cat,
           fill = liana.cat)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ sp) +
  stat_smooth(method = "lm") +
  theme_bw()


################################################################################

all.df <- readRDS("./outputs/All.CA.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10)


df.species <- all.df %>%
                 group_by(site,sp) %>%
                 summarise(N = n(),
                           Nno = length(which(liana.cat == "no")),
                           Nlow = length(which(liana.cat == "low")),
                           Nhigh = length(which(liana.cat == "high")),
                           .groups = "keep") %>%
                 arrange(desc(N)) %>%
                 filter(!(tolower(sp) %in% c("","other","indet indet"))) %>%
                 filter(((Nno + Nhigh) >= 5) & (min(Nno,Nhigh)>=3))


sites <- as.character(unique(df.species$site) )
df.all <- data.frame()
for (isite in seq(1,length(sites))){

  csite <- sites[isite]
  csite.corrected <- gsub(" ", "",csite, fixed = TRUE)

  print(csite)

  diagnosis.file <- file.path("./outputs/",csite.corrected,"Diagnostics.CA.RDS")

  if (!file.exists(diagnosis.file)) next()

  raw.diagnosis <- readRDS(diagnosis.file)

  best.model.file <- paste0(rownames(raw.diagnosis)[1],".RDS")

  if (!file.exists(best.model.file)) next()

  cmodel <- readRDS(best.model.file)

  species <- unique(df.species %>%
                      filter(site == csite) %>%
                      pull(sp))

  newdata <- bind_rows(data.frame(dbh = 50,
                                  liana.cat = "no",
                                  sp = c(species,NA)),
                       data.frame(dbh = 50,
                                  liana.cat = "high",
                                  sp = c(species,NA))) %>%
    mutate(id = 1:length(dbh))

  ccoef <- as.numeric(exp( (summary(cmodel)[["spec_pars"]][1]**2)/2))
  A <- posterior_epred(cmodel,
                       newdata = newdata)

  newnewdata <- bind_rows(data.frame(dbh = 50,
                                     liana.cat = "no",
                                     sp = c(NA)),
                          data.frame(dbh = 50,
                                     liana.cat = "high",
                                     sp = c(NA))) %>%
    mutate(id = 1:length(dbh))

  A <- posterior_epred(cmodel,
                       newdata = newdata)
  B <- posterior_epred(cmodel,
                       newdata = newnewdata,
                       re_formula = NA)

  pep <- melt(A) %>%
    rename(rep = Var1,
           id = Var2) %>%
    group_by(id) %>%
    filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
    mutate(CA.pred = ccoef*exp(value)) %>%
    left_join(newdata,
              by = "id") %>%
    ungroup() %>%
    dplyr::select(-c(id,value)) %>%
    pivot_wider(names_from = liana.cat,
                values_from = CA.pred) %>%
    ungroup() %>%
    mutate(diff.CA = high - no) %>%
    mutate(diff.CA_rel = 100*diff.CA/no)

  pep2 <- melt(B) %>%
    rename(rep = Var1,
           id = Var2) %>%
    group_by(id) %>%
    filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
    mutate(h.pred = ccoef*exp(value)) %>%
    left_join(newnewdata,
              by = "id") %>%
    ungroup() %>%
    dplyr::select(-c(id,value)) %>%
    pivot_wider(names_from = liana.cat,
                values_from = h.pred) %>%
    mutate(diff.h = high - no,
           diff.h_rel = 100*(high - no)/no)

  df.all <- bind_rows(df.all,
                      pep %>%
                        mutate(site = csite),
                      pep2 %>% mutate(site = csite))
}


df.all2plot <-
  df.all %>%
  group_by(site) %>%
  filter(length(unique(sp))>=2) # NA + one species


ggplot() +
  geom_density(data = df.all2plot %>% filter(!is.na(sp)),
               aes(x = diff.CA_rel,group = sp),color = "darkgrey", fill = NA,
               alpha = 0.1) +
  geom_density(data = df.all2plot %>% filter(is.na(sp)),
               aes(x = diff.CA_rel),color = "black", fill = NA,
               alpha = 0.1) +
  theme_bw() +
  facet_wrap(~ site, scales = "free") +
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  guides(fill = "none", color = "none")


alpha = 0.11

df.all2plot.sum <- df.all2plot %>%
  group_by(sp, dbh,site) %>%
  summarise(delta_CA = median(diff.CA,na.rm = TRUE),
            delta_CA_low = quantile(diff.CA,alpha/2,na.rm = TRUE),
            delta_CA_high = quantile(diff.CA,1 - alpha/2,na.rm = TRUE),
            delta_CA_tilde = median(diff.CA_rel,na.rm = TRUE),
            delta_CA_tilde_low = quantile(diff.CA_rel,alpha/2,na.rm = TRUE),
            delta_CA_tilde_high = quantile(diff.CA_rel,1 - alpha/2,na.rm = TRUE),
            .groups = "keep") %>%
  mutate(site = case_when(site == "Kisangani_all" ~ "Kisangani",
                          site == "ALF" ~ "Parque Cristalino",
                          site == "FLO" ~ "Fazenda Floresta",
                          site == "FRP" ~ "Fazenda Rio Preto",
                          site == "GAU" ~ "Gaúcha do Norte",
                          site == "OKU" ~ "Oku",
                          site == "PEA" ~ "Parque Estadual do Araguaia",
                          site == "POA" ~ "Porto Alegre do Norte",
                          site == "SAA" ~ "Santana do Araguaia",
                          site == "SAT" ~ "Santa Terezinha",
                          site == "SIP" ~ "Sinop",
                          site == "TAN" ~ "Tanguro",
                          site == "VCR" ~ "Vera Cruz",
                          TRUE ~ site))

df.all2plot.wide <- df.all2plot.sum %>%
  filter(!is.na(sp)) %>%
  left_join(df.all2plot.sum %>%
              filter(is.na(sp)),
            by = c("dbh","site")) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(delta_CA_tilde.y_jitter =
           rnorm(n = 1,
                 mean = delta_CA_tilde.y,
                 sd = max(0.25,(abs(delta_CA_tilde.y))/20)))

cols <- brewer.pal(
  length(unique(df.all2plot.wide$site)),"Dark2")

Bplot <- ggplot(data = df.all2plot.wide,
       aes(x = (delta_CA_tilde.y),
           y = delta_CA_tilde.x, color = site)) +
  geom_point() +
  # geom_jitter() +
  geom_errorbar(aes(ymin = delta_CA_tilde_low.x, ymax = delta_CA_tilde_high.x),
                linewidth = 0.2, width = 0) +
  geom_errorbarh(aes(xmin = delta_CA_tilde_low.y, xmax = delta_CA_tilde_high.y),
                 linewidth = 0.2, height = 0) +
  geom_abline(slope = 1, intercept = 0, linetype = 1) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_color_manual(values = cols) +
  labs(x = "", y = "", color = "") +
  theme_bw() +
  theme(text = element_text(size = 24),
        legend.position = c(0.25,0.8)) +
  guides(color = guide_legend(ncol=1,
                              override.aes=list(fill=NA))) +
  coord_equal() +
  guides(color = "none")
Bplot

plot_grid(Aplot,Bplot, align = c("hv"), nrow = 1)

################################################################################
# TLS

rm(list = ls())

data <-  read.csv("./data/2019_TLS_data.csv",stringsAsFactors = FALSE) %>%
  dplyr::select(Species,Final.DBH..cm.,Height..m.,Liana,CPA..m2.,WSG,Total.vol..L.,Biomass..kg.) %>%
  rename(dbh = Final.DBH..cm.,
         h = Height..m.,
         sp = Species,
         CA = CPA..m2.,
         Vol = Total.vol..L.,
         Biomass = Biomass..kg.) %>%
  mutate(liana.cat = factor(case_when(Liana == 0 ~ "no",
                                      Liana == 1 ~ "low",
                                      Liana == 2 ~ "high"),
                            levels = c("no","low","high")))

df.species <- data %>%
  group_by(liana.cat,sp) %>%
  summarise(N = n(),
            .groups = "keep") %>%
  pivot_wider(names_from = liana.cat,
              values_from = N) %>%
  filter(!is.na(high) & !is.na(no)) %>%
  mutate(N = sum(c(no,low,high))) %>%
  arrange(desc(N)) %>%
  filter(N>3,min(high,no)>=2)


cmodel <- list()
cmodel[["H"]] <- readRDS("./outputs/Fit.BCI.2019.gmm_k.RDS")
cmodel[["CA"]] <- readRDS("./outputs/Fit.CA.power_a.RDS")
cmodel[["AGB"]] <- readRDS("./outputs/Fit.Biomass.power_a.RDS")

species <- unique(df.species %>%
                    pull(sp))

newdata <- bind_rows(data.frame(dbh = 50,
                                liana.cat = "no",
                                sp = c(species,NA)),
                     data.frame(dbh = 50,
                                liana.cat = "high",
                                sp = c(species,NA))) %>%
  mutate(id = 1:length(dbh))

################################################################################

df.all <- data.frame()

vars <- c("H","CA","AGB")
for (cvar in vars){

  ccoef <- as.numeric(exp( (summary(cmodel[[cvar]])[["spec_pars"]][1]**2)/2))
  A <- posterior_epred(cmodel[[cvar]],
                       newdata = newdata)

  newnewdata <- bind_rows(data.frame(dbh = 50,
                                     liana.cat = "no",
                                     sp = c(NA)),
                          data.frame(dbh = 50,
                                     liana.cat = "high",
                                     sp = c(NA))) %>%
    mutate(id = 1:length(dbh))

  B <- posterior_epred(cmodel[[cvar]],
                       newdata = newnewdata,
                       re_formula = NA)

  pep <- melt(A) %>%
    rename(rep = Var1,
           id = Var2) %>%
    group_by(id) %>%
    filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
    mutate(h.pred = ccoef*exp(value)) %>%
    left_join(newdata,
              by = "id") %>%
    ungroup() %>%
    dplyr::select(-c(id,value)) %>%
    pivot_wider(names_from = liana.cat,
                values_from = h.pred) %>%
    ungroup() %>%
    mutate(diff = high - no) %>%
    mutate(diff_rel = 100*diff/no)

  pep2 <- melt(B) %>%
    rename(rep = Var1,
           id = Var2) %>%
    group_by(id) %>%
    filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
    mutate(h.pred = ccoef*exp(value)) %>%
    left_join(newnewdata,
              by = "id") %>%
    ungroup() %>%
    dplyr::select(-c(id,value)) %>%
    pivot_wider(names_from = liana.cat,
                values_from = h.pred) %>%
    mutate(diff = high - no,
           diff_rel = 100*(high - no)/no)

  df.all <- bind_rows(df.all,
                      pep %>%
                        mutate(variable = cvar),
                      pep2 %>%
                        mutate(variable = cvar))
}



ggplot() +
  geom_density(data = df.all %>% filter(!is.na(sp)),
               aes(x = diff_rel,group = sp),color = "darkgrey", fill = NA,
               alpha = 0.1) +
  geom_density(data = df.all %>% filter(is.na(sp)),
               aes(x = diff_rel),color = "black", fill = NA,
               alpha = 0.1) +
  theme_bw() +
  facet_wrap(~ variable, scales = "free") +
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  guides(fill = "none", color = "none")


alpha = 0.11

df.all2plot.sum <- df.all %>%
  group_by(variable, sp, dbh) %>%
  summarise(delta = median(diff,na.rm = TRUE),
            delta_low = quantile(diff,alpha/2,na.rm = TRUE),
            delta_high = quantile(diff,1 - alpha/2,na.rm = TRUE),
            delta_tilde = median(diff_rel,na.rm = TRUE),
            delta_tilde_low = quantile(diff_rel,alpha/2,na.rm = TRUE),
            delta_tilde_high = quantile(diff_rel,1 - alpha/2,na.rm = TRUE),
            .groups = "keep")

df.all2plot.wide <- df.all2plot.sum %>%
  filter(!is.na(sp)) %>%
  left_join(df.all2plot.sum %>%
              filter(is.na(sp)),
            by = c("variable","dbh")) %>%
  ungroup() %>%
  rowwise()

cols <- brewer.pal(
  length(unique(df.all2plot.wide$sp.x)),"Dark2")

ggplot(data = df.all2plot.wide,
                aes(x = (delta_tilde.y),
                    y = delta_tilde.x, color = sp.x)) +
  geom_point() +
  geom_errorbar(aes(ymin = delta_tilde_low.x, ymax = delta_tilde_high.x),
                linewidth = 0.2, width = 0) +
  geom_errorbarh(aes(xmin = delta_tilde_low.y, xmax = delta_tilde_high.y),
                 linewidth = 0.2, height = 0) +
  geom_abline(slope = 1, intercept = 0, linetype = 1) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_color_manual(values = cols) +
  labs(x = "", y = "", color = "") +
  theme_bw() +
  theme(text = element_text(size = 24),
        legend.position = c(0.25,0.8)) +
  guides(color = guide_legend(ncol=1,
                              override.aes=list(fill=NA))) +
  coord_equal() +
  facet_wrap(~ variable, nrow = 1)
  # guides(color = "none") +
  scale_x_continuous(limits = c(-30,40)) +
  scale_y_continuous(limits = c(-30,40))





  ggplot(data = df.all2plot.wide,
         aes(x = (delta.y),
             y = delta.x, color = sp.x)) +
    geom_point() +
    geom_errorbar(aes(ymin = delta_low.x, ymax = delta_high.x),
                  linewidth = 0.2, width = 0) +
    geom_errorbarh(aes(xmin = delta_low.y, xmax = delta_high.y),
                   linewidth = 0.2, height = 0) +
    geom_abline(slope = 1, intercept = 0, linetype = 1) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_vline(xintercept = 0, linetype = 2) +
    scale_color_manual(values = cols) +
    labs(x = "", y = "", color = "") +
    theme_bw() +
    theme(text = element_text(size = 24)) +
    guides(color = guide_legend(ncol=1,
                                override.aes=list(fill=NA))) +
    facet_wrap(~ variable, nrow = 1, scales = "free") +
  guides(color = "none")


