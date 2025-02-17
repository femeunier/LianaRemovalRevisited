rm(list = ls())

library(dplyr)
library(tidyr)
library(xlsx)
library(stringr)
library(ggplot2)

df.rainfor <- readRDS("./data/ForestPlots/data/df.Rainfor.RDS") %>%
  mutate(group = substr(site,1,3)) %>%
  filter(!(group %in% c("VCR","GAU"))) %>% # Repeated from the other data from forestplots
  filter(DBH >= 10) %>%
  filter(!is.na(DBH)) %>%
  filter(!is.na(h)) %>%
  filter(!is.na(liana.cat))

Rainfor.plots <- df.rainfor %>%
  pull(site) %>%
  unique() %>%
  sort()

df.rainfor2 <- readRDS("./data/rainfor2.trees.RDS") %>%
  ungroup() %>%
  rename(sp = Species,
         coi = COI) %>%
  dplyr::select(c(dbh,h,coi,sp,liana.cat,site,group)) %>%
  filter(dbh > 0, h > 0) %>%
  filter(dbh < 300) %>% # weird big tree
  mutate(h = case_when(h> 50 & dbh < 25 ~ h/10,
                       TRUE  ~ h)) %>%
  filter(h > 3) %>%
  filter(!(group %in% c("POA","SAT","FRP"))) # Repeated from the other data from forestplots


Rainfor2.plots <- df.rainfor2 %>%
  pull(site) %>%
  unique()

df.afritron <-
  readRDS("./data/Afritron.RDS")

afritron.plots <- df.afritron %>%
  pull(Plot.Code) %>%
  unique() %>%
  sort()

df.Asia <- readRDS("./data/Asia/raw.data.RDS") %>%
  dplyr::select(DBH,Species,h,COI,liana.cat,site)

Asia.plots <- df.Asia %>%
  pull(site) %>%
  unique()

#######################################################

all.inds <- bind_rows(df.rainfor %>%
                        ungroup() %>%
                        dplyr::select(DBH,liana.cat,h,Species,site) %>%
                        rename(plot = site,
                               dbh = DBH),
                      df.rainfor2 %>%
                        ungroup() %>%
                        dplyr::select(dbh,liana.cat,h,sp,site) %>%
                        rename(plot = site,
                               Species = sp),

                      df.afritron %>%
                        rename(plot = Plot.Code) %>%
                        mutate(dbh = dbh/10),

                      df.Asia %>%
                        dplyr::select(DBH,h,liana.cat,site) %>%
                        rename(dbh = DBH,
                               plot = site)

) %>%
  ungroup() %>%
  filter(dbh >= 10,
         h > 0) %>%
  filter(!is.na(dbh)) %>%
  filter(!is.na(h)) %>%
  filter(!is.na(liana.cat)) %>%
  dplyr::select(dbh,h,liana.cat,plot)

df.N.plots <- all.inds %>%
  group_by(plot,liana.cat) %>%
  summarise(N = n(),
            .groups = "keep") %>%
  mutate(plot = sub("_","-",plot))


all.plots <- bind_rows(
  data.frame(plot = Rainfor.plots,
             origin = "rainfor"),
  data.frame(plot = Rainfor2.plots,
             origin = "rainfor"),
  data.frame(plot = afritron.plots,
             origin = "afritron"),
  data.frame(plot = Asia.plots,
             origin = "asiafor")) %>%
  mutate(plot = sub("_","-",plot))

##################################################################
# Now we load metadata

metadata <-
  bind_rows(read.xlsx("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/FP/ForestPlots_Access_FelicienMunier_19Oct23.xlsx",
                      sheetName = "AcceptedCensuses_with_LI") %>%
              dplyr::select(PlotCode,ForestMoistureName,
                            ForestEdaphicName,ForestElevationName,
                            ForestStatusName) %>%
              rename(plot = PlotCode,
                     Forest.status.plotview = ForestStatusName) %>%
              mutate(origin = "afritron"),

            read.xlsx("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/FP/Rainfor1.xlsx",sheetName = "Sheet1") %>%
              dplyr::select(plot,ForestMoistureName,
                            ForestEdaphicName,ForestElevationName,
                            Forest.status.plotview) %>%
              mutate(origin = "rainfor"),

            read.xlsx("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/FPAccess_FelicienMeunier_04Feb25.xlsx",sheetName = "AsiaAustPlots_with LI_Height") %>%
              dplyr::select(PlotCode,ForestMoistureName,
                            ForestEdaphicName,ForestElevationName,
                            ForestStatusName) %>%
              rename(plot = PlotCode,
                     Forest.status.plotview = ForestStatusName) %>%
              mutate(origin = "asiafor"),


            read.xlsx("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/FP/PITeamEmailTracker_SruthiKrishnaMoorthy_030724_complete.xlsx",
                      sheetName = "LastCensus_withHeight_AND_LI") %>%
              dplyr::rename(plot = PlotCode,
                            Forest.status.plotview = ForestStatusName) %>%
              dplyr::select(plot,
                            ForestMoistureName,ForestEdaphicName,
                            ForestElevationName,Forest.status.plotview) %>%
              filter(!(plot %in% c("GAU-01","GAU-03"))) %>%
              mutate(origin = "rainfor")) %>%
  mutate(plot = sub("_","-",plot)) %>%
  distinct()

################################################################################

Afritron <- bind_rows(readRDS("./data/Afritron.eq.RDS") %>%
  dplyr::select(group,site) %>%
  distinct(),
  data.frame(group = "KSN", site ="Kisangani_all")) %>%
  arrange(group)

plots.run <- readRDS("./outputs/All.COI.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10) %>%
  dplyr::select(site) %>%
  distinct() %>%
  mutate(used = TRUE) %>%
  left_join(Afritron,
            by = "site") %>%
  mutate(site = case_when(is.na(group) ~ site,
                          TRUE ~ group)) %>%
  dplyr::select(-group)

all.plots.site <- all.plots %>%
  mutate(site = case_when(plot == "YOK" ~ "KSN",
                          TRUE ~ substr(plot,1,3))) %>%
  left_join(plots.run,
            by = "site",relationship = "many-to-many") %>%
  filter(used) %>%
  dplyr::select(-used) %>%
  arrange(site)

all.plots.site.md <- all.plots.site %>%
  left_join(metadata %>%
              dplyr::select(-origin),
            by = "plot")

all.plots.site.md.sum <- all.plots.site.md %>%
  group_by(site) %>%
  summarise(origin = unique(origin),

            Nmoist = length(unique(ForestMoistureName)),
            Nedaphic = length(unique(ForestEdaphicName)),
            Nstatus = length(unique(Forest.status.plotview)),
            Nelevation = length(unique(ForestElevationName)),

            ForestMoistureName = paste(unique(ForestMoistureName),collapse = "_"),
            ForestEdaphicName = paste(unique(ForestEdaphicName),collapse = "_"),
            Forest.status.plotview = paste(unique(Forest.status.plotview),collapse = "_"),
            ForestElevationName = paste(unique(ForestElevationName),collapse = "_"))

plots.with.multiple.conditions <- all.plots.site.md.sum %>%
  filter(Nmoist > 1 | Nedaphic > 1 | Nstatus > 1 | Nelevation > 1) %>%
  arrange(site) %>%
  dplyr::select(-c(Nmoist,Nedaphic,Nstatus,Nelevation)) %>%
  pivot_longer(cols = c(ForestMoistureName,ForestEdaphicName,
                        Forest.status.plotview,ForestElevationName)) %>%
  filter(grepl("_",value))

plots.with.multiple.conditions %>%
  dplyr::select(site,name,value,origin)

# "BUL": Terra Firma AND White sand  --> remove white sand (only), remove BUL-07, BUL-08, BUL-40
# "CRP": frequent understory fire AND Old-growth --> keep together, Old-Growth
# "GBO": Old-growth AND Mixed: Old-growth and Logged --> keep together, Old-Growth
# "JBS": Terra Firma AND Seasonal floodplain  --> remove seasonal floodplain, Terra Firma, remove JBS-02
# "LKM": Mixed: Old-growth and Logged AND Old-growth AND Logged --> remove logged and Mixed, Old-growth, remove LKM-01 LKM-04
# "NGI": Lowland AND Premontane --> remove premontane, all Lowland, remove  NGI-11
# "NNP": Old-growth AND Logged --> remove NNP (which was merged with NNN)
# "TAM": Terra Firma AND Swamp AND Former floodplain --> remove TAM-03,TAM-06, to only keep Terra Firma

# "LFB": Moist AND Dry // Terra Firma AND Rarely flooded // Old-growth AND Burned   --> remove LFB-03 to make it consisttent

# "PEA": Mixed: Old-growth and Burned AND Old-growth --> split into Old-growth and Mixed
# "NXV": Dry AND Moist // Terra Firma AND Seasonal floodplain // Mixed: Old-growth and Burned AND Old-growth --> split into Mixed: Old-growth and Burned/dry, Old-growth/dry, Old-growth/moist
# "GAU": Terra Firma_Rarely flooded AND NULL // Old-growth_Mixed: Old-growth and Burned AND NULL --> Remove GAU-03 and split into Mixed and Old-Growth


metadata %>%
  filter(grepl("PEA",plot))

ggplot(data = all.inds %>%
         filter(!(dbh > 45 & h < 6)) %>%
         mutate(plot = sub("_","-",plot)) %>%
         filter(plot != "GAU-03") %>%
         filter(grepl("PEA",plot)) %>%
         left_join(metadata,
                   by = "plot"),
       aes(x = dbh, y = h, color = liana.cat, fill = liana.cat)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  # scale_x_log10() +
  # scale_y_log10() +
  facet_wrap(~  interaction(Forest.status.plotview)) +
  theme_bw()

View(metadata %>%
  # filter(plot != "GAU-03") %>%
  filter(grepl("NXV",plot)) %>%
  left_join(all.inds %>%
              mutate(plot = sub("_","-",plot)) %>%
              group_by(plot,liana.cat) %>%
              summarise(N = n()),
            by = "plot") %>%
  group_by(ForestMoistureName,ForestElevationName,
           ForestEdaphicName,
           Forest.status.plotview) %>%
  # filter(N >= 10) %>%
  summarise(plots = paste(unique(plot),collapse = "_"),
            Ntot = sum(N,na.rm = TRUE),
            Nmin = min(N,na.rm = TRUE),
            Ncat = length(unique(liana.cat[!is.na(liana.cat)])),
            .groups = "keep") %>%
  filter(Ntot >= 50,Ncat == 3))


stop()

main.OP <- readRDS("./outputs/Main.OP.50.RDS") %>%
  group_by(site) %>%
  summarise(delta_H = mean(diff_h/no,
                           na.rm = TRUE),
            .groups = "keep") %>%
  left_join(Afritron,
            by = "site") %>%
  mutate(site = case_when(is.na(group) ~ site,
                          TRUE ~ group)) %>%
  dplyr::select(-group)

main.OP %>%
  filter(site %in% c(plots.with.multiple.conditions$site)) %>%
    dplyr::select(site,delta_H)

plots.with.single.conditions <- all.plots.site.md.sum %>%
  filter(Nmoist == 1 & Nedaphic == 1 & Nstatus == 1 & Nelevation == 1) %>%
  arrange(site) %>%
  dplyr::select(-c(Nmoist,Nedaphic,Nstatus,Nelevation))

