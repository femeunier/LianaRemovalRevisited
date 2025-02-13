rm(list = ls())

library(dplyr)
library(xlsx)

df.rainfor <- readRDS("./data/ForestPlots/data/df.Rainfor.RDS") %>%
  mutate(group = substr(site,1,3)) %>%
  filter(!(site %in% c("VCR","GAU"))) %>% # Repeated from the other data from forestplots
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
             origin = "afritron")) %>%
  mutate(plot = sub("_","-",plot))

all.plots %>%
  group_by(plot) %>%
  summarise(N = length(unique(origin))) %>%
  filter(N > 1)  # No doublons

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
    mutate(origin = "Asia"),


  read.xlsx("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/FP/PITeamEmailTracker_SruthiKrishnaMoorthy_030724_complete.xlsx",
            sheetName = "LastCensus_withHeight_AND_LI") %>%
    dplyr::rename(plot = PlotCode,
                  Forest.status.plotview = ForestStatusName) %>%
    dplyr::select(plot,
                  ForestMoistureName,ForestEdaphicName,
                  ForestElevationName,Forest.status.plotview,
                  ) %>%
    mutate(origin = "rainfor2")) %>%
  mutate(plot = sub("_","-",plot)) %>%
  distinct()

plots.with.multiple.conditions <-
  metadata %>%
  filter(plot %in% unique(df.N.plots$plot)) %>%
  group_by(plot) %>%
  summarise(origin = unique(origin),
            Nmoist = length(unique(ForestMoistureName)),
            Nedaphic = length(unique(ForestEdaphicName)),
            Nstatus = length(unique(Forest.status.plotview)),
            Nelevation = length(unique(ForestElevationName))) %>%
  filter(Nmoist > 1 | Nedaphic > 1 | Nstatus > 1 | Nelevation > 1)

Afritron.plotviewID <- readRDS("./outputs/plot.vs.plotviewID")

metadata.nodoublons <- bind_rows(
  metadata %>%
    filter(!(plot %in% plots.with.multiple.conditions[["plot"]])),

  metadata %>%
    filter(plot %in% plots.with.multiple.conditions[["plot"]])
    # filter(PlotViewID %in% c(Afritron.plotviewID[["PlotViewID"]]))
  ) %>%
  # dplyr::select(-PlotViewID) %>%
  distinct()

metadata.nodoublons %>%
  filter(plot %in% c(plots.with.multiple.conditions %>%
    pull(plot))) %>%
  arrange(plot)


all.plots.md <- df.N.plots %>%
  left_join(metadata.nodoublons,
            by = "plot") %>%
  mutate(group = substr(plot,1,3)) %>%
  mutate(status.recat = case_when(
    Forest.status.plotview == "NULL" ~ NA_character_,
    Forest.status.plotview == "Infrequent understory fire" ~ "Old-growth",
    grepl("Secondary",Forest.status.plotview) ~ "Secondary",
    Forest.status.plotview == "Undisturbed" ~ "Old-growth",
    grepl("Mixed",Forest.status.plotview) ~ "Mixed",
    TRUE ~ Forest.status.plotview
  ))


Nmin <- 10

all.plots.md.sum <- all.plots.md %>%
  group_by(group) %>%
  summarise(liana.cats = (length(unique(liana.cat))),

            Nhigh = sum(N[liana.cat == "high"]),
            Nlow = sum(N[liana.cat == "low"]),
            Nno = sum(N[liana.cat == "no"]),

            Nmoist = length(unique(ForestMoistureName)),
            Nedaphic = length(unique(ForestEdaphicName)),
            Nstatus = length(unique(status.recat)),
            Nelevation = length(unique(ForestElevationName)),

            .groups = "keep") %>%
  mutate(keep = (liana.cats == 3) &
           (Nhigh > Nmin) & (Nlow > Nmin) & (Nno > Nmin))

# With the 3 categories and Nmin trees of each

plots2keep <- all.plots.md.sum %>%
  pull(group)

# Stats per elevation
all.plots.md %>%
  filter(group %in% plots2keep) %>%
  group_by(ForestElevationName) %>%
  summarise(Ngroups = length(unique(group)),
            Ntrees = sum(N))

# groups with more than one elevation
groups.with.multiple.elevation <-
  all.plots.md.sum %>%
  filter(keep) %>%
  filter(Nelevation > 1) %>%
  arrange(desc(Nelevation))

# groups with more than one edaphic
groups.with.multiple.edaphic <-
  all.plots.md.sum %>%
  filter(keep) %>%
  filter(Nedaphic > 1) %>%
  arrange(desc(Nedaphic))


all.plots.md %>%
  filter(group %in% groups.with.multiple.elevation[["group"]]) %>%
  group_by(group,ForestElevationName,liana.cat) %>%
  summarise(N = sum(N),
            origin = paste(unique(origin),collapse = "-"),
            .groups = "keep") %>%
  pivot_wider(names_from = liana.cat,
              values_from = N) %>%
  mutate(keep = (high > Nmin) & (low > Nmin) & (no > Nmin))



# Stats per status
all.plots.md %>%
  filter(group %in% plots2keep) %>%
  group_by(status.recat) %>%
  summarise(Ngroups = length(unique(group)),
            Ntrees = sum(N))

groups.with.multiple.status <-
  all.plots.md.sum %>%
  filter(keep) %>%
  filter(Nstatus > 1) %>%
  arrange(desc(Nstatus))

multiple.cat2keep <- all.plots.md %>%
  filter(group %in% groups.with.multiple.status[["group"]]) %>%
  group_by(group,status.recat,liana.cat) %>%
  summarise(N = sum(N),
            plot = paste(unique(plot),collapse = "|"),
            origin = paste(unique(origin),collapse = "|"),
            .groups = "keep") %>%
  pivot_wider(names_from = liana.cat,
              values_from = N) %>%
  mutate(keep = (high > Nmin) & (low > Nmin) & (no > Nmin)) %>%
  ungroup() %>%
  filter(keep) %>%
  group_by(group) %>%
  mutate(Ncat = length(unique(status.recat))) %>%
  filter(Ncat > 1)


saveRDS(all.plots.md %>%
          filter(group %in% multiple.cat2keep[["group"]]) %>%
          ungroup() %>%
          filter(!is.na(status.recat)) %>%
          dplyr::select(plot,status.recat) %>%
          dplyr::distinct(),
        "./data/multiple.cat2keep.RDS")

################################################################
# Effect

Afritron <- readRDS("./data/Afritron.eq.RDS") %>%
  dplyr::select(group,site) %>%
  distinct()

Main.OP <- readRDS("./outputs/Main.OP.50.RDS")
Main.OP.sum <- Main.OP %>%
  group_by(site) %>%
  summarise(delta_H = mean(diff_h/no,
                           na.rm = TRUE),
            .groups = "keep") %>%
  rename(group = site)

Effects.vs.cat <- all.plots.md %>%
  filter(group %in% c(all.plots.md.sum %>%
                        filter(keep) %>%
                        pull(group))) %>%
  filter(!(group %in% c(groups.with.multiple.status[["group"]],
                        groups.with.multiple.elevation[["group"]],
                        groups.with.multiple.edaphic[["group"]]))) %>%
  left_join(Afritron,
            by = 'group') %>%
  mutate(group = case_when(!is.na(site) ~ site,
                           TRUE ~ group)) %>%
  left_join(Main.OP.sum,
            by = "group") %>%
  ungroup() %>%
  dplyr::select(-c(liana.cat,plot,N)) %>%
  distinct()


ggplot(data = Effects.vs.cat,
       aes(x = origin,
           y = delta_H,
           fill = origin,
           color = origin)) +
  geom_boxplot(alpha = 0.5,outlier.shape = NA) +
  geom_hline(yintercept = 0,linetype = 2) +
  geom_jitter() +
  theme_bw() +
  labs(x = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  guides(color = "none")


ggplot(data = Effects.vs.cat,
       aes(x = ForestMoistureName,
           y = delta_H,
           fill = ForestMoistureName,
           color = ForestMoistureName)) +
  geom_boxplot(alpha = 0.5,outlier.shape = NA) +
  geom_jitter() +
  geom_hline(yintercept = 0,linetype = 2) +
  theme_bw() +
  labs(x = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  guides(color = "none")


ggplot(data = Effects.vs.cat,
       aes(x = status.recat,
           y = delta_H,
           fill = status.recat,
           color = status.recat)) +
  geom_boxplot(alpha = 0.5,outlier.shape = NA) +
  geom_jitter() +
  geom_hline(yintercept = 0,linetype = 2) +
  theme_bw() +
  labs(x = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  guides(color = "none")

ggplot(data = Effects.vs.cat,
       aes(x = ForestElevationName,y = delta_H)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  geom_hline(yintercept = 0,linetype = 2) +
  theme_bw() +
  labs(x = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


ggplot(data = Effects.vs.cat,
       aes(x = ForestEdaphicName,y = delta_H, fill = ForestEdaphicName, color = ForestEdaphicName)) +
  geom_boxplot(alpha = 0.5,outlier.shape = NA) +
  geom_jitter() +
  geom_hline(yintercept = 0,linetype = 2) +
  theme_bw() +
  labs(x = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

Effects.vs.cat %>%
  filter(delta_H > 0) %>%
  dplyr::relocate(group,delta_H,origin) %>%
  arrange(desc(delta_H))

