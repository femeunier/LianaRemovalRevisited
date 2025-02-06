rm(list = ls())

multiple.cat2keep <- bind_rows(
  readRDS("./data/multiple.cat2keep.RDS") %>%
  rename(site = plot),
  readRDS("./data/multiple.cat2keep.RDS") %>%
    mutate(plot = sub("-","_",plot)) %>%
    rename(site = plot))

multiple.cat2keep %>%
  mutate(group = substr(site,1,3)) %>%
  pull(group) %>%
  unique()


Rainfor.df <- readRDS("./data/ForestPlots/data/df.Rainfor.RDS") %>%
  filter(site %in% multiple.cat2keep[["site"]]) %>%
  left_join(multiple.cat2keep,
            by = "site") %>%
  ungroup() %>%
  mutate(site = paste0(group,"_",status.recat)) %>%
  rename(dbh = DBH,
         sp = Species,
         coi = COI) %>%
  dplyr::select(c(dbh,h,coi,sp,liana.cat,site)) %>%
  filter(dbh > 0, h > 0) %>%
  filter(!(grepl("GAU|SAT|VCR|FRP|POA",site))) # Repeated from the other data from forestplots

unique(Rainfor.df$site)


Rainfor2.df <- readRDS("./data/rainfor2.trees.RDS") %>%
  filter(site %in% multiple.cat2keep[["site"]]) %>%
  left_join(multiple.cat2keep,
            by = "site") %>%
  ungroup() %>%
  ungroup() %>%
  mutate(site = paste0(group,"_",status.recat)) %>%
  rename(sp = Species,
         coi = COI) %>%
  dplyr::select(c(dbh,h,coi,sp,liana.cat,site)) %>%
  filter(dbh > 0, h > 0) %>%
  filter(dbh < 300) # weird big tree

unique(Rainfor2.df$site)

df.Wannes <- readRDS("./data/Afritron.RDS") %>%
  filter(Plot.Code %in% multiple.cat2keep[["site"]]) %>%
  left_join(multiple.cat2keep %>%
              rename(Plot.Code = site),
            by = "Plot.Code") %>%
  mutate(group = substr(Plot.Code,1,3)) %>%
  mutate(site = paste0(group,"_",status.recat)) %>%
  dplyr::select(-c(group,Plot.Code,status.recat))



all.df <- bind_rows(
  Rainfor.df,
  Rainfor2.df,
  df.Wannes %>%
    mutate(dbh = dbh/10) %>%
    dplyr::select(dbh,h,sp,coi,liana.cat,site))


saveRDS(all.df %>%
          mutate(liana.cat = factor(liana.cat,
                                    levels = c("no","low","high"))),
        "./outputs/COI.mixed.RDS")

# scp ./outputs/COI.mixed.RDS hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/
