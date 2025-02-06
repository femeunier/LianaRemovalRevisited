rm(list = ls())

# Numbers for paper
all.df <- readRDS("./outputs/All.COI.data.RDS") %>%
  filter(dbh >= 10,
         !is.na(dbh),
         !is.na(h),
         !is.na(liana.cat))

all.df %>%
  group_by(site) %>%
  summarise(frac = 100*(1 - sum((liana.cat == "no"))/length(coi)),
            .groups = "keep",
            coi = mean(coi,na.rm = TRUE)) %>%
  arrange(desc(frac))

all.df %>%
  # group_by(site) %>%
  summarise(frac = 100*(1 - sum((liana.cat == "no"))/length(coi)),
            frac2 = 100*(sum(liana.cat == "high")/length(coi)),
            .groups = "keep",
            coi = mean(coi,na.rm = TRUE)) %>%
  arrange(desc(frac))

nrow(all.df)
