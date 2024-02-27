rm(list = ls())

library(dplyr)

list <- as.data.frame(read.csv("./data/ForestPlots/PITeam_EmailTracker_FelicienMunier_26June2023.csv",
                 header = TRUE,skip = 1,stringsAsFactors = FALSE))
unique(list$PlotCode)

list %>%
  filter(LI.measured == 1) %>%
  pull(PlotCode) %>% unique()

list %>%
  filter(LI.measured == 1) %>%
  dplyr::select(PlotCode,FirstName,LastName,WorkEmail,CensusEndDate)

PIs <- list %>%
  filter(LI.measured == 1) %>%
  dplyr::select(FirstName,LastName,WorkEmail) %>%
  distinct()

status <- list %>%
  filter(LI.measured == 1) %>%
  dplyr::select(PlotCode,FirstName,LastName,WorkEmail,CensusEndDate) %>% group_by(PlotCode) %>%
  summarise(allPIs = paste(unique(LastName),collapse = ", "),
            N = length(unique(LastName)),
            Nok = length(which(unique(LastName) %in% c(PIs$LastName[c(1,2,3,4,6)])))) %>%
  mutate(frac = Nok/N)
status %>%
  pull(frac) %>% table()
