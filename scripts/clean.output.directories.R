rm(list = ls())

library(dplyr)
library(stringr)

all.df <- readRDS("./outputs/All.COI.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10)

sites <- sort(unique(all.df$site))

for (csite in sites){

  cdir <- file.path("/data/gent/vo/000/gvo00074/felicien/R/data",csite)

  print(cdir)
  system2("rm",paste("-rf",paste0(cdir,"/*")))

}
