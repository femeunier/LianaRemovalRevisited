rm(list = ls())

load("~/Downloads/field.series.new2.RData")
load("~/Downloads/df.fit.all.RData")

lu.table <- df.fit.all %>%
  mutate(dbh.round = round(dbh,1),
         timing = as.integer(as.numeric(timing)))

field.series.new2.mod <- field.series.new2 %>%
  mutate(DBH = round(DBH,1),
         timing = as.integer(as.numeric(timing))) %>%
  left_join(lu.table %>%
              dplyr::select(dbh.round,timing,height,color.code) %>%
              rename(DBH = dbh.round),
            by = c("timing","color.code","DBH"))

weird <- field.series.new2.mod %>%
  filter(is.na(height))

lu.table %>%
  filter(color.code == weird$color.code[1],
         dbh.round == weird$DBH[1])
