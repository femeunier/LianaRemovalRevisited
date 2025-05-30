rm(list = ls())

library(ncdf4)
library(rhdf5)
library(dplyr)
library(ggplot2)

hfile <- "/home/femeunier/Documents/projects/Data_JEcol_paper/Drivers/BCI/BCI_2003JAN.h5"
A <- h5read(hfile,"ugrd")
plot(A[1:100],type = "l")

Csv <- read.csv("~/Downloads/BCI_met_drivers_2003_2016.csv") %>%
  mutate(date = sapply(strsplit(Date_UTC_start,
                          "\\ "),"[",1)) %>%
  mutate(month = as.numeric(sapply(strsplit(date,
                   "/"),"[",1)),
         year = 2000 + as.numeric(sapply(strsplit(date,
                                     "/"),"[",3)))

df.month <- Csv %>%
  # mutate(month = lubridate::month(date),
  #        year = lubridate::year(date)) %>%
  group_by(year,month) %>%
  summarise(WS.m = mean(WS_m_s,na.rm = TRUE),
            .groups = "keep")

ggplot(data = df.month) +
  geom_line(aes(x = year + (month -1/2)/12,
                y = WS.m)) +
  theme_bw()



Csv2 <- read.csv("~/Downloads/BCI_Met_data_2008-2014_1hr_UTC_v3.csv") %>%
  mutate(date = sapply(strsplit(Date_time_start_UTC,
                                "\\ "),"[",1)) %>%
  mutate(month = as.numeric(sapply(strsplit(date,
                                            "/"),"[",1)),
         year = as.numeric(sapply(strsplit(date,
                                                  "/"),"[",3)))

df.month2 <- Csv2 %>%
  # mutate(month = lubridate::month(date),
  #        year = lubridate::year(date)) %>%
  group_by(year,month) %>%
  summarise(WS.m = mean(Wind_speed_km.hr,na.rm = TRUE),
            .groups = "keep")

all <- bind_rows(df.month %>%
                   mutate(source = "2003"),
                 df.month2 %>%
                   mutate(source = "2008"))

ggplot(data = all) +
  geom_line(aes(x = year + (month -1/2)/12,
                y = WS.m,
                color = source)) +
  theme_bw()
