rm(list= ls())

library(xlsx)
library(ggthemes)
library(rnaturalearth)
library(dplyr)
library(ggplot2)

file <- "~/Documents/projects/LianaRemovalRevisited/data/Asia/PITeamEmailTracker_Felicien_AsiaAustLianas_22Oct24.xlsx"
data.plot <- read.xlsx(file,"AsiaAustPlots_with LI_Height")
plots2keep <- paste0(data.plot$PlotCode,"_",
                    data.plot$CensusNo)

raw.plots <- read.xlsx(file,
                       "PITeam_EmailTracker",
                       startRow = 2) %>%
  mutate(PlotCode_Census = paste0(PlotCode,"_",CensusNo))

data.PI <- raw.plots %>%
  filter(PlotCode_Census %in% plots2keep) %>%
  dplyr::select(PlotCode,PlotName,CensusNo,FirstName,LastName,IsDeceased,WorkEmail,PersonalEmail) %>%
  distinct() %>%
  mutate(email = case_when(is.na(WorkEmail) ~ PersonalEmail,
                           TRUE ~ WorkEmail))
plots <- raw.plots %>%
  filter(PlotCode_Census %in% plots2keep) %>%
  dplyr::select(PlotCode,
                Latitude,Longitude) %>%
  distinct()

data.plot$PlotCode[!(data.plot$PlotCode %in% plots$PlotCode)]
raw.plots %>%
  filter(grepl("SEP",PlotCode))

emails <- data.PI %>%
  pull(email) %>%
  unique()

world <- ne_countries(scale = "small",
                      returnclass = "sf")

ggplot() +
  geom_sf(data = world,color = "black",
          fill = NA) +
  geom_point(data = plots,
             aes(x = Longitude,y = Latitude), color = "red",size = 0.1) +
  scale_y_continuous(limits = c(-1,1)*23.25) +
  theme_map()

for (cemail in emails){
  print("===========")
  print(cemail)
  print(data.PI %>%
          filter(email == cemail) %>%
          dplyr::select(FirstName,LastName) %>%
          distinct())
  print(paste(data.PI %>%
          filter(email == cemail) %>%
          pull(PlotCode),collapse = "")
  )

}

A <- (data.PI %>%
        dplyr::select(FirstName,LastName,email) %>%
        distinct())
A$ACCEPT <- ""
A$ACCEPT[c(2,3,4,6,10,11)] <- 1
A$NO.REPLY <- ""
A$NO.REPLY[c(1,5,7,8,9,12)] <- 1

write.csv(A,"./outputs/Australasia/PIs.csv")

List <- read.xlsx(file,"PITeam_EmailTracker",startRow = 2)

List.merged <- List %>%
  dplyr::select(-c(ACCEPT,NO.REPLY)) %>%
  left_join(A %>%
              dplyr::select(-email),
            by = c("FirstName","LastName"))


write.csv(List.merged,"~/Downloads/List.merged.csv")

# 2,3,4,6,10,11,
