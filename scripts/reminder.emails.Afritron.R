rm(list = ls())

library(tidyverse)
library(readxl)
library(rtf)
library(dplyr)
library(mailR)

PIteams <- read_xlsx("./data/Afritron/PITeam_EmailTracker_FelicienMunierADDITIONAL_Aug2023.xlsx",
                     skip = 1,
                     sheet = "PITeamTracker") %>%
  mutate(fullName = paste(FirstName,
                          LastName))
PIs <- unique(PIteams$fullName)

# [6] "Charlotte Wheeler"      "Raymond Votere"         "Bonaventure Sonké"
# [11] "Serge Begne" "Armandu Daniels"
# [16]  "Lindsay Banin" "Lise Zemagho"
# [21] "Darlington Tuagben" "Jean-Louis Doucet"
# [26] "Seya Nshimba" "John Tshibamba Mukendi"
# [31] "Lee White"                     "Edmond Dimoto"
# [36] "Terry Sunderland"
# [41] "Moses Libalah" "Martin Gilpin"          "Mireille Hockemba"      "Patrick Boundja"
# [46] "Terry Brncic"    "Connie Clark"
# [51] "Corneille Ewango" "Peter Umunay"
# [56] "Faustin Mbayu"


# Olivier Hardy is check with others
# Update: 06/09
agreed <- c("Annette Hladik","Hannsjoerg Woell","Jon Lloyd","John T. Woods",
            "Hans Beeckman","Eric Chezeaux","Wannes Hubau","Christian Amani",
            "Jean-Louis Doucet","David Harris","Kofi Affum-Baffoe","Olivier Hardy",
            "Jason Vleminckx","John Poulsen","Kelvin Peh","Murray Collins",
            "Lan Qie","Miguel Leal","Vincent Medjibe","Oliver Phillips",
            "Ernest G Foli","Aida Cuni Sanchez","Jean-Remy Makana","Jefferson Hall",
            "Greta Dargie","Marie Noel Djuikouo K","Joey Talbot","Ted Feldpausch",
            "Simon Lewis","Kath Jeffery","Jan Reitsma",
            "Lindsay Banin",
            "Jacques Mukinzi","Amy Bennett","Martin Gilpin","Christelle Gonmadje",
            "Hermann Taedoumg")

PIs <- PIs[!(PIs %in% agreed)]

Merged <- PIteams %>%
  dplyr::select(-ACCEPT) %>%
  left_join(bind_rows(data.frame(fullName = agreed,ACCEPT = "yes",`NO REPLY` = ""),
                      data.frame(fullName = PIs, ACCEPT = "",`NO REPLY` = "yes")),
            by = "fullName")

write.csv(Merged,"./data/Afritron/responses.csv")



# for (iPI in seq(1,length(PIs))){
#
#   cPI <- PIs[iPI]
#
#   cdata <- PIteams %>%
#     filter(fullName == cPI)
#
#   cemail <- unique(cdata) %>%
#     filter(!is.na(WorkEmail)) %>%
#     pull(WorkEmail) %>% unique()
#   cplot <- cdata %>% pull(PlotCode) %>% unique() %>% sort()
#
#   if (cemail == ".") next()
#
#   cemail <- strsplit(cemail,split = " ; ")[[1]]
#
#   print(paste(iPI,cemail))
#
#   send.mail(from="felicien.meunier@gmail.com",
#             to=cemail,
#             subject="Request ForestPlots data",
#             body=paste0(
# "Dear ",cPI,',',
# "
#
# Further to my last email, I am writing with a question regarding my meta-analysis on the impact of lianas on
# tree allometries.  I am writing to request your permission to use data from the following plots for this project:
#
# ",
#
#             paste(cplot, collapse = ', '),
#
# "
#
# Please do let me know by mid September if you have any objections to the use of the data in the way described.
# If I do not hear from you by then, and if I get approval from other members of the PI team (Principal Investigator or Grant holder or Field lead)
# to use the plot data as described in my last email, then I will assume it is Ok to use them for this project.
# Please note that if any member of the PI team says no to this data request, then the data will not be used.
# As before, this collaboration will adhere to the Code of Conduct described on the ForestPlots.net website.
# The data will only be used for the purposes of the research stipulated above and will not be shared with any third parties.
# You will receive drafts of any eventual papers that emerge from this collaboration that include your data and you will have
# the opportunity to participate as a co-author.
#
# I will write again with an update once this data request phase is complete.
# Please feel free to contact me if you have any questions or need more information in the meantime.
#
# Many thanks and best wishes,
#
# Dr. Félicien Meunier"),
#             html=FALSE,
#             smtp=list(host.name = "smtp.gmail.com",
#                       port = 465,
#                       user.name = "felicien.meunier@gmail.com",
#                       passwd = "xdixqjogjbuhnwke",
#                       ssl = TRUE),
#             authenticate=TRUE,
#             send = TRUE)
#
#
# }

# Undelivered to: # mirhockemba2@yahoo.fr --> DB manager: tbrncic@wcs.org
# Problem with peter.umunay@yale.edu
