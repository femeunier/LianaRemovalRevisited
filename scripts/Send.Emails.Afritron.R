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



write.csv(PIteams %>% filter(fullName %in% PIs) %>%
            dplyr::select(fullName,WorkEmail) %>%
            distinct(),
          "./data/PI.Afritron.csv")

##### Definitely see this: https://datawookie.github.io/emayili/
##### and if using gmail then also accept to use less secure apps here #####https://myaccount.google.com/lesssecureapps
#
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
# #   send.mail(from="felicien.meunier@gmail.com",
# #             to=cemail,
# #             subject="Request ForestPlots data",
# #             body=paste0(
# # "Dear ",cPI,',',
# # "
# #
# # I am working as postdoc at Ghent University under the supervision of Hans Verbeeck and Wannes Hubau.
# # I am currently running a meta-analysis on the impact of lianas on tree allometries and I am contacting you to request data listed in the ForestPlots database.
# # In particular, I am interested in the individual tree DBH, species, height and liana infestation (COI) of the following plots:
# #
# # ",
# #
# #             paste(cplot, collapse = ', '),
# #
# # "
# #
# # As a member of the PI team (i.e. you are either the Principal investigator, Grant holder or Field leader)
# # for these plots/censuses we would be very grateful if you will grant permission to use your plot data
# # for this purpose. I have also contacted the other members of the PI team associated with these plots/censuses,
# # if however you know of anyone else that we need to contact to request permission please let me know so that
# # I can also write to them. If you have any doubts regarding this data request please discuss this with the PI team.
# #
# # This collaboration will adhere to the Code of Conduct described on the ForestPlots.net website and we would of course offer you co-authorship on
# # any resulting paper, keep you involved with progress, and include all acknowledgements that are appropriate.
# # The data will only be used for the purposes of the research stipulated above and will not be shared with any third parties.
# #
# # It would be very useful if you could reply - even just a brief yes to let us know if you are happy for your data to be used for this project.
# # I would be also very happy to give more details/explanation on this study if you'd wish.
# #
# # Many thanks and best wishes,
# #
# # Dr. Félicien Meunier"),
# #             html=FALSE,
# #             smtp=list(host.name = "smtp.gmail.com",
# #                       port = 465,
# #                       user.name = "felicien.meunier@gmail.com",
# #                       passwd = "",
# #                       ssl = TRUE),
# #             authenticate=TRUE,
# #             send = TRUE)
#
#
# }

# Update 30/08/08
# Status:
# Lloyd: retired --> Acknowledgements

# Delivery failed:
# mirhockemba2@yahoo.fr --> DB manager: tbrncic@wcs.org
# hladik@mnhn.fr  --> DB manager: S.L.Lewis@leeds.ac.uk --> Acknoledgements
# HannsjoergWoell@aol.com --> DB manager: Wannes.Hubau@UGent.be --> Said "nevermind"
# Corneille Ewango --> Send to Unikis
# vjohntwoods38@gmail.com --> sent to johntwoods38@gmail.co

# Accepted: Hans Beeckman ; Eric Chezeaux ; Wannes Hubau ; Christian Amani ; Jean-Louis Doucet ; David Harris ;
# Kofi Affum-Baffoe ; Olivier Hardy ; Jason Vleminckx ; John Poulsen ;
# Kelvin Peh ; Murray Collins ; Lan Qie ; Miguel Leal ; Vincent Medjibe ;
# Oliver Philips ; Ernest Foli ; Aida Cuni Sanchez ; Jean-Remy Makana
# Greta Dargie ; Mike O'Sullivan ; Janvier Lisingo ; Jefferson Hall
#  Marie Noel Nguembou ; Joey Talbot ; John T. Woods ; Ted Feldpausch

# To be added: Jason Vleminckx , Janvier Lisingo


