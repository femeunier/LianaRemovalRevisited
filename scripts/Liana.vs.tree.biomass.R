rm(list = ls())

library(dplyr)
library(ggplot2)
library(minpack.lm)

c2b.ratio = 0.456 # 0.4735

################################################################################
# Gigante

# Lianas
data.gigante <- read.csv("/home/femeunier/Documents/projects/LVLRE.long/data/Liana 2013-2018 for R V7 - 2023 Control plots-1.csv") %>%
  dplyr::select(PLOT,DIAM.2013,DIAM.2018) %>%
  pivot_longer(cols = starts_with("DIAM."),
               names_to = "year",
               values_to = "DBH") %>%
  mutate(year = as.numeric(substr(year,6,10)),
         DBH = DBH/10)


data.liana.AGB <- data.gigante %>%
  group_by(PLOT,year) %>%
  summarise(N = length(DBH[!is.na(DBH)]),
            BA = sum(DBH*DBH,na.rm = TRUE)*pi/4/40/40,
            AGBC = c2b.ratio*sum(exp(-0.968+2.657*log(DBH)),na.rm = TRUE)/(40*40),
            .groups = "keep")

# Trees
census.data <- read.csv(file.path("/home/femeunier/Documents/projects/LVLRE.long/",
                                  "data",
                                  "c97to13.csv"),stringsAsFactors = FALSE)

trees <- census.data %>% dplyr::select(tag,sp,sp13,dbhtot13,code13,gx13,gy13,origin,status13,hght13,plot2030,plot3030,plot4040,
                                                  block,repl,tmt) %>%
  filter(status13 %in% c("A","P"))

df.height <- data.frame(dbh = c(census.data$dbhtot98,census.data$dbhtot13),
                        h = c(census.data$hght98,census.data$hght13)) %>%
  filter(dbh > 1,!is.na(h),!is.na(dbh))

# Fit dbh vs tree allometry
href <- 61.7;b1Ht <- 0.0352;b2Ht <- 0.694
m0 <- nlsLM(data = df.height,
            h ~ href*(1 -exp(-b1Ht*((dbh)**b2Ht))),
            start=list(href=href, b1Ht=b1Ht, b2Ht = b2Ht), control = nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024/10,
                                                                                 printEval = TRUE, warnOnly = TRUE))
dbhs <- 1:200
df.h.pred <- data.frame(dbh = dbhs,
                        h = predict(m0,newdata = data.frame(dbh = dbhs*10)))

ggplot(data = df.height) +
  geom_point(aes(x = dbh/10, y = h)) +
  geom_line(data = df.h.pred,
            aes(x = dbh,y = h), color = "red") +
  theme_bw()

# Local WD data
WDdata <- read.csv("/home/femeunier/Documents/projects/LVLRE.long/data/WoodDens_20130917.csv")

WD.db <- (WDdata %>% dplyr::select(SP.,SG100C_AVG)) %>% group_by(SP.) %>% summarise(WD = mean(SG100C_AVG,na.rm = TRUE)) %>% rename(sp = SP.)
rhos <- trees %>%
  dplyr::select(sp) %>% left_join(WD.db) %>% mutate(WD = case_when(is.na(WD) ~ mean(WD.db$WD,na.rm = TRUE),
                                                                   TRUE ~ WD))

trees40 <- trees %>%
  mutate(rho = rhos$WD) %>%
  filter(!is.na(dbhtot13)) %>%
  filter(plot4040 %in% c(6,12,26,36)) %>%
  mutate(h = predict(m0,newdata = data.frame(dbh = dbhtot13))) %>%
  mutate(AGB = 0.0673*(rho*((dbhtot13/10)**2)*h)**0.976,
         AGBC = c2b.ratio*AGB)

data.AGB <- trees40 %>%
  group_by(plot4040) %>%
  summarise(N = length(dbhtot13[!is.na(dbhtot13)]),
            BA = sum(dbhtot13*dbhtot13,na.rm = TRUE)*pi/4/10/10/40/40,
            AGBC = sum(AGBC,na.rm = TRUE)/(40*40),
            .groups = "keep")

r <- data.liana.AGB %>%
  filter(year == 2013) %>% pull(AGBC) /
  data.AGB %>% pull(AGBC) *100


################################################################################
# BCI

data.file <- "/home/femeunier/Documents/projects/LianaBCI/data/2021-09-22_liana_bci_data_V9_0_felicien.csv"
data.filtered <- read.csv(data.file) %>%
  mutate(DBH = DBH/10)


Delta_X = Delta_Y = 20
data.patch <- data.filtered %>% filter(PX >= 0 & PX < 1000 &
                                         PY >= 0 & PY < 500,
                                       Status %in% c("L","N")) %>% ungroup() %>%
  mutate(patch = LianaBCI::patchnumber_from_position(PX,PY,patch_X = Delta_X,patch_Y = Delta_Y,
                                                     extr_x = c(0,1000),
                                                     extr_y = c(0,500))[["patch"]]) %>%
  mutate(BA = pi/4*(DBH)^2,
         AGB = exp(-0.968+2.657*log(DBH)))


data.patch.m <- data.patch %>%
  group_by(Census,patch) %>%
  summarise(N = length(DBH[DBH > 0])/(Delta_X*Delta_Y),
            BA = pi*sum(DBH*DBH,na.rm = TRUE)/4/(Delta_X*Delta_Y),
            AGBC = c2b.ratio*sum(exp(-0.968+2.657*log(DBH)),na.rm = TRUE)/(Delta_X*Delta_Y),
            .groups = "keep") %>%
  ungroup() %>%
  complete(Census = 1:2,
           patch = 1:(500000/(Delta_X*Delta_Y)),
           fill = list(N = 0,
                       BA = 0,
                       AGBC = 0))

data.census.m <- data.patch.m %>%
  group_by(Census) %>%
  summarise(N.m = mean(N),
            N.sd = sd(N),

            BA.m = mean(BA),
            BA.sd = sd(BA),

            AGBC.m = mean(AGBC),
            AGBC.sd = sd(AGBC),

            .groups = "keep")

0.679/(0.679 + 14)

################################################################################
# Ituri

# Lianas
datafile.Lenda <- "/home/femeunier/Documents/projects/BCI.AGB/data/Ituri/Lenda lianas.csv"
data.Lenda <- read.csv(datafile.Lenda, stringsAsFactors = FALSE) %>% mutate(DATE0 = as.Date(as.character(DATE0),"%d/%m/%Y"),
                                                                            DATE1 = as.Date(as.character(DATE1),"%d/%m/%Y"),
                                                                            DATE2 = as.Date(as.character(DATE2),"%d/%m/%Y")) %>%
  filter(DBH2 <= 450) # ???

data.Lenda.mod <- data.Lenda %>% mutate(DBH.94 = case_when(DBH0 > 0 ~ DBH0,
                                                           TRUE ~ NA_integer_),
                                        DBH.01 = case_when(DBH1 > 0 ~ DBH1,
                                                           TRUE ~ NA_integer_),
                                        DBH.07 = case_when(DBH2 > 0 ~ DBH2,
                                                           TRUE ~ NA_integer_)) %>%
  mutate(ID = 1:nrow(data.Lenda))


data.Lenda.mod.long <- data.Lenda.mod %>%
  dplyr::select(c(ID,SITE,PLACETTE,DBH.94,DBH.01,DBH.07)) %>%
  pivot_longer(cols = c(DBH.94,DBH.01,DBH.07),
               values_to = "DBH",
               names_to = "var") %>%
  mutate(year = case_when(var == "DBH.94" ~ 1994,
                          var == "DBH.01" ~ 2001,
                          TRUE ~ 2007)) %>%
  mutate(DBH = DBH/10) %>%
  mutate(AGB = exp(-0.968+2.657*log(DBH)))


data.Lenda.mod.long.sum <-
  data.Lenda.mod.long %>% group_by(SITE,year) %>%
  summarise(N = length(DBH[!is.na(DBH)])/(10),
            mean.DBH = mean(DBH,na.rm = TRUE),
            AGBC = c2b.ratio*sum(AGB,na.rm = TRUE)/(10*10000),
            BA = sum(DBH*DBH*pi/4,na.rm = TRUE)/(10*10000),
            .groups = "keep")


################################################################################
# Trees

Census <- 1:3
years <- c("94","01","07")
Plots <- 1:2
Sites <- c("Edoro","Lenda")

directory <- "/home/femeunier/Documents/projects/BCI.AGB/data/Ituri/"

df.tree.all <- data.frame()
for (isite in seq(1,length(Sites))){
  for (iplot in seq(1,length(Plots))){
    for (icensus in seq(1,length(Census))){

      census.file <- file.path(directory,paste0("Tree.",Sites[isite],Plots[iplot],".census",Census[icensus],".txt"))

      df.tree <- read.csv(census.file,stringsAsFactors = FALSE,sep = "\t",header = TRUE) %>%
        mutate(site = Sites[isite],
               plot = Plots[iplot],
               year = years[icensus])

      df.tree.all <- bind_rows(list(df.tree.all,
                                    df.tree))
    }
  }
}

# Tree height allometry
# https://static-content.springer.com/esm/art%3A10.1038%2Fnature07771/MediaObjects/41586_2009_BFnature07771_MOESM320_ESM.pdf
# 54.01 * (1-exp(-0.053(d^0.759)))

# # Tree biomass allemotry
# file:///home/femeunier/Downloads/Fayolle_et_all_2018.pdf
# A regional allometry for the Congo basin forests based on the largest ever
# destructive samplin
# AGB = 0.125*WSG**1.079 *D**2.210 * H**0.506
# AGB = 0.055*(WSG*D**2 *H)**0.989


GWDD <- read.csv("/home/femeunier/Documents/data/robin/GWDDB.csv") %>%
  mutate(WD = Wood.density..g.cm.3...oven.dry.mass.fresh.volume) %>%
  dplyr::select(Family,Binomial,WD,Region)

df.tree.all %>% group_by(site,plot) %>% summarise(N = length(TreeID))
species.Ituri <- df.tree.all %>% group_by(Latin,site,plot) %>% summarise(N = length(TreeID),
                                                                         .groups = "keep") %>% arrange(desc(N))

GWDD.f <- GWDD %>% filter(Binomial %in% species.Ituri$Latin) %>% group_by(Binomial) %>% summarise(WDm = mean(WD),
                                                                                                  .groups = "keep")

species.Ituri %>% left_join(GWDD.f %>% rename(Latin = Binomial),
                            by = "Latin") %>%
  group_by(is.na(WDm)) %>%
  summarise(sum(N))


df.tree.all.all <- df.tree.all %>%
  filter(!is.na(Quadrat)) %>%
  mutate(Date = as.Date(Date)) %>%
  dplyr::select(Latin,Quadrat,PX,PY,Date,StemID,DBH,Status,site,plot,year) %>%
  mutate(H = 54.01*(1-exp(-0.053*((DBH/10)**0.759)))) %>%
  # mutate(H = 39.092 - 36.852*exp(-0.023*DBH/10)) %>% # Kearsley
  left_join(GWDD.f %>% rename(Latin = Binomial),
            by = "Latin") %>%
  mutate(WDm = case_when(is.na(WDm) ~ mean(WDm,na.rm = TRUE),
                         TRUE ~ WDm)) %>%
  mutate(AGB = 0.0673*(WDm*((DBH/10)**2)*H)**0.976) %>%
  filter(DBH >= 100)

# hist(df.tree.all.all$WDm)


df.tree.all.wide <- df.tree.all.all %>%
  pivot_wider(values_from = c(DBH,Date,Status,H,AGB),
              names_from = year) %>%
  mutate(DBH_01 = case_when(!is.na(DBH_94) & !is.na(DBH_07) & is.na(DBH_01) ~ (DBH_94 + DBH_07)/2,
                            TRUE ~ as.double(DBH_01)),
         DBH_01 = case_when(DBH_01 < DBH_94 & DBH_01 < DBH_07 ~ (DBH_94 + DBH_07)/2,
                            DBH_01 > DBH_94 & DBH_01 > DBH_07 ~ (DBH_94 + DBH_07)/2,
                            TRUE ~ as.double(DBH_01))) %>%
  mutate(Status_01 = case_when(!is.na(DBH_94) & !is.na(DBH_07) & is.na(DBH_01) ~ "alive",
                               TRUE ~ Status_01)) %>%
  mutate(H_01 = 54.01*(1-exp(-0.053*((DBH_01/10)**0.759)))) %>%
  mutate(AGB_01 = 0.0673*(WDm*((DBH_01/10)**2)*H_01)**0.976)


df.tree.all.wide %>%
  group_by(site,plot) %>%
  summarise(AGB = sum(AGB_01,na.rm = TRUE)/(10*10000),
            .groups = "keep")


# ggplot(data = df.tree.all.wide) +
#   geom_point(aes(x = DBH_01/10, y = H_01)) +
#   # scale_x_log10() +
#   # scale_y_log10() +
#   theme_bw()


df.tree.sum <- df.tree.all.wide %>%
  filter(Status_01 == "alive") %>%
  group_by(site,plot) %>%
  summarise(AGB.init = sum(AGB_01, na.rm = TRUE)/
              (10*10000),
            AGBC = c2b.ratio*AGB.init,
            .groups = "keep")

mean(data.Lenda.mod.long.sum %>%
  filter(year == 2001) %>% pull(AGBC) /

  (data.Lenda.mod.long.sum %>%
     filter(year == 2001) %>% pull(AGBC) +
  (df.tree.sum %>% filter(site == "Lenda") %>% pull(AGBC)))*100)


################################################################################
# Paracou
source("http://chave.ups-tlse.fr/pantropical_allometry/readlayers.r")
library("raster")
library("ncdf4")

trees <- read.csv("/home/femeunier/Documents/data/paracou/Paracou_tree.csv") %>%
  dplyr::select(Plotnr,SOL,DBH2015)

# E <- retrieve_raster("E",cbind(-52.9,5.3))
E <- -0.09899134
trees.sum <- trees %>%
  group_by(Plotnr) %>%
  summarise(AGBCt = c2b.ratio*sum(exp(-1.803 - 0.976*E + 0.976*log(0.55) + 2.673*log(DBH2015) -
                            0.0299*(log(DBH2015)**2)),na.rm = TRUE)/
              (0.5*100*100),
            .groups = "keep")

lianas <- read.csv("/home/femeunier/Documents/data/paracou/Paracou_liana.csv") %>%
  filter(Liana.epiphyte == "L") %>%
  dplyr::select(Plotnr,DBH)

lianas.sum <- lianas %>%
  group_by(Plotnr) %>%
  summarise(N = length(DBH[DBH > 0])/(0.5),
            BA = pi*sum(DBH*DBH,na.rm = TRUE)/4/(0.5*100*100),
            AGBC = c2b.ratio*sum(exp(-0.968+2.657*log(DBH)),na.rm = TRUE)/(0.5*100*100),
            .groups = "keep")

summary(lianas.sum %>%
  left_join(trees.sum,
            by = "Plotnr") %>%
  mutate(ratio = AGBC/AGBCt*100) %>%
  pull(ratio))
