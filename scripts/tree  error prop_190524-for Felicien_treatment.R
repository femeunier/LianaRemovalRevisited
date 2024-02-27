# setwd("~/OneDrive - The University of Nottingham/1. Research/Data/Lianas - Gigantes/Gigante Liana Removal Data/Analyses")
# data <-  read.csv("Diam, ba, agb calcs based on dendros_220118.csv", header=T)
rm(list = ls())

library(boot)
library(pbapply)

data <- read.csv("/home/femeunier/Documents/projects/LVLRE.long/data/TREE data for felicien 220119.csv",
                 header = TRUE)

data <- data.frame(data)
data$Treatment <- ifelse(data$Treatment=="C",1,2)

lianas <- read.csv("/home/femeunier/Documents/projects/LVLRE.long//data/Liana data 220303.csv", header=T)

error_diam <- function(x, error){
  x + x*rnorm(1,0,error)
}

error_census <- function(x, error){ #input matrix of 3 columns original DBH1, original DBH2 and DBH_ep1
  GR_ep <- t(diff(rbind(x[1],x[2])))
  if(is.na(GR_ep)==F){
    y <- x[3]+(GR_ep[1]+abs(GR_ep[1])*rnorm(1,0,error))
  }
  else{
    y <- error_diam(x[2], 0.05)
  }
  y
}

yearly_diam <- function(x){
  DBH_ep2011 <- apply(data.frame(x$DBH1),1,error_diam, error=0.05)
  DBH_ep2012 <- apply(cbind(x$DBH1, x$DBH3, DBH_ep2011),1, error_census, error=0.03)
  DBH_ep2013 <- apply(cbind(x$DBH3, x$DBH5, DBH_ep2012),1, error_census, error=0.03)
  DBH_ep2014 <- apply(cbind(x$DBH5, x$DBH7, DBH_ep2013),1, error_census, error=0.03)
  DBH_ep2015 <- apply(cbind(x$DBH7, x$DBH9, DBH_ep2014),1, error_census, error=0.03)
  DBH_ep2016 <- apply(cbind(x$DBH9, x$DBH11, DBH_ep2015),1, error_census, error=0.03)
  DBH_ep2017 <- apply(cbind(x$DBH11, x$DBH13, DBH_ep2016),1, error_census, error=0.03)
  DBH_ep2018 <- apply(cbind(x$DBH13, x$DBH15, DBH_ep2017),1, error_census, error=0.03)
  DBH_ep2019 <- apply(cbind(x$DBH15, x$DBH17, DBH_ep2018),1,error_census, error=0.03)
  DBH_ep2020 <- apply(cbind(x$DBH17, x$DBH19, DBH_ep2019),1,error_census, error=0.03)
  DBH_ep2021 <- apply(cbind(x$DBH19, x$DBH21, DBH_ep2020),1,error_census, error=0.03)
  DBH <- cbind(x[2:12], DBH_ep2011, DBH_ep2012, DBH_ep2013, DBH_ep2014, DBH_ep2015, DBH_ep2016, DBH_ep2017, DBH_ep2018, DBH_ep2019, DBH_ep2020, DBH_ep2021)
}

AGB_func <- function(x, wd,treatment = 1){

  H <- (51.38*(1-exp(-0.01322*(x)^0.6465))) * abs(treatment - 2) +
    (2 + 51.38*(1-exp(-0.01322*(x)^0.6465)))*(treatment - 1)

  0.0673*(wd*((x/10)^2)*H)^0.976
}

MORT_func <- function(x){
  y <- is.na(x)
  y2 <- x[!y]
  tail(y2, n=1)
}

RECR_func <- function(x){
  if(is.na(x[1])==T & is.na(x[2])==F){
    recr <- x[2]
  }
  else{
    recr <- NA
  }
  recr
}

MORT_func <- function(x){
  if(is.na(x[1])==F & is.na(x[2])==T){
    mort <- x[1]
  }
  else{
    mort <- NA
  }
  mort
}

TOTALyear_func <- function(x, growth){
  if(growth=="T"){
    DIAM <- yearly_diam(x)
    #    CENSUS <- t(diff(t(cbind(x$Date_1, x$Date_3, x$Date_5, x$Date_7, x$Date_9, x$Date_11, x$Date_13, x$Date_15, x$Date_17, x$Date_19, x$Date_21))))
    AGB <- apply(cbind(DIAM[,12:22]), 2, AGB_func,
                 wd=DIAM$WD, treatment  = rep(x$Treatment),
                 timing)
    AGBC <- AGB*(DIAM$CC/100)
  }
  if(growth=="L"){
    DIAM <- apply(x[,seq(10,50,4)],c(1,2),error_diam, error=0.05)
    #    CENSUS <- t(diff(t(cbind(x$Date_1, x$Date_3, x$Date_5, x$Date_7, x$Date_9, x$Date_11, x$Date_13, x$Date_15, x$Date_17))))
    AGB <- exp(-0.968 + 2.657*log(DIAM/10))
    AGBC <- AGB*0.47
  }
  BGRC <- t(diff(t(AGBC)))
  RECRC <- cbind(apply(AGBC[,1:2],1,RECR_func),apply(AGBC[,2:3],1,RECR_func),
                 apply(AGBC[,3:4],1,RECR_func),apply(AGBC[,4:5],1,RECR_func),
                 apply(AGBC[,5:6],1,RECR_func),apply(AGBC[,6:7],1,RECR_func),
                 apply(AGBC[,7:8],1,RECR_func), apply(AGBC[,8:9],1,RECR_func),
                 apply(AGBC[,9:10],1,RECR_func), apply(AGBC[,10:11],1,RECR_func))
  MORTC <- cbind(apply(AGBC[,1:2],1,MORT_func),apply(AGBC[,2:3],1,MORT_func),
                 apply(AGBC[,3:4],1,MORT_func),apply(AGBC[,4:5],1,MORT_func),
                 apply(AGBC[,5:6],1,MORT_func),apply(AGBC[,6:7],1,MORT_func),
                 apply(AGBC[,7:8],1,MORT_func), apply(AGBC[,8:9],1,MORT_func),
                 apply(AGBC[,9:10],1,MORT_func), apply(AGBC[,10:11],1,MORT_func))
  if(growth=="T"){
    TOTALC <- cbind.data.frame(x$Treatment, x$Parcela, x$Placa, AGBC, BGRC, RECRC, MORTC, x$Date_1, x$Date_3, x$Date_5, x$Date_7, x$Date_9, x$Date_11, x$Date_13, x$Date_15, x$Date_17, x$Date_19, x$Date_21)
    col <- c("treat","plot","placa","AGBC2011","AGBC2012","AGBC2013","AGBC2014","AGBC2015","AGBC2016","AGBC2017","AGBC2018","AGBC2019", "AGBC2020", "AGBC2021",
             "BGRC2012","BGRC2013","BGRC2014","BGRC2015","BGRC2016","BGRC2017","BGRC2018","BGRC2019", "BGRC2020", "BGRC2021",
             "RECRC2012","RECRC2013","RECRC2014","RECRC2015","RECRC2016","RECRC2017","RECRC2018", "RECR2019", "RECR2020", "RECR2021",
             "MORTC2012","MORTC2013","MORTC2014","MORTC2015","MORTC2016","MORTC2017","MORTC2018", "MORTC2019", "MORTC2020", "MORTC2021",
             "Date2011","Date2012","Date2013","Date2014","Date2015","Date2016","Date2017","Date2018","Date2019", "Date2020", "Date2021")
    colnames(TOTALC) <- col
    TOTALC <- data.frame(TOTALC)
  }
  if(growth=="L"){
    TOTALC <- cbind.data.frame(1,x$plot, x$TagNr, AGBC, BGRC, RECRC, MORTC, x$Date_1, x$Date_3, x$Date_5, x$Date_7, x$Date_9, x$Date_11, x$Date_13, x$Date_15, x$Date_17, x$Date_19, x$Date_21)
    col <- c("treat","plot","placa","AGBC2011","AGBC2012","AGBC2013","AGBC2014","AGBC2015","AGBC2016","AGBC2017","AGBC2018","AGBC2019", "AGBC2020", "AGBC2021",
             "BGRC2012","BGRC2013","BGRC2014","BGRC2015","BGRC2016","BGRC2017","BGRC2018","BGRC2019", "BGRC2020", "BGRC2021",
             "RECRC2012","RECRC2013","RECRC2014","RECRC2015","RECRC2016","RECRC2017","RECRC2018", "RECR2019", "RECR2020", "RECR2021",
             "MORTC2012","MORTC2013","MORTC2014","MORTC2015","MORTC2016","MORTC2017","MORTC2018", "MORTC2019", "MORTC2020", "MORTC2021",
             "Date2011","Date2012","Date2013","Date2014","Date2015","Date2016","Date2017","Date2018","Date2019", "Date2020", "Date2021")
    colnames(TOTALC) <- col
    TOTALC <- data.frame(TOTALC)
  }
  TOTALC
}

PLOT_func <- function(x){
  dates <- apply(t(diff(t(cbind.data.frame(x$Date2011, x$Date2012, x$Date2013, x$Date2014, x$Date2015, x$Date2016, x$Date2017, x$Date2018, x$Date2019, x$Date2020, x$Date2021)))),2,mean,na.rm=T)
  plotC <- apply(x[,4:44],2,sum, na.rm=T)
  plotnetAGBC <- t(diff(plotC[1:11]))
  plotAGBCmgyear <- (plotnetAGBC/36*100/1000)/dates
  plotBGRCmgyear <- (plotC[12:21]/36*100/1000)/dates
  plotRECRCmgyear <- (plotC[22:31]/36*100/1000)/dates
  plotMORTCmgyear <- (plotC[32:41]/36*100/1000)/dates
  c(mean(x$plot,na.rm=T), mean(x$treat, na.rm=T), plotC[1:11]/36*100/1000, plotAGBCmgyear, plotBGRCmgyear, plotRECRCmgyear, plotMORTCmgyear)
}



tree_ep <- function(x, growth, ind){

  tot <- TOTALyear_func(x, growth)

  if(growth=="T"){
    tot_pl <- split(tot, tot$plot)
    all_tot <- t(sapply(split(tot, tot$plot),PLOT_func))
  }
  if(growth=="L"){
    all_tot <- t(sapply(split(tot, tot$plot),PLOT_func))
    all_tot <- rbind(c(all_tot[1,]), matrix(0, 2,length(all_tot[1,])), c(all_tot[2,]), matrix(0, 1,length(all_tot[1,])), c(all_tot[3,]), matrix(0, 1,length(all_tot[1,])), c(all_tot[4,]), matrix(0, 1,length(all_tot[1,])), c(all_tot[5,]), matrix(0, 1,length(all_tot[1,])), c(all_tot[6,]), c(all_tot[7,]), matrix(0, 2,length(all_tot[1,])), all_tot[8,])
  }
  all_tot
}

tree_median <- function(x,treat,ind){
  x <- cbind(x,treat)
  m1 <- mean(subset(x[ind,1], x[ind,2]==1), na.rm=T)
  m2 <- mean(subset(x[ind,1], x[ind,2]==2), na.rm=T)
  med1 <- median(subset(x[ind,1], x[ind,2]==1), na.rm=T)
  med2 <- median(subset(x[ind,1], x[ind,2]==2), na.rm=T)
  dm <- mean(subset(x[ind,1], x[ind,2]==2))-mean(subset(x[ind,1], x[ind,2]==1))
  dmed <- med2-med1
  perc <- dmed/med2
  c(m1, m2, med1, med2, dm, dmed, perc)
}

TREE_func <- function(x,ind){
  apply(x[ind,3:length(x[1,])],2,tree_median, treat=x[ind,2])
}

boot_ep_t <- function(x, R, ind){
  tree <- tree_ep(x, "T")
  boot(tree,TREE_func,R)$t
}

boot_ep_l <- function(y, R, ind){
  liana <- tree_ep(y, "L")
  boot(liana,TREE_func,R)$t
}

boot_ep_tl <- function(x, y, R, ind){
  tree <- tree_ep(x, "T")
  liana <- tree_ep(y, "L")
  tl <- cbind.data.frame(tree[,1:2], tree[,3:length(tree[1,])] +  liana[,3:length(liana[1,])])
  boot(tl,TREE_func,R)$t
}

NAMES_func <- function(x){
  row <- c("AGBC1","AGBC2","AGBC3","AGBC4","AGBC5","AGBC6","AGBC7","AGBC8","AGBC9", "AGBC10", "AGBC11",
           "dAGBC1","dAGBC2","dAGBC3","dAGBC4","dAGBC5","dAGBC6","dAGBC7","dAGBC8", "dAGBC9", "dAGBC10",
           "BGRC1","BGRC2","BGRC3","BGRC4","BGRC5","BGRC6","BGRC7","BGRC8", "BGRC9", "BGRC10",
           "RECRC1","RECRC2","RECRC3","RECRC4","RECRC5","RECRC6", "RECRC7", "RECRC8", "RECRC9", "RECRC10",
           "MORTC1","MORTC2","MORTC3","MORTC4","MORTC5","MORTC6","MORTC7", "MORTC8", "MORTC9", "MORTC10")
  col <- c("lo_95","lo_90","med","hi_90","hi_95")
  rownames(x) <- row
  colnames(x) <- col
  x
}

# ######LIANAS#####
# N=1000
# plottot_epl <- pbreplicate(N,boot_ep_l(lianas, 100))
# liana_confint <- t(apply(plottot_epl, 2, quantile, probs=c(0.025,0.05,0.5,0.95,0.975), na.rm=T))
# y <- seq(1,length(liana_confint[,1]),7)
# meanl_con <- NAMES_func(liana_confint[y,])
# medianl_con <- NAMES_func(liana_confint[(y+2),])

# setwd("~/OneDrive - The University of Nottingham/1. Research/Data/Lianas - Gigantes/Gigante Liana Removal Data/Analyses")
# write.table(meanl_con, "BCa conf int L mean_con 220303.txt")
# write.table(medianl_con, "BCa conf int L median_con 220303.txt")

#####TREES#####
N=25
plottot_ept <- pbreplicate(N,boot_ep_t(data,25))
tree_confint <- t(apply(plottot_ept, 2, quantile, probs=c(0.025,0.05,0.5,0.95,0.975), na.rm=T))
y <- seq(1,length(tree_confint[,1]),7)
meant_con <- NAMES_func(tree_confint[y,])
meant_rem <- NAMES_func(tree_confint[(y+1),])
mediant_con <- NAMES_func(tree_confint[(y+2),])
mediant_rem <- NAMES_func(tree_confint[(y+3),])
difft_mean <- NAMES_func(tree_confint[(y+4),])
difft_median <- NAMES_func(tree_confint[(y+5),])
perct <- NAMES_func(tree_confint[(y+6),])


matplot(perct[12:17,c(1,3,5)],type = "l",lty = 1)
abline(h = 0)


matplot(difft_median[12:22,c(1,3,5)],type = "l",lty = 1)
abline(h = 0)

# setwd("~/OneDrive - The University of Nottingham/1. Research/Data/Lianas - Gigantes/Gigante Liana Removal Data/Analyses")
#setwd("D:/OneDrive - The University of Nottingham/1. Research/Data/Lianas - Gigantes/Gigante Liana Removal Data/Analyses")
# write.table(meant_con, "BCa conf int T mean_con 220303.txt")
# write.table(meant_rem, "BCa conf int T mean_rem 220303.txt")
# write.table(mediant_con, "BCa conf int T median_con 220303.txt")
# write.table(mediant_rem, "BCa conf int T median_rem 220303.txt")
# write.table(difft_mean, "BCa conf int T diff_mean 220303.txt")
# write.table(difft_median, "BCa conf int T diff_median 220303.txt")
# write.table(perct, "BCa conf int T perc 220303.txt")

# #####LIANAS + TREES#####
# N=1000
# plottot_eptl <- pbreplicate(N,boot_ep_tl(data, lianas, 100))
# tl_confint <- t(apply(plottot_eptl, 2, quantile, probs=c(0.025,0.05,0.5,0.95,0.975), na.rm=T))
# y <- seq(1,length(tl_confint[,1]),7)
# meantl_con <- NAMES_func(tl_confint[y,])
# meantl_rem <- NAMES_func(tl_confint[(y+1),])
# mediantl_con <- NAMES_func(tl_confint[(y+2),])
# mediantl_rem <- NAMES_func(tl_confint[(y+3),])
# difftl_mean <- NAMES_func(tl_confint[(y+4),])
# difftl_median <- NAMES_func(tl_confint[(y+5),])
# perctl <- NAMES_func(tl_confint[(y+6),])
#
# matplot(mediantl_con[12:21,c(1,3,5)],type = "l", col = "red",ylim = c(0,5))
# matlines(mediantl_rem[12:21,c(1,3,5)],type = "l", col = "black")
# abline(h = 0)

# setwd("~/OneDrive - The University of Nottingham/1. Research/Data/Lianas - Gigantes/Gigante Liana Removal Data/Analyses")
# write.table(meantl_con, "BCa conf int T&L mean_con 220303.txt")
# write.table(meantl_rem, "BCa conf int T&L mean_rem 220303.txt")
# write.table(mediantl_con, "BCa conf int T&L median_con 220303.txt")
# write.table(mediantl_rem, "BCa conf int T&L median_rem 220303.txt")
# write.table(difftl_mean, "BCa conf int T&L diff_mean 220303.txt")
# write.table(difftl_median, "BCa conf int T&L diff_median 220303.txt")
# write.table(perctl, "BCa conf int T&L perc 230202.txt")

# saveRDS(plottot_epl,file = "./outputs/plottot_epl_mod.RDS")
saveRDS(plottot_ept,file = "./outputs/plottot_ept_mod.RDS")
# saveRDS(plottot_eptl,file = "./outputs/plottot_eptl_mod.RDS")

################################################################################

plottot_ept_ref <- readRDS(file = "./outputs/plottot_ept_ref.RDS")
plottot_ept_mod <- readRDS(file = "./outputs/plottot_ept_mod.RDS")

tree_confint_ref <- t(apply(plottot_ept_ref, 2, quantile, probs=c(0.025,0.05,0.5,0.95,0.975), na.rm=T))
tree_confint_mod <- t(apply(plottot_ept_mod, 2, quantile, probs=c(0.025,0.05,0.5,0.95,0.975), na.rm=T))

mediant_con_ref <- NAMES_func(tree_confint_ref[(y+2),])
mediant_rem_ref <- NAMES_func(tree_confint_ref[(y+3),])

mediant_con_mod <- NAMES_func(tree_confint_mod[(y+2),])
mediant_rem_mod <- NAMES_func(tree_confint_mod[(y+3),])

matplot(mediant_con_ref[1:11,c(3)],type = "l",lty = 1)
matlines(mediant_con_mod[1:11,c(3)],type = "l",lty = 2)

matplot(mediant_rem_ref[1:11,c(3)],type = "l",lty = 1,
        ylim = c(70,120))
matlines(mediant_rem_mod[1:11,c(3)],type = "l",lty = 2)


perct.ref <- NAMES_func(tree_confint_ref[(y+6),])
perct.mod <- NAMES_func(tree_confint_mod[(y+6),])


matplot(perct.ref[12:22,c(1,3,5)],type = "l",lty = 1,ylim = c(-1,4))
abline(h = 0, lty = 3)
matlines(perct.mod[12:22,c(1,3,5)],type = "l",lty = 2)

