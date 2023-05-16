
rm(list=ls())

library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)

tree <- read.table("/home/femeunier/Downloads/Tree_data_liana_removal_experiment.txt", header=T)
liana <- read.table("/home/femeunier/Downloads//Liana_data_liana_removal_experiment.txt", header=T)


#Census period length for each year of experiment (in years)
census_l <- c(1.279,0.986, 0.999)


c_index <- filter(tree,Treatment == 'C') %>% group_by(Parcela) %>%
  filter(!duplicated(Parcela)) %>% pull(.,Parcela)
r_index <- filter(tree,Treatment == 'R') %>% group_by(Parcela) %>%
  filter(!duplicated(Parcela)) %>% pull(.,Parcela)

allo_tree_r <- function(dbh){
  return(403.7567*(1 - exp(-0.0068267*(dbh**0.35947))))
}

allo_tree_c <- function(dbh){
  return(57.5826*(1 - exp(-0.030973*(dbh**0.45295))))
}

agb_tree <- function(CC,Wd,dbh,treeh){
  return(CC*0.01*0.0673*(Wd*treeh*dbh*dbh)**0.976)
}

##########################################################################################
#the PNAS parameter
##########################################################################################

# original tree AGBC
#
tree_origin <- matrix(0,16,5)
for(i in 1:16){
  plot <- subset(tree, tree$Parcela==i)
  agb_2011 <- sum(plot$AGBC_2011, na.rm=TRUE)
  agb_2012 <- sum(plot$AGBC_2012, na.rm=TRUE)
  agb_2013 <- sum(plot$AGBC_2013, na.rm=TRUE)
  agb_2014 <- sum(plot$AGBC_2014, na.rm=TRUE)
  agb_plot <- i
  tree_origin[i,] <- cbind(agb_plot,agb_2011, agb_2012, agb_2013, agb_2014)
}
col <- c("plot", "agb_2011", "agb_2012", "agb_2013", "agb_2014")
colnames(tree_origin) <- col

# original liana AGBC
#
liana_origin <- matrix(0,8,5)
for (i in 1:8){
  plot <- subset(liana,liana$Parcela ==c_index[i])
  agb_2011 <- sum(plot$AGBC_2011,na.rm=TRUE)
  agb_2012 <- sum(plot$AGBC_2012,na.rm=TRUE)
  agb_2013 <- sum(plot$AGBC_2013,na.rm=TRUE)
  agb_2014 <- sum(plot$AGBC_2014,na.rm=TRUE)
  agb_plot <- c_index[i]
  liana_origin[i,] <- cbind(agb_plot,agb_2011,agb_2012,agb_2013,agb_2014)
}
col <- c("plot", "agb_2011", "agb_2012", "agb_2013", "agb_2014")
colnames(liana_origin) <- col


#biomass increment per treatment per year
#########################################

#tree biomass increment per plot
agbTinc_ori <- matrix(0,16,4)
for(i in 1:16){
  agbTinc_ori[i,1] <- tree_origin[i,1]
  agbTinc_ori[i,2] <- (tree_origin[i,3]-tree_origin[i,2])/census_l[1] #Biomass increase in year 1 corrected for census period
  agbTinc_ori[i,3] <- (tree_origin[i,4]-tree_origin[i,3])/census_l[2] #Biomass increase in year 2 corrected for census period
  agbTinc_ori[i,4] <- (tree_origin[i,5]-tree_origin[i,4])/census_l[3] #Biomass increase in year 3 corrected for census period
}
col <- c("plot", "agbinc_2012", "agbinc_2013", "agbinc_2014")
colnames(agbTinc_ori) <- col

# liana biomass increment for control plot
#
agbLinc_ori <- matrix(0,8,4)
for (i in 1:8){
  agbLinc_ori[i,1] <- liana_origin[i,1]
  agbLinc_ori[i,2] <- (liana_origin[i,3]-liana_origin[i,2])/census_l[1] #Biomass increase in year 1 corrected for census period
  agbLinc_ori[i,3] <- (liana_origin[i,4]-liana_origin[i,3])/census_l[2] #Biomass increase in year 2 corrected for census period
  agbLinc_ori[i,4] <- (liana_origin[i,5]-liana_origin[i,4])/census_l[3] #Biomass increase in year 3 corrected for census period
}
col <- c("plot", "agbinc_2012", "agbinc_2013", "agbinc_2014")
colnames(agbLinc_ori) <- col

# biomass increment for control plot
agbinc_ori <- (agbLinc_ori+agbTinc_ori[c_index,]) %>% data.frame(.) %>% select(-c(1))

#median biomass increase per treatment
#
medianinc_ori <- matrix(0,2,3)
medianinc_ori[1,] <- agbinc_ori %>% apply(X=.,2,FUN=median)
medianinc_ori[2,] <- agbTinc_ori[r_index,2:4] %>% apply(X=.,2,FUN=median)
col <- c("2011-2012","2012-2013","2013-2014")
colnames(medianinc_ori) <- col
row <- c("agbinc_control","agbinc_removal")
rownames(medianinc_ori) <- row

###############################################################################
# updated result
###############################################################################



##updated tree AGB in control plot
#
agbT_newC <- matrix(0,8,5)
agbT_newR <- matrix(0,8,5)
for(i in 1:8){
  for (j in 1:4){
    dbh_loop <- subset(tree,tree$Parcela == c_index[i],select=c(CC,WD,j+5)) %>% drop_na(.)
    height_loop <- allo_tree_c(dbh=dbh_loop[,3])
    agbT_newC[i,j+1] <- agb_tree(CC=dbh_loop$CC,Wd=dbh_loop$WD,dbh=dbh_loop[,3]/10,treeh=height_loop) %>% sum(.)
  }
  agbT_newC[i,1] <-c_index[i]
}
agbT_newC <- (agbT_newC[,2:5] + liana_origin[,2:5]) %>% cbind(agbT_newC[,1],.)
col <- c("plot", "agb_2011", "agb_2012", "agb_2013", "agb_2014")
colnames(agbT_newC) <- col



# updated tree AGB in Removal plot
#
for(i in 1:8){
  for (j in 1:4){
    dbh_loop <- subset(tree,tree$Parcela == r_index[i],select=c(CC,WD,j+5)) %>% drop_na(.)
    height_loop <- allo_tree_r(dbh=dbh_loop[,3])
    agbT_newR[i,j+1] <- agb_tree(CC=dbh_loop$CC,Wd=dbh_loop$WD,dbh=dbh_loop[,3]/10,treeh=height_loop) %>% sum(.)
  }
  agbT_newR[i,1] <-r_index[i]
}
col <- c("plot", "agb_2011", "agb_2012", "agb_2013", "agb_2014")
colnames(agbT_newR) <- col

# meidian increase of updated control and removal
#########################################
medianinc_new <- matrix(0,2,3)
for(i in 1:3){
  medianinc_new[1,i] <- median((agbT_newC[,i+2]-agbT_newC[,i+1])/census_l[i])
  medianinc_new[2,i] <- median((agbT_newR[,i+2]-agbT_newR[,i+1])/census_l[i])
}
col <- c("2011-2012","2012-2013","2013-2014")
colnames(medianinc_new) <- col
row <- c("agbinc_control","agbinc_removal")
rownames(medianinc_new) <- row

## AGB increase difference
percen_new <- (medianinc_new[2,]-medianinc_new[1,])/medianinc_new[2,]
percen_ori <- (medianinc_ori[2,]-medianinc_ori[1,])/medianinc_ori[2,]



out2 <- data.frame(percen_ori,percen_new)%>%
  tibble::rownames_to_column(var = "Year") %>%
  pivot_longer(-Year, names_to = "Treatment", values_to = "Perinc")
ggplot(data=out2, mapping = aes (x=Year,y=Perinc,fill=Treatment)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(palette = "Pastel2")

