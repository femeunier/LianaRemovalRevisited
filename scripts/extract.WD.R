rm(list = ls())

library(qgraph)
library(ggridges)
library(weights)
library(BIOMASS)
library(dplyr)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(FactoMineR)
library(factoextra)
library(dplyr)
library(corrplot)

site.loc <- readRDS("./outputs/site.loc.RDS") %>%
  dplyr::select(site.common,site.group,
                ForestElevationName,Forest.status.plotview) %>%
  distinct()

# Load the data
all.df <- (readRDS(  "./outputs/All.COI.data.RDS") %>%
                      mutate(sp = str_squish(sp)) %>%
                      filter(dbh >= 10)) %>%
  mutate(genus = stringr::word(sp,1),
         species = word(sp, 2, str_count(sp, '\\s')+1)) %>%
  left_join(site.loc %>%
              rename(site = site.common) %>%
              distinct(),
            by = "site")


all.df %>%
  group_by(site,sp,liana.cat) %>%
  summarise(N = n(),
            .groups = "keep") %>%
  arrange(desc(N)) %>%
  filter(N > 50)

WD.df <- (BIOMASS::getWoodDensity(all.df$genus,all.df$species))
WD.df$meanWD[WD.df$levelWD == "dataset"] <- NA


all.df$wd <- WD.df$meanWD
hist(all.df$wd)
summary(all.df$wd)
quantile(all.df$wd,c(1/3,2/3),na.rm = TRUE)


ggplot(data = all.df %>%
         filter(ForestElevationName == "Lowland",
                Forest.status.plotview %in% c("Old-growth","Mature")),
       aes(x = wd, y = site.group,fill = site.group)) +
  stat_density_ridges(alpha = 0.5,
                      quantile_lines = TRUE, quantiles = 2) +
  theme_bw()

ggplot(data = all.df %>%
         filter(ForestElevationName == "Lowland",
                Forest.status.plotview %in% c("Old-growth","Mature")) %>%
         group_by(site.group,site) %>%
         summarise(wd = mean(wd,na.rm = TRUE)),
       aes(x = wd, y = site.group,fill = site.group)) +
  geom_density_ridges(alpha = 0.5, color = NA) +
  theme_bw()

all.df <- all.df %>%
  mutate(wd.cat = factor(case_when(wd < 0.577 ~ "low",
                                   wd < 0.697 ~ "average",
                                   TRUE ~ "high"),
                         levels = c("very low","low","average","high","very high")))

all.df %>%
  filter(h < 3) %>%
  pull(site) %>%
  unique()

ggplot(data = all.df %>%
         filter(!is.na(wd),
                site == "Gigante") ,
       aes(x = dbh, y = h, color = wd.cat)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm", se = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

all.df %>%
  filter(!is.na(wd)) %>%
  group_by(site) %>%
  summarise(N = n()) %>%
  arrange(desc(N))

ggplot(data = all.df %>%
         filter(!is.na(wd)) %>%
         filter(site == "Gigante"),
       aes(x = h, y = wd.cat,  fill = wd.cat)) +
  # geom_density_ridges(alpha = 0.5) +
  stat_density_ridges(alpha = 0.5,
                      quantile_lines = TRUE, quantiles = 2) +
  theme_bw()

ggplot(data = all.df %>%
         filter(!is.na(wd)),
       aes(x = wd, y = liana.cat, fill = liana.cat)) +
  # geom_density_ridges(alpha = 0.5) +
  stat_density_ridges(alpha = 0.5,
                      quantile_lines = TRUE, quantiles = 2) +
  theme_bw()


all.df.sum <- all.df %>%
  group_by(site) %>%
  summarise(H.m = mean(h,na.rm = TRUE),
            H.wm = weighted.mean(h,w = dbh**2,
                                  na.rm = TRUE),

            coi.m = mean(coi,na.rm = TRUE),
            coi.wm = weighted.mean(coi,w = dbh**2,
                                   na.rm = TRUE),

            WD.m = mean(wd,na.rm = TRUE),
            WD.wm = weighted.mean(wd,w = dbh**2,
                                  na.rm = TRUE),
            frac.high = length(which(liana.cat == "high"))/length(wd),
            frac.WD = 1- length(which(is.na(wd)))/length(wd),
            .groups = "keep")

saveRDS(all.df.sum %>%
          dplyr::select(-c("frac.WD")),
        "./outputs/WD.sum.RDS")

df <- readRDS("./outputs/DeltaH.vs.climate.RDS") %>%
  filter(target == 50) %>%
  dplyr::select(-any_of(c("WD.m","WD.wm","H.m","H.wm"))) %>%
  left_join(all.df.sum,
            by = "site")

cols<-brewer.pal(length(unique(df$continent)),"Dark2")

cdf <- df %>%
  filter(ForestElevationName == "Lowland",
         Forest.status.plotview %in% c("Old-growth","Mature"))

df2plot <- df %>%
  mutate(type = case_when(ForestElevationName == "Lowland" &
                          Forest.status.plotview %in% c("Old-growth") ~ "1",
                          TRUE ~ "2"))

ggplot(data = cdf,
       aes(x = WD.wm, y = m_high, color = continent)) +
  geom_point() +
  scale_color_manual(values = cols) +
  stat_smooth(method = "lm", se = FALSE) +
  stat_smooth(color = "black",fill = "lightgrey",
              method = "lm", se = TRUE) +
  theme_bw()

summary(lm(data = cdf,
           formula = m_high ~ srad,
           weights = sqrt(Nno*Nhigh)))


ggplot(data = cdf,
       aes(x = H.wm, y = m_high, color = continent)) +
  geom_point() +
  scale_color_manual(values = cols) +
  stat_smooth(method = "lm", se = FALSE) +
  stat_smooth(color = "black",fill = "lightgrey",
              method = "lm", se = TRUE) +
  theme_bw()

summary(lm(data = cdf,
           formula = m_high ~ H.wm,
           weights = sqrt(Nno*Nhigh)))

ggplot(data = cdf,
       aes(x = WD.wm,
           y = m_high,color = continent, fill = continent)) +
  geom_errorbar(data = df2plot,
                aes(ymin = Qlow_high, ymax = Qhigh_high),
                width = 0) +
  geom_point(data = df2plot,
             aes(shape = type,
                 size = sqrt(Nno*Nhigh))) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  stat_smooth(color = "black",fill = "darkgrey",
              method = "lm") +
  stat_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  guides(size = "none", fill = "none")+
  labs(x = "Mean wood density", y = "relative liana effect on height", fill = "",
       color = "") +
  scale_y_continuous(limits = c(-0.35,0.2)) +
  scale_shape_manual(values = c(16,1))

summary(lm(data = cdf,
           formula = m_high ~ WD.wm,
           weights = sqrt(Nno*Nhigh)))

cdf %>%
  group_by(continent) %>%
  summarise(r2 = summary(lm(formula = m_high ~WD.wm))[["adj.r.squared"]],
            pval = summary(lm(formula = m_high ~WD.wm))[["coefficients"]][2,"Pr(>|t|)"])

hist(cdf$frac.WD) ; summary(cdf$frac.WD)

ggplot(data = cdf,
       aes(x = srad,y = WD.wm,
           color = continent, fill = continent,
           size = sqrt(Nno*Nhigh))) +
  geom_point() +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  stat_smooth(color = "black",fill = "darkgrey",
              method = "lm") +
  stat_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  guides(size = "none", fill = "none")+
  labs(y = "Mean wood density", x = "Incoming radiation", fill = "",
       color = "")

summary(lm(data = cdf,
           formula = WD.wm ~ srad,
           weights = sqrt(Nlarge)))

df.PCA <- cdf %>%
  dplyr::select(AI,m_high,
                m.abs_high,WD.m,
                MAP,MAT,Prec.sd,MCWD,t.sd,VPD,VPD.sd,srad,srad.sd,
                H.no_high,H.wm,H.m,
                WD.wm,frac.high)

res.pca <- PCA(df.PCA, scale.unit = TRUE, ncp = 5, graph = FALSE,
               row.w = sqrt(cdf$Nhigh*cdf$Nno))
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

fviz_pca_biplot(res.pca,
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = cdf$continent,
                addEllipses = TRUE,ellipse.type = "confidence",
                col.ind = "black",
                col.var = "black",
                # Color variable by groups
                # col.var = factor(c("sepal", "sepal", "petal", "petal")),

                legend.title = list(fill = "Continent", color = "Clusters"),
                repel = TRUE        # Avoid label overplotting
)

cdf.ext <- cbind(cdf,
                 res.pca$ind$coord[,c(1,2)])

cdf.ext %>%
  filter(continent == "Amazon",Dim.1<0)

################################################################################

df.PCA_nolianaeffect <- cdf %>%
  dplyr::select(AI,
                # m.abs_high,WD.m,
                MAP,MAT,Prec.sd,MCWD,t.sd,VPD,VPD.sd,srad,srad.sd,
                H.no_high,H.wm,H.m,
                WD.wm,frac.high)

res.pca_nolianaeffect <- PCA(df.PCA_nolianaeffect, scale.unit = TRUE,
                             ncp = 5, graph = TRUE,
                             row.w = sqrt(cdf$Nhigh*cdf$Nno))



df.all <- cbind(cdf,
                res.pca_nolianaeffect$ind$coord[,c(1:2)])



ggplot(data = df.all,
       aes(x = Dim.1,
           y = m_high,
           color = continent, fill = continent,
           size = sqrt(Nno*Nhigh))) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  stat_smooth(color = "black",fill = "darkgrey",
              method = "lm") +
  stat_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  guides(size = "none", fill = "none") +
  theme(legend.position = c(0.15,0.8))

summary(lm(data = df.all,
           formula = m_high ~ Dim.1 + Dim.2,
           weights = sqrt(Nno*Nhigh)))

var <- get_pca_var(res.pca_nolianaeffect)

corrplot(var$contrib, is.corr=FALSE)
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)


#use cor function to create a correlation matrix with milan.mort dataset
#and put into cormat variable
###################################################
cormat=wtd.cor(df.PCA,
               weight=sqrt(cdf$Nno*cdf$Nhigh))  #correlation matrix generated
ccormat <- cormat[["correlation"]]
# diag(ccormat) <- NA
###################################################
###################################################
#now plot a graph of the correlation matrix
###################################################

qgraph(ccormat,
       shape="circle",
       posCol="darkgreen",
       negCol="darkred",
       layout="groups",
       minimum = 0.3,
       graph = "cor",
       vsize=10)

library(piecewiseSEM)

modelList <- psem(
  lm(WD.wm ~ srad + MAT, df.all),
  lm(H.m ~ VPD.sd + MCWD, df.all),
  lm(m_high ~ WD.wm + H.m, df.all),
  df.all
)

summary(modelList)
