rm(list = ls())

library(raster)

# DEM
str_name<-'/home/femeunier/Downloads/CentralPanama_dem5m/CentralPanama_dem5m.tif'

e <- extent(620000,635000,1000000,1015000)
imported_raster= crop(raster(str_name),e)

plot(imported_raster)
# crs(imported_raster) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"

r <-projectRaster(from = imported_raster,
                  crs=crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

plot(r)

# DSM
str_name2<-'/home/femeunier/Downloads/CentralPanama_dem5m/mdt_bcnm.tif'
imported_raster2= crop(raster(str_name2),e)
r2 <-projectRaster(from = imported_raster2,
                  crs=crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

r2_resam <- resample(r2,r,method='bilinear')

plot(r2_resam - r)
rect(-79.852753,9.152909,-79.846058,9.156129, border = "red") # BCI

hist(as.vector(crop(r2_resam - r,extent(-79.852753,-79.846058,9.152909,9.156129))))
summary(as.vector(crop(r2_resam - r,extent(-79.852753,-79.846058,9.152909,9.156129))))

data.loc.plots <- read.csv(file.path(getwd(),"data","location_plots.csv")) %>%
  mutate(Treatment = case_when(Plot %in% c(1,4,6,10,8,12,13,16) ~ "Control",
                               TRUE ~ "Removal")) %>%
  mutate(Col = case_when(Treatment == "Control" ~ "#000000",
                         TRUE ~ "#FFFFFF"))

df.h <- data.frame()
for (iplot in seq(1,length(unique(data.loc.plots$Plot)))){
  cplot <- data.loc.plots %>% filter(Plot == iplot)
  xmin <- cplot %>% filter(Corner == "ll") %>% pull(X)
  xmax <- cplot %>% filter(Corner == "ur") %>% pull(X)
  ymin <- cplot %>% filter(Corner == "ll") %>% pull(Y)
  ymax <- cplot %>% filter(Corner == "ur") %>% pull(Y)
  e <- extent(xmin,xmax,ymin,ymax)
  ch <- as.vector(crop(r2_resam - r,e))

  df.h <- bind_rows(list(df.h,
                         data.frame(plot = iplot,
                                    treatment = cplot$Treatment[1],
                                    h = ch)))

  rect(xmin,ymin,xmax,ymax,border = cplot$Col[1])

}

ggplot(data = df.h) +
  geom_boxplot(aes(x = treatment, y = h)) +
  theme_bw()

summary(lm(data = df.h, formula = h ~ treatment))
