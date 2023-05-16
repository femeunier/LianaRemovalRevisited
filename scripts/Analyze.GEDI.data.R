rm(list = ls())

# Assuming the files have been downloaded
# loading rGEDI package
library(rGEDI)
library(dplyr)
library(sp)
library(leaflet)
library(leafsync)
library(tidyr)
library(ggplot2)


# Figure 1 https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2745.13155
site.name <- "Gigante"
ul_lat <- 9.125
lr_lat <- 9.105
ul_lon <- -79.86
lr_lon <- -79.845

# Get path to GEDI data
gLevel1B<-gedifinder(product="GEDI01_B",ul_lat, ul_lon, lr_lat, lr_lon,version="002")
gLevel2A<-gedifinder(product="GEDI02_A",ul_lat, ul_lon, lr_lat, lr_lon,version="002")
gLevel2B<-gedifinder(product="GEDI02_B",ul_lat, ul_lon, lr_lat, lr_lon,version="002")

# Set output dir for downloading the files
outdir=file.path(getwd(),"data","GEDI",site.name)

data.loc.plots <- read.csv(file.path(getwd(),"data","location_plots.csv")) %>%
  mutate(Treatment = case_when(Plot %in% c(1,4,6,10,8,12,13,16) ~ "Control",
                               TRUE ~ "Removal")) %>%
  mutate(Col = case_when(Treatment == "Control" ~ "#000000",
                         TRUE ~ "#FFFFFF"))

level1bGeo <- data.frame()
level1bGeo_select <- data.frame()
df.rh <- data.frame()

for (ifile in seq(1,length(gLevel1B))){

  file2read1b <- file.path(outdir,paste0(sub('\\.h5$','',basename(gLevel1B[ifile])),"_sub.h5"))
  file2read2a <- file.path(outdir,paste0(sub('\\.h5$','',basename(gLevel2A[ifile])),"_sub.h5"))

  if (file.exists(file2read1b) & file.exists(file2read2a)){
    gedilevel1b <- readLevel1B(level1Bpath = file2read1b)
    gedilevel2a <- readLevel2A(level2Apath = file2read2a)
  } else {
    next()
  }

  level1b <- getLevel1BGeo(level1b=gedilevel1b,select=c("elevation_bin0","elevation_lastbin","beam"))
  level1bGeo <- bind_rows(list(level1bGeo,
                               level1b %>% mutate(file = ifile)))

  # debugonce(getLevel2AM)
  level2AM <- getLevel2AM(gedilevel2a)

  for (iplot in seq(1,length(unique(data.loc.plots$Plot)))){
    cplot <- data.loc.plots %>% filter(Plot == iplot)

    clevel1b <- level1b %>% filter(latitude_bin0 >= cplot %>% filter(Corner == "ll") %>% pull(Y),
                                   latitude_bin0 <= cplot %>% filter(Corner == "ur") %>% pull(Y),
                                   longitude_bin0 >= cplot %>% filter(Corner == "ll") %>% pull(X),
                                   longitude_bin0 <= cplot %>% filter(Corner == "ur") %>% pull(X)) %>%
      mutate(Treatment = cplot$Treatment[1],
             plot = iplot,
             file = ifile,
             Col = cplot$Col[1])

    level1bGeo_select <- bind_rows(list(level1bGeo_select,
                                        clevel1b))


    df.rh <- bind_rows(list(df.rh,
                            level2AM %>%
                              filter(shot_number %in% clevel1b$shot_number) %>%
                              dplyr::select(c("beam","shot_number","quality_flag","elev_highestreturn","elev_lowestmode","rh50","rh95","rh100")) %>%
                              mutate(plot = iplot,
                                     Treatment = cplot$Treatment[1]),
                            level2AM %>%
                              filter(!(shot_number %in% clevel1b$shot_number)) %>%
                              dplyr::select(c("beam","shot_number","quality_flag","elev_highestreturn","elev_lowestmode","rh50","rh95","rh100")) %>%
                              mutate(plot = 0,
                                     Treatment = "Outside")))

  }

  # wf <- getLevel1BWF(gedilevel1b, shot_number= c(level1bGeo_select$shot_number[1]))
  # plotWFMetrics(gedilevel1b, gedilevel2a, level1bGeo_select$shot_number[1], rh=c(5, 50, 95))


  level1bGeo_spdf<-SpatialPointsDataFrame(cbind(level1bGeo$longitude_bin0, level1bGeo$latitude_bin0),
                                          data=level1bGeo)


}

pal <- colorNumeric(
  palette = colorRampPalette(c('red', 'green'))(length(level1bGeo$file)),
  domain = level1bGeo$file)

leaflet(level1bGeo_select) %>%
  addCircleMarkers(data = level1bGeo,
                   ~longitude_bin0,
                   ~latitude_bin0,
                   radius = 1,
                   opacity = 1,
                   fillOpacity = 1,
                   color =  "red")  %>%
  addCircleMarkers(~longitude_bin0,
                   ~latitude_bin0,
                   radius = 1,
                   opacity = 1,
                   fillOpacity = 1,
                   color =  ~Col)  %>%
  addRectangles(
    lng1 = data.loc.plots %>% filter(Corner == "ll") %>% pull(X),
    lat1 = data.loc.plots %>% filter(Corner == "ll") %>% pull(Y),
    lng2 = data.loc.plots %>% filter(Corner == "ur") %>% pull(X),
    lat2 = data.loc.plots %>% filter(Corner == "ur") %>% pull(Y),
    fillColor = "transparent",
    color = data.loc.plots %>% filter(Corner == "ur") %>% pull(Col)
  ) %>%
  addScaleBar(options = list(imperial = FALSE)) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addLegend(colors = c(unique(data.loc.plots$Col),"red"),
            labels = c("Control","Removal","Outside"),
            title = "Treatment")

ggplot(data = df.rh %>% filter(quality_flag>0),
       aes(x = Treatment, y = rh95, color = Treatment)) +
  # geom_jitter(position=position_jitter(), cex=0.2, alpha = 0.4, color = "black") +
  geom_boxplot(fill = NA,outlier.shape = NA) +
  theme_bw() +
  guides(color = FALSE)


df.rh %>% filter(quality_flag>0,
                 Treatment %in% c("Control","Removal")) %>% arrange(Treatment)
