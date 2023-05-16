rm(list = ls())

library(dplyr)
library(LianaBCI)
library(data.table)

spatial.resolutions = c(0.5,1,2)
patch.size = 60

##############################################################################
# Load data

plots <- c(5,7,8,11,13,16)
years <- c(2016,2019)

dir <- "/data/gent/vo/000/gvo00074/felicien/Gigante/"

for (iplot in seq(1,length(plots))){
  print(iplot)

  rm(list = setdiff(ls(), c("spatial.resolutions","patch.size","plots","years","dir","iplot")))

  for (iyear in seq(1,length(years))){
    print(years[iyear])

    df.SP <- df.chm <- data.frame()

    SP <- read.csv(file.path(dir,"scan_position",paste0("SP_",years[iyear],"_plot",sprintf("%02d",plots[iplot]),".txt")),header = FALSE)  %>%
      rename(X = V1,
             Y = V2,
             Z = V3)

    df.SP <- bind_rows(list(df.SP,
                            SP %>% mutate(plot = plots[iplot],
                                          year = years[iyear])))

    plot.file <- file.path(dir,paste0("plot",sprintf("%02d",plots[iplot]),"_",years[iyear],".txt"))
    data <- fread(plot.file, header = T)

    colnames(data) <- c("X","Y","Z","C2M")

    min.X <- min(data$X,na.rm = TRUE)
    min.Y <- min(data$Y,na.rm = TRUE)

    df.SP <- df.SP %>% mutate(X.rel = X - min.X,
                              Y.rel = Y - min.Y)

    for (ispatial in seq(1,length(spatial.resolutions))){
      spatial.resolution <- spatial.resolutions[ispatial]

      Npatch_X = patch.size/spatial.resolution
      Npatch_Y = patch.size/spatial.resolution
      Npatch = Npatch_X*Npatch_Y


      pc <- data %>%
        rename(Zabs = C2M) %>%
        filter(!is.na(X) & !is.na(Y)) %>%
        mutate(X.rel = X - min.X,
               Y.rel = Y - min.Y) %>%
        mutate(patch = LianaBCI::patchnumber_from_position(X.rel,Y.rel,
                                                           patch_X = spatial.resolution, patch_Y = spatial.resolution,
                                                           extr_x = c(0,60),
                                                           extr_y = c(0,60))[["patch"]],
               patch_X =  LianaBCI::patchnumber_from_position(X.rel,Y.rel,
                                                              patch_X = spatial.resolution, patch_Y = spatial.resolution,
                                                              extr_x = c(0,60),
                                                              extr_y = c(0,60))[["patch_X"]],
               patch_Y =  LianaBCI::patchnumber_from_position(X.rel,Y.rel,
                                                              patch_X = spatial.resolution, patch_Y = spatial.resolution,
                                                              extr_x = c(0,60),
                                                              extr_y = c(0,60))[["patch_Y"]]) %>%
        filter(patch_X %in% seq(1,Npatch_X),
               patch_Y %in% seq(1,Npatch_Y),
               patch %in% seq(1,Npatch))


      df.chm.tmp <- data.frame()

      for (i in seq(1,length(seq(1,Npatch,100)))){
        df.chm.tmp <- bind_rows(list(df.chm.tmp,
                                     pc %>%
                                       filter(patch %in% seq(1+ (i-1)*100,(i)*100)) %>%
                                       group_by(patch,patch_X,patch_Y) %>%
                                       summarise(Z = max(Zabs,na.rm = TRUE),
                                                 .groups = "keep")))
      }


      # chm <- pc %>% group_by(patch,patch_X,patch_Y) %>%
      #   summarise(Z = max(Zabs,na.rm = TRUE),
      #             .groups = "keep")

      df.chm <- bind_rows(list(df.chm,
                               df.chm.tmp %>% mutate(plot = plots[iplot],
                                                     year = years[iyear],
                                                     resolution = spatial.resolution)))
    }

    saveRDS(object = df.chm,
            file = file.path(getwd(),"OP_chm",paste0("df_chm","plot",sprintf("%02d",plots[iplot]),"_",years[iyear],".RDS")))

    saveRDS(object = df.SP,
            file = file.path(getwd(),"OP_chm",paste0("df_SP","plot",sprintf("%02d",plots[iplot]),"_",years[iyear],".RDS")))

  }
}



# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/generate.chm.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
