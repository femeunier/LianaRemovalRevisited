rm(list = ls())

library(dplyr)
library(brms)
library(ggplot2)
library(LianaRemovalRevisited)
library(raster)

data <-  read.csv("./data/2019_TLS_data.csv",stringsAsFactors = FALSE) %>%
  dplyr::select(Species,Final.DBH..cm.,Height..m.,Liana,CPA..m2.,WSG,Total.vol..L.,Biomass..kg.) %>%
  rename(dbh = Final.DBH..cm.,
         h = Height..m.,
         sp = Species,
         CA = CPA..m2.,
         Vol = Total.vol..L.,
         Biomass = Biomass..kg.) %>%
  mutate(liana.cat = factor(case_when(Liana == 0 ~ "no",
                                      Liana == 1 ~ "low",
                                      Liana == 2 ~ "high"),
                            levels = c("no","low","high")))

WD <- read.csv("./data/WoodDens_20130917.csv") %>%
  dplyr::select(SP.,SG100C_AVG) %>%
  rename(sp = SP.,
         WSG2 = SG100C_AVG) %>%
  mutate(sp = tolower(sp)) %>%
  left_join(bci.spptable %>%
              dplyr::select(sp,Latin),
            by = "sp") %>%
  filter(!is.na(Latin)) %>%
  dplyr::select(-sp) %>%
  rename(sp = Latin)

data.WSG <- data %>%
  left_join(WD,
            by = "sp")

saveRDS(data.WSG,
        "./data/BCI.Biomass.data.RDS")

################################################################################
# Main params

Names <- c("power")
overwrite <- TRUE
re <- "all"

Nchains <- 4
Niter <- 5000
control.list <- list(adapt_delta = 0.8,
                     max_treedepth = 10)

fixed.effect.2.test <- list(power = list("a","none","all","b"))

# Fit Biomass
# cE <- (extract(RAST,cbind(-79.85,9.15),method="bilinear"))

E <- extract(R <- raster("./unzipdir/E.nc"),cbind(-79.85,9.15),method = "bilinear")
dbhs <- floor(min(data.WSG$dbh)):ceiling(max(data.WSG$dbh))
WD = 0.52
df.Chave <- data.frame(dbh = dbhs) %>%
  mutate(h = exp(0.893 - E + 0.76*log(dbh) - 0.034*(log(dbh)**2))) %>%
  mutate(AGB = 0.0673*(WD*dbh**2*h)**0.976)

ggplot(data = data.WSG) +
  geom_point(aes(x = (WSG*dbh**2*h),y = Biomass/1e3, color = liana.cat, fill = liana.cat)) +
    geom_line(data = df.Chave,
              aes(x = WD*dbh**2*h, y = AGB/1e3), color = "black") +
  scale_x_log10() +
  scale_y_log10() +
  stat_smooth(aes(x = (WSG*dbh**2*h),y = Biomass/1e3, color = liana.cat, fill = liana.cat),
              method = "lm") +
  theme_bw()

ggplot(data) +
  geom_density(aes(x = WSG, fill = liana.cat), alpha = 0.5) +
  theme_bw()

for (model in Names){

  cfixed.effect.2.test <- fixed.effect.2.test[[model]]

  for (model.form in seq(1,length(cfixed.effect.2.test))){

    print(paste("Biomass fit -",
                paste0("Model (",model,"):"),paste0(which(Names == model),"/",length(Names)),"-",
                paste0("Model Form (",paste(cfixed.effect.2.test[[model.form]],collapse = ""),"):"),paste0(model.form,"/",length(cfixed.effect.2.test))
    )
    )

    cname <- paste(model,paste(cfixed.effect.2.test[[model.form]],collapse = ""),sep = "_")
    op.file <- file.path(".","outputs",paste0("Fit.Biomass.",cname,".RDS"))

    if (!overwrite & file.exists(op.file)){
      next()
    }

    form.list <- default.forms(names = Names,
                               fixed.effect = cfixed.effect.2.test[[model.form]],
                               random.effect = re,
                               model.output = "logBiomass")


    priors.list <- default.priors.CA(names = model,
                                  fixed.effect = cfixed.effect.2.test[[model.form]],
                                  random.effect = "none")

    priors.list <- c()


    cfit <- brm(form.list[[model]],
                data=data %>%
                  mutate(dbh2keep = dbh) %>%
                  mutate(logBiomass = log(Biomass),
                         dbh = dbh,     # Chave
                         sp = as.factor(sp)),
                cores = min(Nchains,
                            parallel::detectCores() - 1),
                prior = priors.list[[model]],
                control = control.list,
                chains = Nchains,
                iter = Niter,
                silent = 2)

    saveRDS(cfit,op.file)

  }
}

# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/data/2019_TLS_data.csv hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/data/
# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/Fit.Biomass.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

