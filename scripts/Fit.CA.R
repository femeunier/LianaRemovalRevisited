rm(list = ls())

library(dplyr)
library(brms)
library(ggplot2)
library(LianaRemovalRevisited)

data <-  read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/2019_TLS_data.csv",stringsAsFactors = FALSE) %>%
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
                            levels = c("no","low","high"))) %>%
  group_by(sp) %>%
  mutate(N = n()) %>%
  mutate(sp = case_when(N <= 10 | sp == "" | tolower(sp) == "other" ~ "OTHER",
                        TRUE ~ sp)) %>%
  dplyr::select(-N)

################################################################################
# Main params
Names <- c("power")

Nchains <- 4
Niter <- 5000
control.list <- list(adapt_delta = 0.8,
                     max_treedepth = 10)

overwrite <- TRUE

fixed.effect.2.test <- list(power = list("none","b",
                                         "all"),
                            weibull = list("none",
                                           "a",
                                           "all"),
                            gmm = list("none",
                                       "a","b","k",
                                       c("a","b"),c("a","k"),c("b","k"),
                                       "all"))
re <- "all"

################################################################################
# Fit CA

ggplot(data = data,
       aes(x = dbh,y = CA, color = liana.cat, fill = liana.cat)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  stat_smooth(method = "lm") +
  theme_bw()

for (model in Names){

  cfixed.effect.2.test <- fixed.effect.2.test[[model]]

  for (model.form in seq(1,length(cfixed.effect.2.test))){

    print(paste("Crown area fit -",
                paste0("Model (",model,"):"),paste0(which(Names == model),"/",length(Names)),"-",
                paste0("Model Form (",paste(cfixed.effect.2.test[[model.form]],collapse = ""),"):"),paste0(model.form,"/",length(cfixed.effect.2.test))
    )
    )

    cname <- paste(model,paste(cfixed.effect.2.test[[model.form]],collapse = ""),sep = "_")
    op.file <- file.path(".","outputs",paste0("Fit.CA.",cname,".RDS"))

    if (!overwrite & file.exists(op.file)){
      next()
    }

    form.list <- default.forms(names = Names,
                               fixed.effect = cfixed.effect.2.test[[model.form]],
                               random.effect = re,
                               model.output = "logCA")

    priors.list <- default.priors(names = model,
                                  fixed.effect = cfixed.effect.2.test[[model.form]],
                                  random.effect = "none")

    cfit <- brm(form.list[[model]],
                data=data %>%
                  mutate(logCA = log(CA),
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
