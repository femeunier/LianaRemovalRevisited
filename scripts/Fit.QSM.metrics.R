rm(list = ls())

library(dplyr)
library(brms)
library(ggplot2)
library(tidyr)
library(LianaRemovalRevisited)

data <- readRDS("./outputs/QSM_metrics.RDS") %>%
  mutate(liana.cat = factor(case_when(Liana == 0 ~ "no",
                                      Liana == 1 ~ "low",
                                      Liana == 2 ~ "high"),
                            levels = c("no","low","high"))) %>%
  mutate(branch_vol_L = tree_vol_L - trunk_vol_L) %>%
  mutate(branch_biomass = WSG*branch_vol_L,
         tree_biomass = WSG*tree_vol_L,
         trunk_biomass = WSG*trunk_vol_L)


cols2test <- c("branch_len","crown_length","crown_vol","Nbranches",
               "tree_vol_L","trunk_vol_L","branch_vol_L","crown_area_conv","crown_area_alpha",
               "branch_area")

data2keep <- data %>%
  dplyr::select(c("dbh","sp","liana.cat",cols2test))


data.long <- data2keep %>%
  pivot_longer(cols = -c(dbh,sp,liana.cat),
               names_to = "variable")

################################################################################
# Main params

Names <- c("power")
overwrite <- TRUE
re <- "all"

Nchains <- 4
Niter <- 5000
control.list <- list(adapt_delta = 0.8,
                     max_treedepth = 10)

fixed.effect.2.test <- list( power = list("a","b","all","none"))

ggplot(data = data.long,
       aes(x = dbh,y = value, color = liana.cat, fill = liana.cat)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ variable, scales = "free") +
  stat_smooth(method = "lm") +
  theme_bw()

for (ivar in seq(1,length(cols2test))){

  cvar <- cols2test[ivar]
  cdata <- data %>%
    dplyr::select(c("dbh","sp","liana.cat",cvar))


  for (model in Names){

    cfixed.effect.2.test <- fixed.effect.2.test[[model]]

    for (model.form in seq(1,length(cfixed.effect.2.test))){

      print(paste(cvar,"fit -",
                  paste0("Model (",model,"):"),paste0(which(Names == model),"/",length(Names)),"-",
                  paste0("Model Form (",paste(cfixed.effect.2.test[[model.form]],collapse = ""),"):"),paste0(model.form,"/",length(cfixed.effect.2.test))
      )
      )

      cname <- paste(model,paste(cfixed.effect.2.test[[model.form]],collapse = ""),sep = "_")
      op.file <- file.path(".","outputs",paste0("Fit.",cvar,".",cname,".RDS"))

      if (!overwrite & file.exists(op.file)){
        next()
      }

      form.list <- default.forms(names = Names,
                                 fixed.effect = cfixed.effect.2.test[[model.form]],
                                 random.effect = re,
                                 model.output = paste0("log",cvar))

      priors.list <- c()

      cdata[[paste0("log",cvar)]] <- log(cdata[[cvar]])

      cfit <- brm(form.list[[model]],
                  data=cdata %>%
                    mutate(sp = as.factor(sp)),
                  cores = min(Nchains,
                              parallel::detectCores() - 1),
                  prior = priors.list[[model]],
                  control = control.list,
                  chains = Nchains,
                  iter = Niter,
                  silent = 2)

      saveRDS(cfit,
              op.file)

    }
  }
}

