Opt.Bayes.Model.CA <- function(dir.name,settings,site.name,strong = FALSE,site.re = FALSE){

  Names <- settings[["Names"]]
  fixed.effect.2.test <- settings[["fixed.effect.2.test"]]
  overwrite <- settings[["overwrite"]]
  re <- settings[["re"]]
  Nchains <- settings[["Nchains"]]
  Niter <- settings[["Niter"]]
  control.list <- settings[["control.list"]]

  data.file <- file.path(dir.name,
                         paste0("data_",site.name,"_CA.RDS"))

  data <- readRDS(data.file)

  for (model in Names){

    cfixed.effect.2.test <- fixed.effect.2.test[[model]]

    for (model.form in seq(1,length(cfixed.effect.2.test))){

      print(paste(site.name,"-",
                  paste0("Model (",model,"):"),paste0(which(Names == model),"/",length(Names)),"-",
                  paste0("Model Form (",paste(cfixed.effect.2.test[[model.form]],collapse = ""),"):"),paste0(model.form,"/",length(cfixed.effect.2.test))
      )
      )

      cname <- paste(model,paste(cfixed.effect.2.test[[model.form]],collapse = ""),sep = "_")
      op.file <- file.path(dir.name,paste0("Fit.",site.name,".",cname,".CA.RDS"))

      if (!overwrite & file.exists(op.file)){
        next()
      }

      form.list <- default.forms(names = Names,
                                 fixed.effect = cfixed.effect.2.test[[model.form]],
                                 random.effect = re,
                                 model.output = "logCA",
                                 site.re = site.re)

      priors.list <- default.priors.CA(names = model,
                                       fixed.effect = cfixed.effect.2.test[[model.form]],
                                       random.effect = "none",
                                       strong = strong)


      cats <- unique(data %>%
                       filter(liana.cat != "no") %>%
                       pull(liana.cat))
      existing.cat <- paste0("liana.cat",cats)

      for (i in seq(1,length(priors.list))){
        priors.list[[i]] <- priors.list[[i]]  %>%
          filter(coef %in% c("","Intercept",existing.cat))
      }

      cfit <- brm(form.list[[model]],
                  data=data %>%
                    mutate(logCA = log(area),
                           sp = as.factor(sp),
                           liana.cat = factor(liana.cat,
                                              levels = c("no",as.character(cats)))),
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
}
