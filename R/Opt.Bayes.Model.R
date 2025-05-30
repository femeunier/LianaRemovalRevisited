Opt.Bayes.Model <- function(dir.name,
                            settings,
                            site.name,
                            strong = FALSE,
                            site.re = FALSE,
                            threads = FALSE,
                            nested = FALSE){

  Names <- settings[["Names"]]
  fixed.effect.2.test <- settings[["fixed.effect.2.test"]]
  overwrite <- settings[["overwrite"]]
  re <- settings[["re"]]
  Nchains <- settings[["Nchains"]]
  Niter <- settings[["Niter"]]
  control.list <- settings[["control.list"]]
  backend <- settings[["backend"]]

  data.file <- file.path(dir.name,
                         paste0("data_",site.name,".RDS"))

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
      op.file <- file.path(dir.name,paste0("Fit.",site.name,".",cname,".RDS"))

      if (!overwrite & file.exists(op.file)){
        next()
      }


      if (nested){
        form.list <- default.forms.nested(names = Names,
                                          fixed.effect = cfixed.effect.2.test[[model.form]],
                                          random.effect = re,
                                          site.re = site.re)
      } else {
        form.list <- default.forms(names = Names,
                                   fixed.effect = cfixed.effect.2.test[[model.form]],
                                   random.effect = re,
                                   site.re = site.re)
      }



      priors.list <- default.priors(names = model,
                                    fixed.effect = cfixed.effect.2.test[[model.form]],
                                    random.effect = "none",
                                    strong = strong)


      existing.cat <- paste0("liana.cat",unique(data %>%
                                                  filter(liana.cat != "no") %>%
                                                  pull(liana.cat)))

      for (i in seq(1,length(priors.list))){
        priors.list[[i]] <- priors.list[[i]]  %>%
          filter(coef %in% c("","Intercept",existing.cat))
      }

      if (!(threads)){
        cfit <- brm(form.list[[model]],
                    data=data %>%
                      mutate(logh = log(h),
                             sp = as.factor(sp)),
                    cores = min(Nchains,
                                parallel::detectCores() - 1),
                    prior = priors.list[[model]],
                    control = control.list,
                    chains = Nchains,
                    backend = backend,
                    iter = Niter,
                    silent = 2)
      } else{
        cfit <- brm(form.list[[model]],
                    data=data %>%
                      mutate(logh = log(h),
                             sp = as.factor(sp)),
                    cores = Nchains,
                    prior = priors.list[[model]],
                    control = control.list,
                    chains = Nchains,
                    threads = threading(floor(parallel::detectCores()/Nchains)),
                    iter = Niter,
                    backend = backend,
                    silent = 2)
      }

      saveRDS(cfit,op.file)
    }
  }
}
