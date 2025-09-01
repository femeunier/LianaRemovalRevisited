Opt.Bayes.Model.sp <- function(dir.name,
                            settings,
                            site.name,
                            strong = FALSE,
                            site.re = FALSE,
                            threads = FALSE,
                            nested = FALSE,
                            crossed = FALSE,
                            include.re.prior = "none"){

  Names <- settings[["Names"]]
  fixed.effect.2.test <- settings[["fixed.effect.2.test"]]
  overwrite <- settings[["overwrite"]]
  re <- settings[["re"]]
  Nchains <- settings[["Nchains"]]
  Niter <- settings[["Niter"]]
  warmup <- settings[["warmup"]]

  make.a.copy <- settings[["make.a.copy"]]
  if (is.null(make.a.copy)){
    make.a.copy <- FALSE
  }

  if (is.null(warmup)){
    warmup <- floor(Niter/2)
  }

  init <- settings[["init"]]
  if (is.null(init)){
    init <- 0
  }

  init_r <- settings[["init_r"]]

  if (is.null(init_r)){
    init_r <- 2
  }


  refresh <- settings[["refresh"]]

  if (is.null(refresh)){
    refresh <- round(Niter/10)
  }

  thin <- settings[["thin"]]

  if (is.null(thin)){
    thin <- 1
  }

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
      op.file <- file.path(dir.name,
                           paste0("Fit.",site.name,".",cname,".RDS"))

      if (!overwrite & file.exists(op.file)){
        next()
      }

      form.list <- default.forms.sp(names = Names,
                                    fixed.effect = cfixed.effect.2.test[[model.form]],
                                    random.effect = re,
                                    site.re = site.re,
                                    nested = nested,
                                    crossed = crossed)

      priors.list <- default.priors(names = model,
                                    fixed.effect = cfixed.effect.2.test[[model.form]],
                                    random.effect = include.re.prior,
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
                             delta = 1e-5,
                             sp = as.factor(sp)),
                    cores = min(Nchains,
                                parallel::detectCores() - 1),
                    prior = priors.list[[model]],
                    control = control.list,
                    init = init,
                    init_r = init_r,
                    chains = Nchains,
                    backend = backend,
                    refresh = refresh,
                    iter = Niter,
                    warmup = warmup,
                    silent = 2)
      } else{
        cfit <- brm(form.list[[model]],
                    data=data %>%
                      mutate(logh = log(h),
                             delta = 1e-5,
                             sp = as.factor(sp)),
                    cores = Nchains,
                    prior = priors.list[[model]],
                    init = init,
                    init_r = init_r,
                    control = control.list,
                    chains = Nchains,
                    threads = threading(floor(parallel::detectCores()/Nchains)),
                    iter = Niter,
                    refresh = refresh,
                    warmup = warmup,
                    backend = backend,
                    silent = 2)
      }

      does.file.exist <- file.exists(op.file)
      count <- 1

      if (file.exists(op.file) & make.a.copy){
        while (does.file.exist){
          op.file <- file.path(dir.name,
                               paste0("Fit.",site.name,".",cname,".",count,".RDS"))
          count = count + 1

          does.file.exist <- file.exists(op.file)
        }
      }

      saveRDS(cfit,
              op.file)
    }
  }
}
