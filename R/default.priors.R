default.priors <- function(names,
                           params = "all"){


  priors.list <- list()

  if ("power" %in% tolower(names)){

    priors.list[["power"]] <- c(set_prior("normal(2.98,0.3)",  nlpar = "a", lb = 0),
                                set_prior("normal(0.56,0.05)", nlpar = "b", lb = 0))

    # Power
    if ("all" %in% tolower(params)){
      priors.list[["power"]] <- priors.list[["power"]] +
                                  set_prior("normal(0,0.3)",  nlpar = "a", coef = "liana.catlow") +
                                  set_prior("normal(0,0.3)",  nlpar = "a", coef = "liana.cathigh") +
                                  set_prior("normal(0,0.05)",  nlpar = "b", coef = "liana.catlow") +
                                  set_prior("normal(0,0.05)",  nlpar = "b", coef = "liana.cathigh")
    } else if ("a" %in% tolower(params)){
      priors.list[["power"]] <- priors.list[["power"]] +
        set_prior("normal(0,0.3)",  nlpar = "a", coef = "liana.catlow") +
        set_prior("normal(0,0.3)",  nlpar = "a", coef = "liana.cathigh")
    } else if ("b" %in% tolower(params)){
      priors.list[["power"]] <- priors.list[["power"]] +
        set_prior("normal(0,0.05)",  nlpar = "b", coef = "liana.catlow") +
        set_prior("normal(0,0.05)",  nlpar = "b", coef = "liana.cathigh")
    }
  }

  if ("weibull" %in% tolower(names)){

    priors.list[["weibull"]] <- c(set_prior("normal(62,20)",  nlpar = "a", lb = 0),
                                  set_prior("normal(0.0352,0.02)", nlpar = "b", lb = 0),
                                  set_prior("normal(0.694,0.3)", nlpar = "k", lb = 0))

    # Weibull
    if ("all" %in% tolower(params)){
      priors.list[["weibull"]] <-    priors.list[["weibull"]] +

                                    set_prior("normal(0,20)",  nlpar = "a", coef = "liana.catlow") +
                                    set_prior("normal(0,20)",  nlpar = "a", coef = "liana.cathigh") +

                                    set_prior("normal(0,0.02)",  nlpar = "b", coef = "liana.catlow") +
                                    set_prior("normal(0,0.02)",  nlpar = "b", coef = "liana.cathigh") +

                                    set_prior("normal(0,0.03)",  nlpar = "k", coef = "liana.catlow") +
                                    set_prior("normal(0,0.03)",  nlpar = "k", coef = "liana.cathigh")

    } else if ("a" %in% tolower(params)){
      priors.list[["weibull"]] <- priors.list[["weibull"]] +
        set_prior("normal(0,20)",  nlpar = "a", coef = "liana.catlow") +
        set_prior("normal(0,20)",  nlpar = "a", coef = "liana.cathigh")

    } else if ("b" %in% tolower(params)){
      priors.list[["weibull"]] <- priors.list[["weibull"]] +
        set_prior("normal(0,0.02)",  nlpar = "b", coef = "liana.catlow") +
        set_prior("normal(0,0.02)",  nlpar = "b", coef = "liana.cathigh")

    } else if ("k" %in% tolower(params)){
      priors.list[["weibull"]] <- priors.list[["weibull"]] +
        set_prior("normal(0,0.03)",  nlpar = "k", coef = "liana.catlow") +
        set_prior("normal(0,0.03)",  nlpar = "k", coef = "liana.cathigh")

    }
  }

  if ("gmm" %in% tolower(names)){

    priors.list[["gmm"]] <- c(set_prior("normal(58,10)",  nlpar = "a", lb = 0),
                              set_prior("normal(0.73,0.2)", nlpar = "b", lb = 0),
                              set_prior("normal(22,5)", nlpar = "k", lb = 0))

    # gMM
    if ("all" %in% tolower(params)){
      priors.list[["gmm"]] <-   priors.list[["gmm"]] +

                                set_prior("normal(0,10)",  nlpar = "a", coef = "liana.catlow") +
                                set_prior("normal(0,10)",  nlpar = "a", coef = "liana.cathigh") +

                                set_prior("normal(0,0.2)",  nlpar = "b", coef = "liana.catlow") +
                                set_prior("normal(0,0.2)",  nlpar = "b", coef = "liana.cathigh") +

                                set_prior("normal(0,5)",  nlpar = "k", coef = "liana.catlow") +
                                set_prior("normal(0,5)",  nlpar = "k", coef = "liana.cathigh")

    } else if ("a" %in% tolower(params)){
      priors.list[["gmm"]] <- priors.list[["gmm"]] +
        set_prior("normal(0,10)",  nlpar = "a", coef = "liana.catlow") +
        set_prior("normal(0,10)",  nlpar = "a", coef = "liana.cathigh")

    } else if ("b" %in% tolower(params)){
      priors.list[["gmm"]] <- priors.list[["gmm"]] +
        set_prior("normal(0,0.2)",  nlpar = "b", coef = "liana.catlow") +
        set_prior("normal(0,0.2)",  nlpar = "b", coef = "liana.cathigh")

    } else if ("k" %in% tolower(params)){
      priors.list[["gmm"]] <- priors.list[["gmm"]] +
        set_prior("normal(0,5)",  nlpar = "k", coef = "liana.catlow") +
        set_prior("normal(0,5)",  nlpar = "k", coef = "liana.cathigh")

    }
  }

  return(priors.list)
}
