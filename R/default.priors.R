default.priors <- function(names,
                           params = "all"){


  priors.list <- list()

  if ("power" %in% tolower(names)){

    priors.list[["power"]] <- empty_prior()

    # Power
    if ("all" %in% tolower(params)){
      priors.list[["power"]] <- c(set_prior("normal(2.98,0.3)",  nlpar = "a"),
                                  set_prior("normal(0.56,0.05)", nlpar = "b"))
    } else if ("a" %in% tolower(params)){
      priors.list[["power"]] <- priors.list[["power"]] + set_prior("normal(2.98,0.3)",  nlpar = "a")
    } else if ("b" %in% tolower(params)){
      priors.list[["power"]] <- priors.list[["power"]] + set_prior("normal(0.56,0.05)", nlpar = "b")
    }
  }

  if ("weibull" %in% tolower(names)){

    priors.list[["weibull"]] <- empty_prior()

    # Weibull
    if ("all" %in% tolower(params)){
      priors.list[["weibull"]] <- c(set_prior("normal(62,20)",  nlpar = "a"),
                                    set_prior("normal(0.0352,0.02)", nlpar = "b"),
                                    set_prior("normal(0.694,0.3)", nlpar = "k"))

    } else if ("a" %in% tolower(params)){
      priors.list[["weibull"]] <- priors.list[["weibull"]] + set_prior("normal(62,20)",  nlpar = "a")

    } else if ("b" %in% tolower(params)){
      priors.list[["weibull"]] <- priors.list[["weibull"]] + set_prior("normal(0.0352,0.02)", nlpar = "b")

    } else if ("k" %in% tolower(params)){
      priors.list[["weibull"]] <- priors.list[["weibull"]] + set_prior("normal(0.694,0.3)", nlpar = "k")
    }
  }

  if ("gmm" %in% tolower(names)){

    priors.list[["gmm"]] <- empty_prior()

    # gMM
    if ("all" %in% tolower(params)){
      priors.list[["gmm"]] <- c(set_prior("normal(58,10)",  nlpar = "a"),
                                set_prior("normal(0.73,0.2)", nlpar = "b"),
                                set_prior("normal(22,5)", nlpar = "k"))

    } else if ("a" %in% tolower(params)){
      priors.list[["gmm"]] <- priors.list[["gmm"]] + set_prior("normal(58,10)",  nlpar = "a")

    } else if ("b" %in% tolower(params)){
      priors.list[["gmm"]] <- priors.list[["gmm"]] + set_prior("normal(0.73,0.2)", nlpar = "b")

    } else if ("k" %in% tolower(params)){
      priors.list[["gmm"]] <- priors.list[["gmm"]] + set_prior("normal(22,5)", nlpar = "k")
    }
  }

  return(priors.list)
}
