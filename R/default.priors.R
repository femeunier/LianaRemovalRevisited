default.priors <- function(names){


  priors.list <- form.list <- list()

  if ("power" %in% tolower(names)){

    # Power
    priors.list[["power"]] <- c(set_prior("normal(62,20)",  nlpar = "a",lb = 0),
                                set_prior("normal(0.0352,0.02)", nlpar = "b",lb = 0))


  }

  if ("weibull" %in% tolower(names)){

    # Weibull
    priors.list[["weibull"]] <- c(set_prior("normal(62,20)",  nlpar = "a",lb = 0),
                                  set_prior("normal(0.0352,0.02)", nlpar = "b",lb = 0),
                                  set_prior("normal(0.694,0.3)", nlpar = "k",lb = 0))
  }

  if ("gmm" %in% tolower(names)){

    # gMM
    priors.list[["gmm"]] <- c(set_prior("normal(58,10)",  nlpar = "a",lb = 0),
                              set_prior("normal(0.73,0.2)", nlpar = "b",lb = 0),
                              set_prior("normal(22,5)", nlpar = "k",lb = 0))

  }

  return(priors.list)
}
