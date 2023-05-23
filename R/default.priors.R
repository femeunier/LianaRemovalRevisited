default.priors <- function(names,
                           fixed.effect = "all",
                           random.effect = "all"){


  priors.list <- list()

  if ("power" %in% tolower(names)){

    priors.list[["power"]] <- c(set_prior("gamma(7,3)",  nlpar = "a", coef = "Intercept"),
                                set_prior("gamma(3,10)", nlpar = "b", coef = "Intercept"))

    # Power
    if ("all" %in% tolower(fixed.effect)){
      priors.list[["power"]] <- priors.list[["power"]] +
                                  set_prior("normal(0,0.3)",  nlpar = "a", coef = "liana.catlow") +
                                  set_prior("normal(0,0.3)",  nlpar = "a", coef = "liana.cathigh") +
                                  set_prior("normal(0,0.05)",  nlpar = "b", coef = "liana.catlow") +
                                  set_prior("normal(0,0.05)",  nlpar = "b", coef = "liana.cathigh")

    } else if ("a" %in% tolower(fixed.effect)){
      priors.list[["power"]] <- priors.list[["power"]] +
        set_prior("normal(0,0.3)",  nlpar = "a", coef = "liana.catlow") +
        set_prior("normal(0,0.3)",  nlpar = "a", coef = "liana.cathigh")

    } else if ("b" %in% tolower(fixed.effect)){
      priors.list[["power"]] <- priors.list[["power"]] +
        set_prior("normal(0,0.05)",  nlpar = "b", coef = "liana.catlow") +
        set_prior("normal(0,0.05)",  nlpar = "b", coef = "liana.cathigh")

    }

    # Add prior for random effect

    if (!("none" %in% tolower(random.effect))){
      priors.list[["power"]] <- priors.list[["power"]] +
        set_prior("student_t(3, 0, 2.5)", class = "sigma", lb = 0)
    }


    if ("all" %in% tolower(random.effect)){
      priors.list[["power"]] <- priors.list[["power"]] +
        set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
        set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0)

    } else if ("a" %in% tolower(random.effect)){
      priors.list[["power"]] <- priors.list[["power"]] +
        set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0)
    } else if ("b" %in% tolower(random.effect)){
      priors.list[["power"]] <- priors.list[["power"]] +
        set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0)
    }

  }

  if ("weibull" %in% tolower(names)){

    priors.list[["weibull"]] <-  c(set_prior("gamma(5,0.1)",  nlpar = "a", coef = "Intercept"),
                                   set_prior("gamma(0.2,10)", nlpar = "b", coef = "Intercept"),
                                   set_prior("gamma(1,3)", nlpar = "k", coef = "Intercept"))

    # Weibull
    if ("all" %in% tolower(fixed.effect)){
      priors.list[["weibull"]] <-    priors.list[["weibull"]] +

                                    set_prior("normal(0,20)",  nlpar = "a", coef = "liana.catlow") +
                                    set_prior("normal(0,20)",  nlpar = "a", coef = "liana.cathigh") +

                                    set_prior("normal(0,0.02)",  nlpar = "b", coef = "liana.catlow") +
                                    set_prior("normal(0,0.02)",  nlpar = "b", coef = "liana.cathigh") +

                                    set_prior("normal(0,0.03)",  nlpar = "k", coef = "liana.catlow") +
                                    set_prior("normal(0,0.03)",  nlpar = "k", coef = "liana.cathigh")

    } else if ("a" %in% tolower(fixed.effect)){
      priors.list[["weibull"]] <- priors.list[["weibull"]] +
        set_prior("normal(0,20)",  nlpar = "a", coef = "liana.catlow") +
        set_prior("normal(0,20)",  nlpar = "a", coef = "liana.cathigh")

    } else if ("b" %in% tolower(fixed.effect)){
      priors.list[["weibull"]] <- priors.list[["weibull"]] +
        set_prior("normal(0,0.02)",  nlpar = "b", coef = "liana.catlow") +
        set_prior("normal(0,0.02)",  nlpar = "b", coef = "liana.cathigh")

    } else if ("k" %in% tolower(fixed.effect)){
      priors.list[["weibull"]] <- priors.list[["weibull"]] +
        set_prior("normal(0,0.03)",  nlpar = "k", coef = "liana.catlow") +
        set_prior("normal(0,0.03)",  nlpar = "k", coef = "liana.cathigh")

    }

    # Add prior for random effect

    if (!("none" %in% tolower(random.effect))){
      priors.list[["weibull"]] <- priors.list[["weibull"]] +
        set_prior("student_t(3, 0, 2.5)", class = "sigma", lb = 0)
    }

    if ("all" %in% tolower(random.effect)){
      priors.list[["weibull"]] <- priors.list[["weibull"]] +
        set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
        set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
        set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0)

    } else if ("a" %in% tolower(random.effect)){
      priors.list[["weibull"]] <- priors.list[["weibull"]] +
        set_prior("student_t(3, 0, 2.5)", class = "sigma", lb = 0) +
        set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0)

    } else if ("b" %in% tolower(random.effect)){
      priors.list[["weibull"]] <- priors.list[["weibull"]] +
        set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0)

    }  else if ("k" %in% tolower(random.effect)){
      priors.list[["weibull"]] <- priors.list[["weibull"]] +
        set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0)

    }

  }

  if ("gmm" %in% tolower(names)){

    priors.list[["gmm"]] <- c(set_prior("gamma(5,0.1)",  nlpar = "a", coef = "Intercept"),
                              set_prior("gamma(5,10)", nlpar = "b", coef = "Intercept"),
                              set_prior("normal(5,0.3)", nlpar = "k", coef = "Intercept"))

    # gMM
    if ("all" %in% tolower(fixed.effect)){
      priors.list[["gmm"]] <-   priors.list[["gmm"]] +

                                set_prior("normal(0,10)",  nlpar = "a", coef = "liana.catlow") +
                                set_prior("normal(0,10)",  nlpar = "a", coef = "liana.cathigh") +

                                set_prior("normal(0,0.2)",  nlpar = "b", coef = "liana.catlow") +
                                set_prior("normal(0,0.2)",  nlpar = "b", coef = "liana.cathigh") +

                                set_prior("normal(0,5)",  nlpar = "k", coef = "liana.catlow") +
                                set_prior("normal(0,5)",  nlpar = "k", coef = "liana.cathigh")

    } else if ("a" %in% tolower(fixed.effect)){
      priors.list[["gmm"]] <- priors.list[["gmm"]] +
        set_prior("normal(0,10)",  nlpar = "a", coef = "liana.catlow") +
        set_prior("normal(0,10)",  nlpar = "a", coef = "liana.cathigh")

    } else if ("b" %in% tolower(fixed.effect)){
      priors.list[["gmm"]] <- priors.list[["gmm"]] +
        set_prior("normal(0,0.2)",  nlpar = "b", coef = "liana.catlow") +
        set_prior("normal(0,0.2)",  nlpar = "b", coef = "liana.cathigh")

    } else if ("k" %in% tolower(fixed.effect)){
      priors.list[["gmm"]] <- priors.list[["gmm"]] +
        set_prior("normal(0,5)",  nlpar = "k", coef = "liana.catlow") +
        set_prior("normal(0,5)",  nlpar = "k", coef = "liana.cathigh")

    }


    # Add prior for random effect

    if (!("none" %in% tolower(random.effect))){
      priors.list[["gmm"]] <- priors.list[["gmm"]] +
        set_prior("student_t(3, 0, 2.5)", class = "sigma", lb = 0)
    }

    if ("all" %in% tolower(random.effect)){
      priors.list[["gmm"]] <- priors.list[["gmm"]] +
        set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
        set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
        set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0)

    } else if ("a" %in% tolower(random.effect)){
      priors.list[["gmm"]] <- priors.list[["gmm"]] +
        set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0)

    } else if ("b" %in% tolower(random.effect)){
      priors.list[["gmm"]] <- priors.list[["gmm"]] +
        set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0)

    }  else if ("k" %in% tolower(random.effect)){
      priors.list[["gmm"]] <- priors.list[["gmm"]] +
        set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0)

    }


  }

  return(priors.list)
}
