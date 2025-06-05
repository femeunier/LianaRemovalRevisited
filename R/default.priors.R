default.priors <- function(names,
                           fixed.effect = "all",
                           random.effect = "all",
                           strong = FALSE){


  priors.list <- list()

  if (!strong){

    if ("power" %in% tolower(names)){

      priors.list[["power"]] <- c(set_prior("normal(1,0.3)",  nlpar = "a", coef = "Intercept"),
                                  set_prior("normal(0.3,0.2)", nlpar = "b", coef = "Intercept"))

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
          set_prior("normal(0,0.2)",  nlpar = "b", coef = "liana.catlow") +
          set_prior("normal(0,0.2)",  nlpar = "b", coef = "liana.cathigh")

      }

      # Add prior for random effect

      if (!("none" %in% tolower(random.effect))){
        priors.list[["power"]] <- priors.list[["power"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sigma", lb = 0)
      }


      if ("all" %in% tolower(random.effect)){
        priors.list[["power"]] <- priors.list[["power"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0, group = "sp") +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0, group = "sp")

      } else if ("a" %in% tolower(random.effect)){
        priors.list[["power"]] <- priors.list[["power"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0, group = "sp")
      } else if ("b" %in% tolower(random.effect)){
        priors.list[["power"]] <- priors.list[["power"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0, group = "sp")
      }

    }

    if ("weibull" %in% tolower(names)){

      priors.list[["weibull"]] <-  c(set_prior("normal(4,2)",  nlpar = "a", coef = "Intercept"),
                                     set_prior("normal(0.03,0.02)", nlpar = "b", coef = "Intercept"),
                                     set_prior("normal(0.7,0.2)", nlpar = "k", coef = "Intercept"))

      # Weibull
      if ("all" %in% tolower(fixed.effect)){
        priors.list[["weibull"]] <-    priors.list[["weibull"]] +

          set_prior("normal(0,1)",  nlpar = "a", coef = "liana.catlow") +
          set_prior("normal(0,1)",  nlpar = "a", coef = "liana.cathigh") +

          set_prior("normal(0,0.02)",  nlpar = "b", coef = "liana.catlow") +
          set_prior("normal(0,0.02)",  nlpar = "b", coef = "liana.cathigh") +

          set_prior("normal(0,0.2)",  nlpar = "k", coef = "liana.catlow") +
          set_prior("normal(0,0.2)",  nlpar = "k", coef = "liana.cathigh")

      } else if ("a" %in% tolower(fixed.effect)){
        priors.list[["weibull"]] <- priors.list[["weibull"]] +
          set_prior("normal(0,1)",  nlpar = "a", coef = "liana.catlow") +
          set_prior("normal(0,1)",  nlpar = "a", coef = "liana.cathigh")

      } else if ("b" %in% tolower(fixed.effect)){
        priors.list[["weibull"]] <- priors.list[["weibull"]] +
          set_prior("normal(0,0.02)",  nlpar = "b", coef = "liana.catlow") +
          set_prior("normal(0,0.02)",  nlpar = "b", coef = "liana.cathigh")

      } else if ("k" %in% tolower(fixed.effect)){
        priors.list[["weibull"]] <- priors.list[["weibull"]] +
          set_prior("normal(0,0.2)",  nlpar = "k", coef = "liana.catlow") +
          set_prior("normal(0,0.2)",  nlpar = "k", coef = "liana.cathigh")

      }

      # Add prior for random effect

      if (!("none" %in% tolower(random.effect))){
        priors.list[["weibull"]] <- priors.list[["weibull"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sigma", lb = 0)
      }

      if ("all" %in% tolower(random.effect)){
        priors.list[["weibull"]] <- priors.list[["weibull"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0, group = "sp") +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0, group = "sp") +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0, group = "sp")

      } else if ("a" %in% tolower(random.effect)){
        priors.list[["weibull"]] <- priors.list[["weibull"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sigma", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0, group = "sp")

      } else if ("b" %in% tolower(random.effect)){
        priors.list[["weibull"]] <- priors.list[["weibull"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0, group = "sp")

      }  else if ("k" %in% tolower(random.effect)){
        priors.list[["weibull"]] <- priors.list[["weibull"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0, group = "sp")

      }

    }

    if ("gmm" %in% tolower(names)){

      priors.list[["gmm"]] <- c(set_prior("normal(4, 1)",  nlpar = "a", coef = "Intercept"),
                                set_prior("normal(0.75,0.25)", nlpar = "b", coef = "Intercept"),
                                set_prior("normal(25,10)", nlpar = "k", coef = "Intercept"))

      # gMM
      if ("all" %in% tolower(fixed.effect)){
        priors.list[["gmm"]] <-   priors.list[["gmm"]] +

          set_prior("normal(0,1)",  nlpar = "a", coef = "liana.catlow") +
          set_prior("normal(0,1)",  nlpar = "a", coef = "liana.cathigh") +

          set_prior("normal(0,0.25)",  nlpar = "b", coef = "liana.catlow") +
          set_prior("normal(0,0.25)",  nlpar = "b", coef = "liana.cathigh") +

          set_prior("normal(0,10)",  nlpar = "k", coef = "liana.catlow") +
          set_prior("normal(0,10)",  nlpar = "k", coef = "liana.cathigh")

      } else if ("a" %in% tolower(fixed.effect)){
        priors.list[["gmm"]] <- priors.list[["gmm"]] +
          set_prior("normal(0,1)",  nlpar = "a", coef = "liana.catlow") +
          set_prior("normal(0,1)",  nlpar = "a", coef = "liana.cathigh")

      } else if ("b" %in% tolower(fixed.effect)){
        priors.list[["gmm"]] <- priors.list[["gmm"]] +
          set_prior("normal(0,0.25)",  nlpar = "b", coef = "liana.catlow") +
          set_prior("normal(0,0.25)",  nlpar = "b", coef = "liana.cathigh")

      } else if ("k" %in% tolower(fixed.effect)){
        priors.list[["gmm"]] <- priors.list[["gmm"]] +
          set_prior("normal(0,10)",  nlpar = "k", coef = "liana.catlow") +
          set_prior("normal(0,10)",  nlpar = "k", coef = "liana.cathigh")

      }


      # Add prior for random effect

      if (!("none" %in% tolower(random.effect))){
        priors.list[["gmm"]] <- priors.list[["gmm"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sigma", lb = 0)
      }

      if ("all" %in% tolower(random.effect)){
        priors.list[["gmm"]] <- priors.list[["gmm"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0, group = "sp") +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0, group = "sp") +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0, group = "sp")

      } else if ("a" %in% tolower(random.effect)){
        priors.list[["gmm"]] <- priors.list[["gmm"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0, group = "sp")

      } else if ("b" %in% tolower(random.effect)){
        priors.list[["gmm"]] <- priors.list[["gmm"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0, group = "sp")

      }  else if ("k" %in% tolower(random.effect)){
        priors.list[["gmm"]] <- priors.list[["gmm"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0, group = "sp")

      }

    }
  } else {  # strong priors


    if ("power" %in% tolower(names)){

      priors.list[["power"]] <- c(set_prior("normal(1,0.15)",  nlpar = "a", coef = "Intercept"),
                                  set_prior("normal(0.3,0.1)", nlpar = "b", coef = "Intercept"))

      # Power
      if ("all" %in% tolower(fixed.effect)){
        priors.list[["power"]] <- priors.list[["power"]] +
          set_prior("normal(0,0.15)",  nlpar = "a", coef = "liana.catlow") +
          set_prior("normal(0,0.15)",  nlpar = "a", coef = "liana.cathigh") +
          set_prior("normal(0,0.025)",  nlpar = "b", coef = "liana.catlow") +
          set_prior("normal(0,0.025)",  nlpar = "b", coef = "liana.cathigh")

      } else if ("a" %in% tolower(fixed.effect)){
        priors.list[["power"]] <- priors.list[["power"]] +
          set_prior("normal(0,0.15)",  nlpar = "a", coef = "liana.catlow") +
          set_prior("normal(0,0.15)",  nlpar = "a", coef = "liana.cathigh")

      } else if ("b" %in% tolower(fixed.effect)){
        priors.list[["power"]] <- priors.list[["power"]] +
          set_prior("normal(0,0.025)",  nlpar = "b", coef = "liana.catlow") +
          set_prior("normal(0,0.025)",  nlpar = "b", coef = "liana.cathigh")

      }

      # Add prior for random effect

      if (!("none" %in% tolower(random.effect))){
        priors.list[["power"]] <- priors.list[["power"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sigma", lb = 0)
      }


      if ("all" %in% tolower(random.effect)){
        priors.list[["power"]] <- priors.list[["power"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0, group = "sp") +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0, group = "sp")

      } else if ("a" %in% tolower(random.effect)){
        priors.list[["power"]] <- priors.list[["power"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0, group = "sp")
      } else if ("b" %in% tolower(random.effect)){
        priors.list[["power"]] <- priors.list[["power"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0, group = "sp")
      }

    }

    if ("weibull" %in% tolower(names)){

      priors.list[["weibull"]] <-  c(set_prior("normal(4,1)",  nlpar = "a", coef = "Intercept"),
                                     set_prior("normal(0.03,0.01)", nlpar = "b", coef = "Intercept"),
                                     set_prior("normal(0.7,0.1)", nlpar = "k", coef = "Intercept"))

      # Weibull
      if ("all" %in% tolower(fixed.effect)){
        priors.list[["weibull"]] <-    priors.list[["weibull"]] +

          set_prior("normal(0,0.05)",  nlpar = "a", coef = "liana.catlow") +
          set_prior("normal(0,0.05)",  nlpar = "a", coef = "liana.cathigh") +

          set_prior("normal(0,0.0025)",  nlpar = "b", coef = "liana.catlow") +
          set_prior("normal(0,0.0025)",  nlpar = "b", coef = "liana.cathigh") +

          set_prior("normal(0,0.025)",  nlpar = "k", coef = "liana.catlow") +
          set_prior("normal(0,0.025)",  nlpar = "k", coef = "liana.cathigh")

      } else if ("a" %in% tolower(fixed.effect)){
        priors.list[["weibull"]] <- priors.list[["weibull"]] +
          set_prior("normal(0,0.05)",  nlpar = "a", coef = "liana.catlow") +
          set_prior("normal(0,0.05)",  nlpar = "a", coef = "liana.cathigh")

      } else if ("b" %in% tolower(fixed.effect)){
        priors.list[["weibull"]] <- priors.list[["weibull"]] +
          set_prior("normal(0,0.0025)",  nlpar = "b", coef = "liana.catlow") +
          set_prior("normal(0,0.0025)",  nlpar = "b", coef = "liana.cathigh")

      } else if ("k" %in% tolower(fixed.effect)){
        priors.list[["weibull"]] <- priors.list[["weibull"]] +
          set_prior("normal(0,0.025)",  nlpar = "k", coef = "liana.catlow") +
          set_prior("normal(0,0.025)",  nlpar = "k", coef = "liana.cathigh")

      }

      # Add prior for random effect

      if (!("none" %in% tolower(random.effect))){
        priors.list[["weibull"]] <- priors.list[["weibull"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sigma", lb = 0)
      }

      if ("all" %in% tolower(random.effect)){
        priors.list[["weibull"]] <- priors.list[["weibull"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0, group = "sp") +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0, group = "sp") +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0, group = "sp")

      } else if ("a" %in% tolower(random.effect)){
        priors.list[["weibull"]] <- priors.list[["weibull"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sigma", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0, group = "sp")

      } else if ("b" %in% tolower(random.effect)){
        priors.list[["weibull"]] <- priors.list[["weibull"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0, group = "sp")

      }  else if ("k" %in% tolower(random.effect)){
        priors.list[["weibull"]] <- priors.list[["weibull"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0, group = "sp")

      }

    }

    if ("gmm" %in% tolower(names)){

      priors.list[["gmm"]] <- c(set_prior("normal(4, 0.5)",  nlpar = "a", coef = "Intercept"),
                                set_prior("normal(0.75,0.05)", nlpar = "b", coef = "Intercept"),
                                set_prior("normal(22,2.5)", nlpar = "k", coef = "Intercept"))

      # gMM
      if ("all" %in% tolower(fixed.effect)){
        priors.list[["gmm"]] <-   priors.list[["gmm"]] +

          set_prior("normal(0,0.5)",  nlpar = "a", coef = "liana.catlow") +
          set_prior("normal(0,0.5)",  nlpar = "a", coef = "liana.cathigh") +

          set_prior("normal(0,0.05)",  nlpar = "b", coef = "liana.catlow") +
          set_prior("normal(0,0.05)",  nlpar = "b", coef = "liana.cathigh") +

          set_prior("normal(0,0.5)",  nlpar = "k", coef = "liana.catlow") +
          set_prior("normal(0,0.5)",  nlpar = "k", coef = "liana.cathigh")

      } else if ("a" %in% tolower(fixed.effect)){
        priors.list[["gmm"]] <- priors.list[["gmm"]] +
          set_prior("normal(0,0.5)",  nlpar = "a", coef = "liana.catlow") +
          set_prior("normal(0,0.5)",  nlpar = "a", coef = "liana.cathigh")

      } else if ("b" %in% tolower(fixed.effect)){
        priors.list[["gmm"]] <- priors.list[["gmm"]] +
          set_prior("normal(0,0.05)",  nlpar = "b", coef = "liana.catlow") +
          set_prior("normal(0,0.05)",  nlpar = "b", coef = "liana.cathigh")

      } else if ("k" %in% tolower(fixed.effect)){
        priors.list[["gmm"]] <- priors.list[["gmm"]] +
          set_prior("normal(0,0.5)",  nlpar = "k", coef = "liana.catlow") +
          set_prior("normal(0,0.5)",  nlpar = "k", coef = "liana.cathigh")

      }


      # Add prior for random effect

      if (!("none" %in% tolower(random.effect))){
        priors.list[["gmm"]] <- priors.list[["gmm"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sigma", lb = 0)
      }

      if ("all" %in% tolower(random.effect)){
        priors.list[["gmm"]] <- priors.list[["gmm"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0, group = "sp") +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0, group = "sp") +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0, group = "sp")

      } else if ("a" %in% tolower(random.effect)){
        priors.list[["gmm"]] <- priors.list[["gmm"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0, group = "sp")

      } else if ("b" %in% tolower(random.effect)){
        priors.list[["gmm"]] <- priors.list[["gmm"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0, group = "sp")

      }  else if ("k" %in% tolower(random.effect)){
        priors.list[["gmm"]] <- priors.list[["gmm"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0) +
          set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0, group = "sp")

      }


    }



  }


  return(priors.list)
}
