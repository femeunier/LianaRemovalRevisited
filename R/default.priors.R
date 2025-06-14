default.priors <- function(names,
                           fixed.effect = "all",
                           random.effect = "all",    # or none
                           strong = FALSE){


  priors.list <- list()

  if (!strong){

    if ("power" %in% tolower(names)){

      priors.list[["power"]] <- c(set_prior("normal(1,0.3)",  nlpar = "a", coef = "Intercept"),
                                  set_prior("normal(0.3,0.1)", nlpar = "b", coef = "Intercept"))

      # Power
      if ("all" %in% tolower(fixed.effect)){
        priors.list[["power"]] <- priors.list[["power"]] +
          set_prior("normal(0,0.3)",  nlpar = "a", coef = "liana.catlow") +
          set_prior("normal(0,0.3)",  nlpar = "a", coef = "liana.cathigh") +
          set_prior("normal(0,0.1)",  nlpar = "b", coef = "liana.catlow") +
          set_prior("normal(0,0.1)",  nlpar = "b", coef = "liana.cathigh")

        if (!("none" %in% tolower(random.effect))){
          priors.list[["power"]] <- priors.list[["power"]] +
            set_prior("student_t(3, 0, 0.3)", class = "sd", coef = "liana.catlow", nlpar = "a", group = "site") +
            set_prior("student_t(3, 0, 0.3)", class = "sd", coef = "liana.cathigh", nlpar = "a", group = "sp") +
            set_prior("student_t(3, 0, 0.1)", class = "sd", coef = "liana.catlow", nlpar = "b", group = "site") +
            set_prior("student_t(3, 0, 0.1)", class = "sd", coef = "liana.cathigh", nlpar = "b", group = "sp")
        }


      } else if ("a" %in% tolower(fixed.effect)){
        priors.list[["power"]] <- priors.list[["power"]] +
          set_prior("normal(0,0.3)",  nlpar = "a", coef = "liana.catlow") +
          set_prior("normal(0,0.3)",  nlpar = "a", coef = "liana.cathigh")

        if (!("none" %in% tolower(random.effect))){
          priors.list[["power"]] <- priors.list[["power"]] +
            set_prior("student_t(3, 0, 0.3)", class = "sd", coef = "liana.catlow", nlpar = "a", group = "site") +
            set_prior("student_t(3, 0, 0.3)", class = "sd", coef = "liana.cathigh", nlpar = "a", group = "sp")
        }

      } else if ("b" %in% tolower(fixed.effect)){
        priors.list[["power"]] <- priors.list[["power"]] +
          set_prior("normal(0,0.1)",  nlpar = "b", coef = "liana.catlow") +
          set_prior("normal(0,0.1)",  nlpar = "b", coef = "liana.cathigh")

        if (!("none" %in% tolower(random.effect))){
          priors.list[["power"]] <- priors.list[["power"]] +
            set_prior("student_t(3, 0, 0.1)", class = "sd", coef = "liana.catlow", nlpar = "b", group = "site") +
            set_prior("student_t(3, 0, 0.1)", class = "sd", coef = "liana.cathigh", nlpar = "b", group = "sp")
        }


      }

      # Add prior for random effect

      if ("all" %in% tolower(random.effect)){
        priors.list[["power"]] <- priors.list[["power"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sigma", lb = 0)
          set_prior("student_t(3, 0, 0.3)", class = "sd", nlpar = "a", group = "site") +
          set_prior("student_t(3, 0, 0.3)", class = "sd", nlpar = "a", group = "sp") +
          set_prior("student_t(3, 0, 0.1)", class = "sd", nlpar = "b", group = "site") +
          set_prior("student_t(3, 0, 0.1)", class = "sd", nlpar = "b", group = "sp")

      } else{
        if ("a" %in% tolower(random.effect)){
          priors.list[["power"]] <- priors.list[["power"]] +
            set_prior("student_t(3, 0, 0.3)", class = "sd", nlpar = "a", group = "site") +
            set_prior("student_t(3, 0, 0.3)", class = "sd", nlpar = "a", group = "sp")
        }
        if ("b" %in% tolower(random.effect)){
          priors.list[["power"]] <- priors.list[["power"]] +
            set_prior("student_t(3, 0, 0.1)", class = "sd", nlpar = "b", group = "site") +
            set_prior("student_t(3, 0, 0.1)", class = "sd", nlpar = "b", group = "sp")
        }
      }

    }

    if ("weibull" %in% tolower(names)){

      priors.list[["weibull"]] <-  c(set_prior("normal(4,1.5)",  nlpar = "a", coef = "Intercept"),
                                     set_prior("normal(0.03,0.01)", nlpar = "b", coef = "Intercept"),
                                     set_prior("normal(0.7,0.2)", nlpar = "k", coef = "Intercept"))

      # Weibull
      if ("all" %in% tolower(fixed.effect)){
        priors.list[["weibull"]] <-    priors.list[["weibull"]] +

          set_prior("normal(0,1.5)",  nlpar = "a", coef = "liana.catlow") +
          set_prior("normal(0,1.5)",  nlpar = "a", coef = "liana.cathigh") +

          set_prior("normal(0,0.01)",  nlpar = "b", coef = "liana.catlow") +
          set_prior("normal(0,0.01)",  nlpar = "b", coef = "liana.cathigh") +

          set_prior("normal(0,0.2)",  nlpar = "k", coef = "liana.catlow") +
          set_prior("normal(0,0.2)",  nlpar = "k", coef = "liana.cathigh")


        if (!("none" %in% tolower(random.effect))){

          priors.list[["weibull"]] <- priors.list[["weibull"]] +
            set_prior("student_t(3, 0, 1.5)", class = "sd", coef = "liana.catlow", nlpar = "a", group = "site") +
            set_prior("student_t(3, 0, 1.5)", class = "sd", coef = "liana.cathigh", nlpar = "a", group = "sp") +
            set_prior("student_t(3, 0, 0.01)", class = "sd", coef = "liana.catlow", nlpar = "b", group = "site") +
            set_prior("student_t(3, 0, 0.01)", class = "sd", coef = "liana.cathigh", nlpar = "b", group = "sp") +
            set_prior("student_t(3, 0, 0.2)", class = "sd", coef = "liana.catlow", nlpar = "k", group = "site") +
            set_prior("student_t(3, 0, 0.2)", class = "sd", coef = "liana.cathigh", nlpar = "k", group = "sp")
        }


      } else {
        if ("a" %in% tolower(fixed.effect)){
          priors.list[["weibull"]] <- priors.list[["weibull"]] +
            set_prior("normal(0,1.5)",  nlpar = "a", coef = "liana.catlow") +
            set_prior("normal(0,1.5)",  nlpar = "a", coef = "liana.cathigh")


          if (!("none" %in% tolower(random.effect))){

            priors.list[["weibull"]] <- priors.list[["weibull"]] +
              set_prior("student_t(3, 0, 1.5)", class = "sd", coef = "liana.catlow", nlpar = "a", group = "site") +
              set_prior("student_t(3, 0, 1.5)", class = "sd", coef = "liana.cathigh", nlpar = "a", group = "sp")
          }

        }

        if ("b" %in% tolower(fixed.effect)){
          priors.list[["weibull"]] <- priors.list[["weibull"]] +
            set_prior("normal(0,0.01)",  nlpar = "b", coef = "liana.catlow") +
            set_prior("normal(0,0.01)",  nlpar = "b", coef = "liana.cathigh")


          if (!("none" %in% tolower(random.effect))){

            priors.list[["weibull"]] <- priors.list[["weibull"]] +
              set_prior("student_t(3, 0, 0.01)", class = "sd", coef = "liana.catlow", nlpar = "b", group = "site") +
              set_prior("student_t(3, 0, 0.01)", class = "sd", coef = "liana.cathigh", nlpar = "b", group = "sp")
          }


        }

        if ("k" %in% tolower(fixed.effect)){
          priors.list[["weibull"]] <- priors.list[["weibull"]] +
            set_prior("normal(0,0.2)",  nlpar = "k", coef = "liana.catlow") +
            set_prior("normal(0,0.2)",  nlpar = "k", coef = "liana.cathigh")

          if (!("none" %in% tolower(random.effect))){

            priors.list[["weibull"]] <- priors.list[["weibull"]] +
              set_prior("student_t(3, 0, 0.2)", class = "sd", coef = "liana.catlow", nlpar = "k", group = "site") +
              set_prior("student_t(3, 0, 0.2)", class = "sd", coef = "liana.cathigh", nlpar = "k", group = "sp")
          }
        }
      }

      # Add prior for random effect

      if (!("none" %in% tolower(random.effect))){
        priors.list[["weibull"]] <- priors.list[["weibull"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sigma", lb = 0)
      }

      if ("all" %in% tolower(random.effect)){
        priors.list[["weibull"]] <- priors.list[["weibull"]] +
          set_prior("student_t(3, 0, 1.5)", class = "sd", nlpar = "a", group = "site") +
          set_prior("student_t(3, 0, 1.5)", class = "sd", nlpar = "a", group = "sp") +
          set_prior("student_t(3, 0, 0.01)", class = "sd", nlpar = "b", group = "site") +
          set_prior("student_t(3, 0, 0.01)", class = "sd", nlpar = "b", group = "sp") +
          set_prior("student_t(3, 0, 0.2)", class = "sd", nlpar = "k", group = "site") +
          set_prior("student_t(3, 0, 0.2)", class = "sd", nlpar = "k", group = "sp")

      } else {
        if ("a" %in% tolower(random.effect)){
          priors.list[["weibull"]] <- priors.list[["weibull"]] +
            set_prior("student_t(3, 0, 1.5)", class = "sd", nlpar = "a", group = "site") +
            set_prior("student_t(3, 0, 1.5)", class = "sd", nlpar = "a", group = "sp")

        }

        if ("b" %in% tolower(random.effect)){
          priors.list[["weibull"]] <- priors.list[["weibull"]] +
            set_prior("student_t(3, 0, 0.01)", class = "sd", nlpar = "b", group = "site") +
            set_prior("student_t(3, 0, 0.01)", class = "sd", nlpar = "b", group = "sp")

        }

        if ("k" %in% tolower(random.effect)){
          priors.list[["weibull"]] <- priors.list[["weibull"]] +
            set_prior("student_t(3, 0, 0.2)", class = "sd", nlpar = "k", group = "site") +
            set_prior("student_t(3, 0, 0.2)", class = "sd", nlpar = "k", group = "sp")

        }
      }

    }

    if ("gmm" %in% tolower(names)){

      priors.list[["gmm"]] <- c(set_prior("normal(4, 1.5)",  nlpar = "a", coef = "Intercept"),
                                set_prior("normal(0.75,0.25)", nlpar = "b", coef = "Intercept"),
                                set_prior("normal(25,8)", nlpar = "k", coef = "Intercept"))

      # gMM
      if ("all" %in% tolower(fixed.effect)){
        priors.list[["gmm"]] <-   priors.list[["gmm"]] +

          set_prior("normal(0,1.5)",  nlpar = "a", coef = "liana.catlow") +
          set_prior("normal(0,1.5)",  nlpar = "a", coef = "liana.cathigh") +

          set_prior("normal(0,0.25)",  nlpar = "b", coef = "liana.catlow") +
          set_prior("normal(0,0.25)",  nlpar = "b", coef = "liana.cathigh") +

          set_prior("normal(0,8)",  nlpar = "k", coef = "liana.catlow") +
          set_prior("normal(0,8)",  nlpar = "k", coef = "liana.cathigh")

        if (!("none" %in% tolower(random.effect))){

          priors.list[["gmm"]] <- priors.list[["gmm"]] +
            set_prior("student_t(3, 0, 1.5)", class = "sd", coef = "liana.catlow", nlpar = "a", group = "site") +
            set_prior("student_t(3, 0, 1.5)", class = "sd", coef = "liana.cathigh", nlpar = "a", group = "sp") +
            set_prior("student_t(3, 0, 0.25)", class = "sd", coef = "liana.catlow", nlpar = "b", group = "site") +
            set_prior("student_t(3, 0, 0.25)", class = "sd", coef = "liana.cathigh", nlpar = "b", group = "sp") +
            set_prior("student_t(3, 0, 8)", class = "sd", coef = "liana.catlow", nlpar = "k", group = "site") +
            set_prior("student_t(3, 0, 8)", class = "sd", coef = "liana.cathigh", nlpar = "k", group = "sp")
        }


      } else {
        if ("a" %in% tolower(fixed.effect)){
          priors.list[["gmm"]] <- priors.list[["gmm"]] +
            set_prior("normal(0,1.5)",  nlpar = "a", coef = "liana.catlow") +
            set_prior("normal(0,1.5)",  nlpar = "a", coef = "liana.cathigh")


          if (!("none" %in% tolower(random.effect))){

            priors.list[["gmm"]] <- priors.list[["gmm"]] +
              set_prior("student_t(3, 0, 1.5)", class = "sd", coef = "liana.catlow", nlpar = "a", group = "site") +
              set_prior("student_t(3, 0, 1.5)", class = "sd", coef = "liana.cathigh", nlpar = "a", group = "sp")
          }

        }

        if ("b" %in% tolower(fixed.effect)){
          priors.list[["gmm"]] <- priors.list[["gmm"]] +
            set_prior("normal(0,0.25)",  nlpar = "b", coef = "liana.catlow") +
            set_prior("normal(0,0.25)",  nlpar = "b", coef = "liana.cathigh")

          if (!("none" %in% tolower(random.effect))){

            priors.list[["gmm"]] <- priors.list[["gmm"]] +
              set_prior("student_t(3, 0, 0.25)", class = "sd", coef = "liana.catlow", nlpar = "b", group = "site") +
              set_prior("student_t(3, 0, 0.25)", class = "sd", coef = "liana.cathigh", nlpar = "b", group = "sp")
          }

        }

        if ("k" %in% tolower(fixed.effect)){
          priors.list[["gmm"]] <- priors.list[["gmm"]] +
            set_prior("normal(0,8)",  nlpar = "k", coef = "liana.catlow") +
            set_prior("normal(0,8)",  nlpar = "k", coef = "liana.cathigh")

          if (!("none" %in% tolower(random.effect))){

            priors.list[["gmm"]] <- priors.list[["gmm"]] +
              set_prior("student_t(3, 0, 8)", class = "sd", coef = "liana.catlow", nlpar = "k", group = "site") +
              set_prior("student_t(3, 0, 8)", class = "sd", coef = "liana.cathigh", nlpar = "k", group = "sp")
          }

        }

      }

      # Add prior for random effect

      if (!("none" %in% tolower(random.effect))){
        priors.list[["gmm"]] <- priors.list[["gmm"]] +
          set_prior("student_t(3, 0, 2.5)", class = "sigma", lb = 0)
      }

      if ("all" %in% tolower(random.effect)){
        priors.list[["gmm"]] <- priors.list[["gmm"]] +
          set_prior("student_t(3, 0, 1.5)", class = "sd", nlpar = "a", group = "site") +
          set_prior("student_t(3, 0, 1.5)", class = "sd", nlpar = "a", group = "sp") +
          set_prior("student_t(3, 0, 0.25)", class = "sd", nlpar = "b", group = "site") +
          set_prior("student_t(3, 0, 0.25)", class = "sd", nlpar = "b", group = "sp") +
          set_prior("student_t(3, 0, 8)", class = "sd", nlpar = "k", group = "site") +
          set_prior("student_t(3, 0, 8)", class = "sd", nlpar = "k", group = "sp")

      } else{
        if ("a" %in% tolower(random.effect)){
          priors.list[["gmm"]] <- priors.list[["gmm"]] +
            set_prior("student_t(3, 0, 1.5)", class = "sd", nlpar = "a", group = "site") +
            set_prior("student_t(3, 0, 1.5)", class = "sd", nlpar = "a", group = "sp")

        }

        if ("b" %in% tolower(random.effect)){
          priors.list[["gmm"]] <- priors.list[["gmm"]] +
            set_prior("student_t(3, 0, 0.25)", class = "sd", nlpar = "b", group = "site") +
            set_prior("student_t(3, 0, 0.25)", class = "sd", nlpar = "b", group = "sp")

        }

        if ("k" %in% tolower(random.effect)){
          priors.list[["gmm"]] <- priors.list[["gmm"]] +
            set_prior("student_t(3, 0, 8)", class = "sd", nlpar = "k", group = "site") +
            set_prior("student_t(3, 0, 8)", class = "sd", nlpar = "k", group = "sp")

        }
      }
    }

  } else {   # Strong priors

  #   if ("power" %in% tolower(names)){
  #
  #     priors.list[["power"]] <- c(set_prior("normal(1,0.1)",  nlpar = "a", coef = "Intercept"),
  #                                 set_prior("normal(0.3,0.033)", nlpar = "b", coef = "Intercept"))
  #
  #     # Power
  #     if ("all" %in% tolower(fixed.effect)){
  #       priors.list[["power"]] <- priors.list[["power"]] +
  #         set_prior("normal(0,0.1)",  nlpar = "a", coef = "liana.catlow") +
  #         set_prior("normal(0,0.1)",  nlpar = "a", coef = "liana.cathigh") +
  #         set_prior("normal(0,0.033)",  nlpar = "b", coef = "liana.catlow") +
  #         set_prior("normal(0,0.033)",  nlpar = "b", coef = "liana.cathigh")
  #
  #     } else if ("a" %in% tolower(fixed.effect)){
  #       priors.list[["power"]] <- priors.list[["power"]] +
  #         set_prior("normal(0,0.1)",  nlpar = "a", coef = "liana.catlow") +
  #         set_prior("normal(0,0.1)",  nlpar = "a", coef = "liana.cathigh")
  #
  #     } else if ("b" %in% tolower(fixed.effect)){
  #       priors.list[["power"]] <- priors.list[["power"]] +
  #         set_prior("normal(0,0.033)",  nlpar = "b", coef = "liana.catlow") +
  #         set_prior("normal(0,0.033)",  nlpar = "b", coef = "liana.cathigh")
  #
  #     }
  #
  #     # Add prior for random effect
  #
  #     if (!("none" %in% tolower(random.effect))){
  #       priors.list[["power"]] <- priors.list[["power"]] +
  #         set_prior("student_t(3, 0, 2.5)", class = "sigma", lb = 0)
  #     }
  #
  #
  #     if ("all" %in% tolower(random.effect)){
  #       priors.list[["power"]] <- priors.list[["power"]] +
  #         set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
  #         set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0, group = "sp") +
  #         set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
  #         set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0, group = "sp")
  #
  #     } else if ("a" %in% tolower(random.effect)){
  #       priors.list[["power"]] <- priors.list[["power"]] +
  #         set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
  #         set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0, group = "sp")
  #     } else if ("b" %in% tolower(random.effect)){
  #       priors.list[["power"]] <- priors.list[["power"]] +
  #         set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
  #         set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0, group = "sp")
  #     }
  #
  #   }
  #
  #   if ("weibull" %in% tolower(names)){
  #
  #     priors.list[["weibull"]] <-  c(set_prior("normal(4,0.5)",  nlpar = "a", coef = "Intercept"),
  #                                    set_prior("normal(0.03,0.0033)", nlpar = "b", coef = "Intercept"),
  #                                    set_prior("normal(0.7,0.08)", nlpar = "k", coef = "Intercept"))
  #
  #     # Weibull
  #     if ("all" %in% tolower(fixed.effect)){
  #       priors.list[["weibull"]] <-    priors.list[["weibull"]] +
  #
  #         set_prior("normal(0,0.5)",  nlpar = "a", coef = "liana.catlow") +
  #         set_prior("normal(0,0.5)",  nlpar = "a", coef = "liana.cathigh") +
  #
  #         set_prior("normal(0,0.0033)",  nlpar = "b", coef = "liana.catlow") +
  #         set_prior("normal(0,0.0033)",  nlpar = "b", coef = "liana.cathigh") +
  #
  #         set_prior("normal(0,0.08)",  nlpar = "k", coef = "liana.catlow") +
  #         set_prior("normal(0,0.08)",  nlpar = "k", coef = "liana.cathigh")
  #
  #     } else {
  #       if ("a" %in% tolower(fixed.effect)){
  #         priors.list[["weibull"]] <- priors.list[["weibull"]] +
  #           set_prior("normal(0,0.5)",  nlpar = "a", coef = "liana.catlow") +
  #           set_prior("normal(0,0.5)",  nlpar = "a", coef = "liana.cathigh")
  #
  #       }
  #
  #       if ("b" %in% tolower(fixed.effect)){
  #         priors.list[["weibull"]] <- priors.list[["weibull"]] +
  #           set_prior("normal(0,0.0033)",  nlpar = "b", coef = "liana.catlow") +
  #           set_prior("normal(0,0.0033)",  nlpar = "b", coef = "liana.cathigh")
  #
  #       }
  #
  #       if ("k" %in% tolower(fixed.effect)){
  #         priors.list[["weibull"]] <- priors.list[["weibull"]] +
  #           set_prior("normal(0,0.08)",  nlpar = "k", coef = "liana.catlow") +
  #           set_prior("normal(0,0.08)",  nlpar = "k", coef = "liana.cathigh")
  #
  #       }
  #     }
  #
  #     # Add prior for random effect
  #
  #     if (!("none" %in% tolower(random.effect))){
  #       priors.list[["weibull"]] <- priors.list[["weibull"]] +
  #         set_prior("student_t(3, 0, 2.5)", class = "sigma", lb = 0)
  #     }
  #
  #     if ("all" %in% tolower(random.effect)){
  #       priors.list[["weibull"]] <- priors.list[["weibull"]] +
  #         set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
  #         set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0, group = "sp") +
  #         set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
  #         set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0, group = "sp") +
  #         set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0) +
  #         set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0, group = "sp")
  #
  #     } else {
  #       if ("a" %in% tolower(random.effect)){
  #         priors.list[["weibull"]] <- priors.list[["weibull"]] +
  #           set_prior("student_t(3, 0, 2.5)", class = "sigma", lb = 0) +
  #           set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
  #           set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0, group = "sp")
  #
  #       }
  #
  #       if ("b" %in% tolower(random.effect)){
  #         priors.list[["weibull"]] <- priors.list[["weibull"]] +
  #           set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
  #           set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0, group = "sp")
  #
  #       }
  #
  #       if ("k" %in% tolower(random.effect)){
  #         priors.list[["weibull"]] <- priors.list[["weibull"]] +
  #           set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0) +
  #           set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0, group = "sp")
  #
  #       }
  #     }
  #
  #   }
  #
  #   if ("gmm" %in% tolower(names)){
  #
  #     priors.list[["gmm"]] <- c(set_prior("normal(4, 0.5)",  nlpar = "a", coef = "Intercept"),
  #                               set_prior("normal(0.75,0.08)", nlpar = "b", coef = "Intercept"),
  #                               set_prior("normal(25,3)", nlpar = "k", coef = "Intercept"))
  #
  #     # gMM
  #     if ("all" %in% tolower(fixed.effect)){
  #       priors.list[["gmm"]] <-   priors.list[["gmm"]] +
  #
  #         set_prior("normal(0,0.5)",  nlpar = "a", coef = "liana.catlow") +
  #         set_prior("normal(0,0.5)",  nlpar = "a", coef = "liana.cathigh") +
  #
  #         set_prior("normal(0,0.08)",  nlpar = "b", coef = "liana.catlow") +
  #         set_prior("normal(0,0.08)",  nlpar = "b", coef = "liana.cathigh") +
  #
  #         set_prior("normal(0,3)",  nlpar = "k", coef = "liana.catlow") +
  #         set_prior("normal(0,3)",  nlpar = "k", coef = "liana.cathigh")
  #
  #     } else {
  #       if ("a" %in% tolower(fixed.effect)){
  #         priors.list[["gmm"]] <- priors.list[["gmm"]] +
  #           set_prior("normal(0,0.5)",  nlpar = "a", coef = "liana.catlow") +
  #           set_prior("normal(0,0.5)",  nlpar = "a", coef = "liana.cathigh")
  #
  #       }
  #
  #       if ("b" %in% tolower(fixed.effect)){
  #         priors.list[["gmm"]] <- priors.list[["gmm"]] +
  #           set_prior("normal(0,0.08)",  nlpar = "b", coef = "liana.catlow") +
  #           set_prior("normal(0,0.08)",  nlpar = "b", coef = "liana.cathigh")
  #
  #       }
  #
  #       if ("k" %in% tolower(fixed.effect)){
  #         priors.list[["gmm"]] <- priors.list[["gmm"]] +
  #           set_prior("normal(0,3)",  nlpar = "k", coef = "liana.catlow") +
  #           set_prior("normal(0,3)",  nlpar = "k", coef = "liana.cathigh")
  #
  #       }
  #
  #     }
  #
  #     # Add prior for random effect
  #
  #     if (!("none" %in% tolower(random.effect))){
  #       priors.list[["gmm"]] <- priors.list[["gmm"]] +
  #         set_prior("student_t(3, 0, 2.5)", class = "sigma", lb = 0)
  #     }
  #
  #     if ("all" %in% tolower(random.effect)){
  #       priors.list[["gmm"]] <- priors.list[["gmm"]] +
  #         set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
  #         set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0, group = "sp") +
  #         set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
  #         set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0, group = "sp") +
  #         set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0) +
  #         set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0, group = "sp")
  #
  #     } else{
  #       if ("a" %in% tolower(random.effect)){
  #         priors.list[["gmm"]] <- priors.list[["gmm"]] +
  #           set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
  #           set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0, group = "sp")
  #
  #       }
  #
  #       if ("b" %in% tolower(random.effect)){
  #         priors.list[["gmm"]] <- priors.list[["gmm"]] +
  #           set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
  #           set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0, group = "sp")
  #
  #       }
  #
  #       if ("k" %in% tolower(random.effect)){
  #         priors.list[["gmm"]] <- priors.list[["gmm"]] +
  #           set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0) +
  #           set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0, group = "sp")
  #
  #       }
  #     }
  #
  #   }
  # }

    # Very strong priors

    if ("power" %in% tolower(names)){

      priors.list[["power"]] <- c(set_prior("normal(1,0.05)",  nlpar = "a", coef = "Intercept"),
                                  set_prior("normal(0.3,0.015)", nlpar = "b", coef = "Intercept"))

      # Power
      if ("all" %in% tolower(fixed.effect)){
        priors.list[["power"]] <- priors.list[["power"]] +
          set_prior("normal(0,0.05)",  nlpar = "a", coef = "liana.catlow") +
          set_prior("normal(0,0.05)",  nlpar = "a", coef = "liana.cathigh") +
          set_prior("normal(0,0.015)",  nlpar = "b", coef = "liana.catlow") +
          set_prior("normal(0,0.015)",  nlpar = "b", coef = "liana.cathigh")

      } else if ("a" %in% tolower(fixed.effect)){
        priors.list[["power"]] <- priors.list[["power"]] +
          set_prior("normal(0,0.05)",  nlpar = "a", coef = "liana.catlow") +
          set_prior("normal(0,0.05)",  nlpar = "a", coef = "liana.cathigh")

      } else if ("b" %in% tolower(fixed.effect)){
        priors.list[["power"]] <- priors.list[["power"]] +
          set_prior("normal(0,0.015)",  nlpar = "b", coef = "liana.catlow") +
          set_prior("normal(0,0.015)",  nlpar = "b", coef = "liana.cathigh")

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

      priors.list[["weibull"]] <-  c(set_prior("normal(4,0.25)",  nlpar = "a", coef = "Intercept"),
                                     set_prior("normal(0.03,0.0015)", nlpar = "b", coef = "Intercept"),
                                     set_prior("normal(0.7,0.04)", nlpar = "k", coef = "Intercept"))

      # Weibull
      if ("all" %in% tolower(fixed.effect)){
        priors.list[["weibull"]] <-    priors.list[["weibull"]] +

          set_prior("normal(0,0.25)",  nlpar = "a", coef = "liana.catlow") +
          set_prior("normal(0,0.25)",  nlpar = "a", coef = "liana.cathigh") +

          set_prior("normal(0,0.0015)",  nlpar = "b", coef = "liana.catlow") +
          set_prior("normal(0,0.0015)",  nlpar = "b", coef = "liana.cathigh") +

          set_prior("normal(0,0.04)",  nlpar = "k", coef = "liana.catlow") +
          set_prior("normal(0,0.04)",  nlpar = "k", coef = "liana.cathigh")

      } else {
        if ("a" %in% tolower(fixed.effect)){
          priors.list[["weibull"]] <- priors.list[["weibull"]] +
            set_prior("normal(0,0.25)",  nlpar = "a", coef = "liana.catlow") +
            set_prior("normal(0,0.25)",  nlpar = "a", coef = "liana.cathigh")

        }

        if ("b" %in% tolower(fixed.effect)){
          priors.list[["weibull"]] <- priors.list[["weibull"]] +
            set_prior("normal(0,0.0015)",  nlpar = "b", coef = "liana.catlow") +
            set_prior("normal(0,0.0015)",  nlpar = "b", coef = "liana.cathigh")

        }

        if ("k" %in% tolower(fixed.effect)){
          priors.list[["weibull"]] <- priors.list[["weibull"]] +
            set_prior("normal(0,0.04)",  nlpar = "k", coef = "liana.catlow") +
            set_prior("normal(0,0.04)",  nlpar = "k", coef = "liana.cathigh")

        }
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

      } else {
        if ("a" %in% tolower(random.effect)){
          priors.list[["weibull"]] <- priors.list[["weibull"]] +
            set_prior("student_t(3, 0, 2.5)", class = "sigma", lb = 0) +
            set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
            set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0, group = "sp")

        }

        if ("b" %in% tolower(random.effect)){
          priors.list[["weibull"]] <- priors.list[["weibull"]] +
            set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
            set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0, group = "sp")

        }

        if ("k" %in% tolower(random.effect)){
          priors.list[["weibull"]] <- priors.list[["weibull"]] +
            set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0) +
            set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0, group = "sp")

        }
      }

    }

    if ("gmm" %in% tolower(names)){

      priors.list[["gmm"]] <- c(set_prior("normal(4, 0.25)",  nlpar = "a", coef = "Intercept"),
                                set_prior("normal(0.75,0.04)", nlpar = "b", coef = "Intercept"),
                                set_prior("normal(25,1.5)", nlpar = "k", coef = "Intercept"))

      # gMM
      if ("all" %in% tolower(fixed.effect)){
        priors.list[["gmm"]] <-   priors.list[["gmm"]] +

          set_prior("normal(0,0.25)",  nlpar = "a", coef = "liana.catlow") +
          set_prior("normal(0,0.25)",  nlpar = "a", coef = "liana.cathigh") +

          set_prior("normal(0,0.04)",  nlpar = "b", coef = "liana.catlow") +
          set_prior("normal(0,0.04)",  nlpar = "b", coef = "liana.cathigh") +

          set_prior("normal(0,1.5)",  nlpar = "k", coef = "liana.catlow") +
          set_prior("normal(0,1.5)",  nlpar = "k", coef = "liana.cathigh")

      } else {
        if ("a" %in% tolower(fixed.effect)){
          priors.list[["gmm"]] <- priors.list[["gmm"]] +
            set_prior("normal(0,0.25)",  nlpar = "a", coef = "liana.catlow") +
            set_prior("normal(0,0.25)",  nlpar = "a", coef = "liana.cathigh")

        }

        if ("b" %in% tolower(fixed.effect)){
          priors.list[["gmm"]] <- priors.list[["gmm"]] +
            set_prior("normal(0,0.04)",  nlpar = "b", coef = "liana.catlow") +
            set_prior("normal(0,0.04)",  nlpar = "b", coef = "liana.cathigh")

        }

        if ("k" %in% tolower(fixed.effect)){
          priors.list[["gmm"]] <- priors.list[["gmm"]] +
            set_prior("normal(0,1.5)",  nlpar = "k", coef = "liana.catlow") +
            set_prior("normal(0,1.5)",  nlpar = "k", coef = "liana.cathigh")

        }

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

      } else{
        if ("a" %in% tolower(random.effect)){
          priors.list[["gmm"]] <- priors.list[["gmm"]] +
            set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0) +
            set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "a", lb = 0, group = "sp")

        }

        if ("b" %in% tolower(random.effect)){
          priors.list[["gmm"]] <- priors.list[["gmm"]] +
            set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0) +
            set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "b", lb = 0, group = "sp")

        }

        if ("k" %in% tolower(random.effect)){
          priors.list[["gmm"]] <- priors.list[["gmm"]] +
            set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0) +
            set_prior("student_t(3, 0, 2.5)", class = "sd", nlpar = "k", lb = 0, group = "sp")

        }
      }

    }
  }


  return(priors.list)

}
