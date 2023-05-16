default.forms <- function(names,
                          fixed.effect = TRUE,
                          random.effect = FALSE){

  form.list <- list()


  # Power function form
  if ("power" %in% tolower(names)){

    if (fixed.effect & !random.effect){

      form.list[["power"]] <- brmsformula(h ~ a*(dbh**b),
                                          a + b  ~ 1 + liana.cat,
                                          nl = TRUE)
    } else if (!fixed.effect & random.effect){

      form.list[["power"]] <- brmsformula(h ~ a*(dbh**b),
                                          a + b  ~ 1 + (1 | sp),
                                          nl = TRUE)

    } else if (fixed.effect & random.effect){
      form.list[["power"]] <- brmsformula(h ~ a*(dbh**b),
                                          a + b ~ 1 + liana.cat + (1 | sp),
                                          nl = TRUE)
    }
  }

  if ("weibull" %in% tolower(names)){

    # Weibull
    if (fixed.effect & !random.effect){

      form.list[["weibull"]] <- brmsformula(h ~ a*(1-exp(-b*(dbh**k))),
                                            a + b + k ~ 1 + liana.cat,
                                            nl = TRUE)
    } else if (!fixed.effect & random.effect){


      form.list[["weibull"]] <- brmsformula(h ~ a*(1-exp(-b*(dbh**k))),
                                            a + b + k ~ 1 + (1 | sp),
                                            nl = TRUE)

    } else if (fixed.effect & random.effect){


      form.list[["weibull"]] <- brmsformula(h ~ a*(1-exp(-b*(dbh**k))),
                                            a + b + k ~ 1 + liana.cat + (1 | sp),
                                            nl = TRUE)
    }
  }

  if ("gmm" %in% tolower(names)){

    # gMM

    if (fixed.effect & !random.effect){

      form.list[["gmm"]] <- brmsformula(h ~ (a*(dbh**b))/(k + dbh**b),
                                        a + b + k ~ 1 + liana.cat,
                                        nl = TRUE)
    } else if (!fixed.effect & random.effect){


      form.list[["gmm"]] <- brmsformula(h ~ (a*(dbh**b))/(k + dbh**b),
                                        a + b + k ~ 1 + (1 | sp),
                                        nl = TRUE)

    } else if (fixed.effect & random.effect){


      form.list[["gmm"]] <- brmsformula(h ~ (a*(dbh**b))/(k + dbh**b),
                                        a + b + k ~ 1 + liana.cat + (1 | sp),
                                        nl = TRUE)
    }
  }

  return(form.list)

}
