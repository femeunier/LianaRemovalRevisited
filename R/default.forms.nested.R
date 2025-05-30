default.forms.nested <- function(names = c("weibull","power","gmm"),
                          fixed.effect = "all",
                          random.effect = "all",
                          model.output = "logh",
                          site.re = FALSE){

  form.list <- list()
  f.functional.form <- list(
    power = paste0(model.output," ~ a + b*log(dbh)"),
    weibull = paste0(model.output," ~ a + log(1 - exp(-b*(dbh^k)))"),
    gmm = paste0(model.output," ~ (a + b * log(dbh))-log(k+(dbh^b))"),
    power.h = paste0(model.output," ~ a + b * log(dbh) + k*log(h)")
  )

  if (any(fixed.effect == "all")){
    fixed.effect.params <- c("a","b","k")
  } else {
    fixed.effect.params <- fixed.effect
  }

  if (any(random.effect == "all")){
    random.effect.params <- c("a","b","k")
  } else {
    random.effect.params <- random.effect
  }


  mixed.effect.params <- intersect(fixed.effect.params,random.effect.params)
  fixed.effect.params <- setdiff(fixed.effect.params, mixed.effect.params)
  random.effect.params <- setdiff(random.effect.params, mixed.effect.params)
  missing.params <- setdiff(c("a","b","k"),
                            c(random.effect.params,
                              fixed.effect.params,
                              mixed.effect.params))


  addition <- ifelse(site.re,
                     "site/sp",
                     "sp")

  # Power function form
  if ("power" %in% tolower(names)){

    cmixed.effect.params <- mixed.effect.params[mixed.effect.params %in% c("a","b")]
    cfixed.effect.params <- fixed.effect.params[fixed.effect.params %in% c("a","b")]
    crandom.effect.params <- random.effect.params[random.effect.params %in% c("a","b")]
    cmissing.params <- missing.params[missing.params %in% c("a","b")]

    if (length(cmixed.effect.params) == 0){
      f.mixed.effect <- cmixed.effect.params
    } else{
      f.mixed.effect <- paste(paste(cmixed.effect.params, collapse = " + "),
                              paste0("~ (1 | liana.cat| ",addition,")"))
    }

    if (length(cfixed.effect.params) == 0){
      f.fixed.effect <- cfixed.effect.params
    } else{
      f.fixed.effect <- paste(paste(cfixed.effect.params, collapse = " + "),
                              "~ (1 | liana.cat)")
    }

    if (length(crandom.effect.params) == 0){
      f.random.effect <- crandom.effect.params
    } else{
      f.random.effect <- paste(paste(crandom.effect.params, collapse = " + "),
                               paste0("~ (1 | ",addition,")"))
    }

    all.effects <- c(f.mixed.effect,
                     f.fixed.effect,
                     f.random.effect)

    if (length(cmissing.params) >= 1){
      all.effects <- c(all.effects,
                       paste(paste(cmissing.params, collapse = "+"), "~ 1"))
    }

    if (length(all.effects) == 1){

      form.list[["power"]] <- brmsformula(f.functional.form[["power"]],
                                          all.effects[1],
                                          nl = TRUE)
    } else if (length(all.effects) == 2){

      form.list[["power"]] <- brmsformula(f.functional.form[["power"]],
                                          all.effects[1],all.effects[2],
                                          nl = TRUE)
    }


  }

  # Weibull
  if ("weibull" %in% tolower(names)){
    cmixed.effect.params <- mixed.effect.params[mixed.effect.params %in% c("a","b","k")]
    cfixed.effect.params <- fixed.effect.params[fixed.effect.params %in% c("a","b","k")]
    crandom.effect.params <- random.effect.params[random.effect.params %in% c("a","b","k")]
    cmissing.params <- missing.params[missing.params %in% c("a","b","k")]

    if (length(cmixed.effect.params) == 0){
      f.mixed.effect <- cmixed.effect.params
    } else{
      f.mixed.effect <- paste(paste(cmixed.effect.params, collapse = " + "),
                              paste0("~ (1 | liana.cat| ",addition,")"))
    }

    if (length(cfixed.effect.params) == 0){
      f.fixed.effect <- cfixed.effect.params
    } else{
      f.fixed.effect <- paste(paste(cfixed.effect.params, collapse = " + "),
                              "~ (1 | liana.cat)")
    }

    if (length(crandom.effect.params) == 0){
      f.random.effect <- crandom.effect.params
    } else{
      f.random.effect <- paste(paste(crandom.effect.params, collapse = " + "),
                               paste0("~ (1 + ",addition,")"))
    }

    all.effects <- c(f.mixed.effect,
                     f.fixed.effect,
                     f.random.effect)


    if (length(cmissing.params) >= 1){
      all.effects <- c(all.effects,
                       paste(paste(cmissing.params, collapse = "+"), "~ 1"))
    }

    if (length(all.effects) == 1){

      form.list[["weibull"]] <- brmsformula(f.functional.form[["weibull"]],
                                            all.effects[1],
                                            nl = TRUE)
    } else if (length(all.effects) == 2){

      form.list[["weibull"]] <- brmsformula(f.functional.form[["weibull"]],
                                            all.effects[1],all.effects[2],
                                            nl = TRUE)
    } else if (length(all.effects) == 3){

      form.list[["weibull"]] <- brmsformula(f.functional.form[["weibull"]],
                                            all.effects[1],all.effects[2],all.effects[3],
                                            nl = TRUE)
    }
  }

  # gMM
  if ("gmm" %in% tolower(names)){

    cmixed.effect.params <- mixed.effect.params[mixed.effect.params %in% c("a","b","k")]
    cfixed.effect.params <- fixed.effect.params[fixed.effect.params %in% c("a","b","k")]
    crandom.effect.params <- random.effect.params[random.effect.params %in% c("a","b","k")]
    cmissing.params <- missing.params[missing.params %in% c("a","b","k")]

    if (length(cmixed.effect.params) == 0){
      f.mixed.effect <- cmixed.effect.params
    } else{
      f.mixed.effect <- paste(paste(cmixed.effect.params, collapse = " + "),
                              paste0("~ (1 | liana.cat |",addition,")"))
    }


    if (length(cfixed.effect.params) == 0){
      f.fixed.effect <- cfixed.effect.params
    } else{
      f.fixed.effect <- paste(paste(cfixed.effect.params, collapse = " + "),
                              "~ (1 | liana.cat)")
    }

    if (length(crandom.effect.params) == 0){
      f.random.effect <- crandom.effect.params
    } else{
      f.random.effect <- paste(paste(crandom.effect.params, collapse = " + "),
                               paste0("~( 1  | ",addition,")"))
    }

    all.effects <- c(f.mixed.effect,
                     f.fixed.effect,
                     f.random.effect)


    if (length(cmissing.params) >= 1){
      all.effects <- c(all.effects,
                       paste(paste(cmissing.params, collapse = "+"), "~ 1"))
    }

    if (length(all.effects) == 1){

      form.list[["gmm"]] <- brmsformula(f.functional.form[["gmm"]],
                                        all.effects[1],
                                        nl = TRUE)
    } else if (length(all.effects) == 2){

      form.list[["gmm"]] <- brmsformula(f.functional.form[["gmm"]],
                                        all.effects[1],all.effects[2],
                                        nl = TRUE)
    } else if (length(all.effects) == 3){

      form.list[["gmm"]] <- brmsformula(f.functional.form[["gmm"]],
                                        all.effects[1],all.effects[2],all.effects[3],
                                        nl = TRUE)
    }
  }





  return(form.list)

}
