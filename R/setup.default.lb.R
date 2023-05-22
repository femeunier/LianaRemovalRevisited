setup.default.lb <- function(model,default_prior){

  lb <- default_prior[["lb"]]

  if (tolower(model) %in% c("weibull","gmm")){
    lb[with(default_prior,
            nlpar %in% c("b","k") &
              class == "b" &
              grepl("liana",coef))]<- as.character(-1)

    lb[with(default_prior,
            nlpar %in% c("a") &
              class == "b" &
              grepl("liana",coef))]<- as.character(-25)
  } else {

    lb[with(default_prior,
            nlpar %in% c("b") &
              class == "b" &
              grepl("liana",coef))]<- as.character(-0.5)

    lb[with(default_prior,
            nlpar %in% c("a") &
              class == "b" &
              grepl("liana",coef))]<- as.character(-2)

  }

  lb[with(default_prior,
          nlpar %in% c("a","b","k") &
            class == "b" &
            coef == "Intercept")]<- as.character(0)



  return(lb)

}



