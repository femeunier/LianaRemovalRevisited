setup.default.ub <- function(model,default_prior){

  ub <- default_prior[["ub"]]

  if (tolower(model) %in% c("weibull","gmm")){
    ub[with(default_prior,
            nlpar %in% c("b","k") &
              class == "b" &
              grepl("liana",coef))]<- as.character(1)

    ub[with(default_prior,
            nlpar %in% c("a") &
              class == "b" &
              grepl("liana",coef))]<- as.character(50)
  } else {

    ub[with(default_prior,
            nlpar %in% c("b") &
              class == "b" &
              grepl("liana",coef))]<- as.character(0.5)

    ub[with(default_prior,
            nlpar %in% c("a") &
              class == "b" &
              grepl("liana",coef))]<- as.character(1)

  }


  return(ub)

}



