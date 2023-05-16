rm(list = ls())

library(brms)
library(dplyr)
library(ggplot2)
library(BayesianTools)
library(bayesplot)
library(bayestestR)

trees.extracted <- bind_rows(list(read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/plot11_2016_tree.csv") %>%
                                    dplyr::select(DBH_HT.cm.,Height) %>%
                                    rename(dbh = DBH_HT.cm.) %>%
                                    mutate(plot = 11,
                                           treatment = "removal"),
                                  read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/plot13_para.csv") %>%
                                    dplyr::select(DBH_LSR,Height) %>%
                                    rename(dbh = DBH_LSR) %>%
                                    mutate(plot = 13,
                                           treatment = "control"))) %>%
  mutate(liana = case_when(treatment == "control" ~ TRUE,
                            TRUE ~ FALSE))

##################################################################################################
# MM-type of allometry


prior_1 <- c(set_prior("normal(4, 3.5)",  nlpar = "Vm"),
                  set_prior("normal(1.2,1)",   nlpar = "x"),
                  set_prior("normal(35,25)",   nlpar = "K"))

prior_mm <- rbind(prior_1)

mm_form1 <- brmsformula(log(Height) ~ (Vm + x * log(dbh))-log(K+(dbh^x)),
                        Vm + x + K ~ 1,
                        nl = TRUE)

mm_form2 <- brmsformula(log(Height) ~ (Vm + x * log(dbh))-log(K+(dbh^x)),
                        x + K ~ 1,
                        Vm ~ 1 + liana,
                        nl = TRUE)

mm_form3 <- brmsformula(log(Height) ~ (Vm + x * log(dbh))-log(K+(dbh^x)),
                        Vm + K ~ 1,
                        x ~ 1 + liana,
                        nl = TRUE)

mm_form4 <- brmsformula(log(Height) ~ (Vm + x * log(dbh))-log(K+(dbh^x)),
                        Vm + x ~ 1,
                        K ~ 1 + liana,
                        nl = TRUE)

mm_form5 <- brmsformula(log(Height) ~ (Vm + x * log(dbh))-log(K+(dbh^x)),
                        K ~ 1,
                        Vm + x ~ 1 + liana,
                        nl = TRUE)


mm_form6 <- brmsformula(log(Height) ~ (Vm + x * log(dbh))-log(K+(dbh^x)),
                        Vm ~ 1,
                        x + K ~ 1 + liana,
                        nl = TRUE)

mm_form7 <- brmsformula(log(Height) ~ (Vm + x * log(dbh))-log(K+(dbh^x)),
                        x ~ 1,
                        Vm + K ~ 1 + liana,
                        nl = TRUE)

mm_form8 <- brmsformula(log(Height) ~ (Vm + x * log(dbh))-log(K+(dbh^x)),
                        Vm + x + K ~ 1 + liana,
                        nl = TRUE)

mm_fit1 = brm(mm_form1,
                   data = trees.extracted,
                   prior = prior_1,
                   chains = 2, iter = 5000,
                   control = list(adapt_delta = 0.95,max_treedepth = 20))

mm_fit2 = brm(mm_form2,
                   data = trees.extracted,
                   prior = prior_1,
                   chains = 2, iter = 5000,
                   control = list(adapt_delta = 0.95,max_treedepth = 20))

mm_fit3 = brm(mm_form3,
                   data = trees.extracted,
                   prior = prior_1,
                   chains = 2, iter = 5000,
                   control = list(adapt_delta = 0.95,max_treedepth = 20))

mm_fit4 = brm(mm_form4,
                   data = trees.extracted,
                   prior = prior_1,
                   chains = 2, iter = 5000,
                   control = list(adapt_delta = 0.95,max_treedepth = 20))

mm_fit5 = brm(mm_form5,
                   data = trees.extracted,
                   prior = prior_1,
                   chains = 2, iter = 5000,
                   control = list(adapt_delta = 0.95,max_treedepth = 20))

mm_fit6 = brm(mm_form6,
                   data = trees.extracted,
                   prior = prior_1,
                   chains = 2, iter = 5000,
                   control = list(adapt_delta = 0.95,max_treedepth = 20))

mm_fit7 = brm(mm_form7,
                   data = trees.extracted,
                   prior = prior_1,
                   chains = 2, iter = 5000,
                   control = list(adapt_delta = 0.95,max_treedepth = 20))

mm_fit8 = brm(mm_form8,
                   data=trees.extracted,
                   prior=prior_1,
                   chains = 2, iter = 5000,
                   control = list(adapt_delta = 0.99,max_treedepth = 20))

waic1_mm <- waic(mm_fit1_2019)
waic2_mm <- waic(mm_fit2_2019)
waic3_mm <- waic(mm_fit3_2019)
waic4_mm <- waic(mm_fit4_2019)
waic5_mm <- waic(mm_fit5_2019)
waic6_mm <- waic(mm_fit6_2019)
waic7_mm <- waic(mm_fit7_2019)
waic8_mm <- waic(mm_fit8_2019)

# compare both models
loo_compare(waic1_mm, waic2_mm, waic3_mm, waic4_mm, waic5_mm, waic6_mm, waic7_mm, waic8_mm)


df.QoF <- bind_rows(list(
  data.frame(obs = trees.extracted$Height,
             fit = predict(mm_fit7_2019)[,"Estimate"],
             low = predict(mm_fit7_2019)[,"Q2.5"],
             high = predict(mm_fit7_2019)[,"Q97.5"],
             model = "best"),
  data.frame(obs = trees.extracted$Height,
             fit = predict(mm_fit1_2019)[,"Estimate"],
             low = predict(mm_fit1_2019)[,"Q2.5"],
             high = predict(mm_fit1_2019)[,"Q97.5"],
             model = "simplest")))

ggplot(data = df.QoF,
       aes(x = obs,
           y = fit,
           color = model)) +
  geom_point() +
  geom_errorbar(aes(ymin = low, ymax = high),width = 0) +
  geom_abline(slope = 1, intercept = 0,linetype = 2) +
  theme_bw()

dbhs <- seq(5,150)

df.fit <- bind_rows(list(
  data.frame(dbh = dbhs,
             fit = exp(predict(mm_fit1_2019,newdata = data.frame(dbh = dbhs))[,"Estimate"]),
             low = exp(predict(mm_fit1_2019,newdata = data.frame(dbh = dbhs))[,"Q2.5"]),
             high = exp(predict(mm_fit1_2019,newdata = data.frame(dbh = dbhs))[,"Q97.5"]),
             model = "simplest",
             type = "all"),
  data.frame(dbh = c(dbhs,dbhs),
             fit = exp(predict(mm_fit7_2019,newdata = data.frame(dbh = dbhs,liana = c(rep(FALSE,length(dbhs)),
                                                                                  rep(TRUE,length(dbhs)))))[,"Estimate"]),
             low = exp(predict(mm_fit7_2019,newdata = data.frame(dbh = dbhs,liana = c(rep(FALSE,length(dbhs)),
                                                                                  rep(TRUE,length(dbhs)))))[,"Q2.5"]),
             high = exp(predict(mm_fit7_2019,newdata = data.frame(dbh = dbhs,liana = c(rep(FALSE,length(dbhs)),
                                                                                   rep(TRUE,length(dbhs)))))[,"Q97.5"]),
             model = "best",
             type = c(rep("FALSE",length(dbhs)),rep("TRUE",length(dbhs))))))


ggplot() +
  # geom_ribbon(data = df.fit,
  #             aes(x = dbh,
  #                 y = fit,
  #                 color = type,
  #                 ymin = low, ymax = high, fill = type),color = NA,alpha = 0.4) +
  geom_point(data = trees.extracted,
             aes(x = dbh, y = Height, color = as.character(liana))) +
  geom_line(data = df.fit,
            aes(x = dbh,
                y = fit,
                color = type),size = 1) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()


##################################################################################################
# ED2-type of allometry

prior_ED <- c(set_prior("normal(62,20)",  nlpar = "href"),
                   set_prior("normal(0.0352,0.02)", nlpar = "b1Ht"),
                   set_prior("normal(0.694,0.3)", nlpar = "b2Ht"))

ed_form1 <- brmsformula(Height ~ href*(1-exp(-b1Ht*(dbh**b2Ht))),
                        href + b1Ht + b2Ht ~ 1,
                        nl = TRUE)

ed_form2 <- brmsformula(Height ~ href*(1-exp(-b1Ht*(dbh**b2Ht))),
                        href + b2Ht ~ 1,
                        b1Ht ~ 1 + liana,
                        nl = TRUE)

ed_form3 <- brmsformula(Height ~ href*(1-exp(-b1Ht*(dbh**b2Ht))),
                        b1Ht + b2Ht ~ 1,
                        href ~ 1 + liana,
                        nl = TRUE)

ed_form4 <- brmsformula(Height ~ href*(1-exp(-b1Ht*(dbh**b2Ht))),
                        b1Ht + href ~ 1,
                        b2Ht ~ 1 + liana,
                        nl = TRUE)

ed_form5 <- brmsformula(Height ~ href*(1-exp(-b1Ht*(dbh**b2Ht))),
                        href ~ 1,
                        b1Ht + b2Ht ~ 1 + liana,
                        nl = TRUE)

ed_form6 <- brmsformula(Height ~ href*(1-exp(-b1Ht*(dbh**b2Ht))),
                        b1Ht ~ 1,
                        href + b2Ht ~ 1 + liana,
                        nl = TRUE)

ed_form7 <- brmsformula(Height ~ href*(1-exp(-b1Ht*(dbh**b2Ht))),
                        b2Ht ~ 1,
                        href + b1Ht ~ 1 + liana,
                        nl = TRUE)

ed_form8 <- brmsformula(Height ~ href*(1-exp(-b1Ht*(dbh**b2Ht))),
                        href + b1Ht + b2Ht ~ 1 + liana,
                        nl = TRUE)

ed_fit1 = brm(ed_form1,
                   data = trees.extracted,
                   prior = prior_ED,
                   chains = 2, iter = 5000,
                   control = list(adapt_delta = 0.95,max_treedepth = 20))

ed_fit2 = brm(ed_form2,
                   data = trees.extracted,
                   prior = prior_ED,
                   chains = 2, iter = 5000,
                   control = list(adapt_delta = 0.95,max_treedepth = 20))

ed_fit3 = brm(ed_form3,
                   data = trees.extracted,
                   prior = prior_ED,
                   chains = 2, iter = 5000,
                   control = list(adapt_delta = 0.95,max_treedepth = 20))

ed_fit4 = brm(ed_form4,
                   data = trees.extracted,
                   prior = prior_ED,
                   chains = 2, iter = 5000,
                   control = list(adapt_delta = 0.99,max_treedepth = 20))

ed_fit5 = brm(ed_form5,
                   data = trees.extracted,
                   prior = prior_ED,
                   chains = 2, iter = 5000,
                   control = list(adapt_delta = 0.95,max_treedepth = 20))

ed_fit6 = brm(ed_form6,
                   data = trees.extracted,
                   prior = prior_ED,
                   chains = 2, iter = 5000,
                   control = list(adapt_delta = 0.95,max_treedepth = 20))

ed_fit7 = brm(ed_form7,
                   data = trees.extracted,
                   prior = prior_ED,
                   chains = 2, iter = 5000,
                   control = list(adapt_delta = 0.95,max_treedepth = 20))

ed_fit8 = brm(ed_form8,
                   data=trees.extracted,
                   prior=prior_ED,
                   chains = 2, iter = 5000,
                   control = list(adapt_delta = 0.99,max_treedepth = 20))

plot(ed_fit7)

waic1 <- waic(ed_fit1)
waic2 <- waic(ed_fit2)
waic3 <- waic(ed_fit3)
waic4 <- waic(ed_fit4)
waic5 <- waic(ed_fit5)
waic6 <- waic(ed_fit6)
waic7 <- waic(ed_fit7)
waic8 <- waic(ed_fit8)

# compare both models
loo_compare(waic1, waic2, waic3, waic5, waic6, waic7, waic8)

df.QoF <- bind_rows(list(
  data.frame(obs = trees.extracted$Height,
             fit = predict(ed_fit7)[,"Estimate"],
             low = predict(ed_fit7)[,"Q2.5"],
             high = predict(ed_fit7)[,"Q97.5"],
             model = "best"),
  data.frame(obs = trees.extracted$Height,
             fit = predict(ed_fit1)[,"Estimate"],
             low = predict(ed_fit1)[,"Q2.5"],
             high = predict(ed_fit1)[,"Q97.5"],
             model = "simplest")))

ggplot(data = df.QoF,
       aes(x = obs,
           y = fit,
           color = model)) +
  geom_point() +
  geom_errorbar(aes(ymin = low, ymax = high),width = 0) +
  geom_abline(slope = 1, intercept = 0,linetype = 2) +
  theme_bw()

dbhs <- seq(5,150)

df.fit <- bind_rows(list(
  data.frame(dbh = dbhs,
             fit = predict(ed_fit1,newdata = data.frame(dbh = dbhs))[,"Estimate"],
             low = predict(ed_fit1,newdata = data.frame(dbh = dbhs))[,"Q2.5"],
             high = predict(ed_fit1,newdata = data.frame(dbh = dbhs))[,"Q97.5"],
             model = "simplest",
             type = "all"),
  data.frame(dbh = c(dbhs,dbhs),
             fit = predict(ed_fit7,newdata = data.frame(dbh = dbhs,liana = c(rep(FALSE,length(dbhs)),
                                                                                  rep(TRUE,length(dbhs)))))[,"Estimate"],
             low = predict(ed_fit7,newdata = data.frame(dbh = dbhs,liana = c(rep(FALSE,length(dbhs)),
                                                                                  rep(TRUE,length(dbhs)))))[,"Q2.5"],
             high = predict(ed_fit7,newdata = data.frame(dbh = dbhs,liana = c(rep(FALSE,length(dbhs)),
                                                                                   rep(TRUE,length(dbhs)))))[,"Q97.5"],
             model = "best",
             type = c(rep("FALSE",length(dbhs)),rep("TRUE",length(dbhs))))))


ggplot() +
  # geom_ribbon(data = df.fit,
  #             aes(x = dbh,
  #                 y = fit,
  #                 color = type,
  #                 ymin = low, ymax = high, fill = type),color = NA,alpha = 0.4) +
  geom_point(data = trees.extracted,
             aes(x = dbh, y = Height, color = as.character(liana))) +
  geom_line(data = df.fit,
            aes(x = dbh,
                y = fit,
                color = type),size = 1) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

save.image("./outputs/BayesFit.RData")


# post <- posterior_samples(ed_fit2, add_chain = T)
# mcmc_acf(post, lags = 35)
# pairs(ed_fit2,
#       off_diag_args = list(size = 1/3, alpha = 1/3))
