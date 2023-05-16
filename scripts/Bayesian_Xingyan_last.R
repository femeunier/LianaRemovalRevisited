rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyverse)
library(brms)
library(BayesianTools)
# library(bayesplot)
# library(bayestestR)
# library(tidyr)
# library(ggpubr)
# library(minpack.lm)
# library(tidybayes)

data <- read.csv(file <- "./data/allwithspev3.csv")

ggplot(data,
       aes(x = tls.dbh/10, y = field21/10, color = Treatment, fill = Treatment)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  scale_x_log10() +
  scale_y_log10() +
  stat_smooth(method = "lm") +
  theme_bw()

unique(data$Cod.Especies)
most.abundant.species <- data %>%
  group_by(Cod.Especies) %>%
  summarise(N = n(),
            Nremoval = length(which(Treatment == "C")),
            Ncontrol = length(which(Treatment == "R")),
            .groups = "keep") %>%
  arrange(desc(N)) %>%
  mutate(species = case_when(N > 12 & Nremoval >= 6 & Ncontrol >= 6 ~ Cod.Especies,
                             TRUE ~ "Other"))

data.species <- data %>%
  left_join(most.abundant.species %>%
              dplyr::select(-c(N,Nremoval,Ncontrol)),
            by = "Cod.Especies")


prior_ED <- c(set_prior("normal(62,20)",  nlpar = "href",lb = 0),
              set_prior("normal(0.0352,0.02)", nlpar = "b1Ht",lb = 0),
              set_prior("normal(0.694,0.3)", nlpar = "b2Ht",lb = 0))

ed_form1 <- brmsformula(tls.h ~ href*(1-exp(-b1Ht*(tls.dbh**b2Ht))),
                        href + b1Ht + b2Ht ~ 1 + (1 |species),
                        nl = TRUE)

ed_form2 <- brmsformula(tls.h ~ href*(1-exp(-b1Ht*(tls.dbh**b2Ht))),
                        b1Ht + b2Ht ~ 1 + (1 |species),
                        href ~ 1 + Treatment + (1 |species),
                        nl = TRUE)

ed_form8 <- brmsformula(tls.h ~ href*(1-exp(-b1Ht*(tls.dbh**b2Ht))),
                           href ~ 1 + Treatment + (1 |species),
                           b1Ht ~ 1 + Treatment + (1 |species),
                           b2Ht ~ 1 + Treatment + (1 |species),
                           nl = TRUE)

ed_fit1 = brm(ed_form1,
              data = data.species,
              prior = prior_ED,
              chains = 2, iter = 5000,
              control = list(adapt_delta = 0.95,max_treedepth = 20))

ed_fit2 = brm(ed_form2,
              data = data.species,
              prior = prior_ED,
              chains = 2, iter = 5000,
              control = list(adapt_delta = 0.95,max_treedepth = 20))


ed_fit8 = brm(ed_form8,
              data=data.species,
              prior=prior_ED,
              chains = 2, iter = 5000,
              control = list(adapt_delta = 0.99,max_treedepth = 20))

plot(ed_fit1)
plot(ed_fit8)

waic1 <- waic(ed_fit1)
waic2 <- waic(ed_fit2)
waic8 <- waic(ed_fit8)


# compare both models
loo_compare(waic1,waic2,waic8)

df.QoF <- bind_rows(list(
  data.frame(obs = data.species$tls.h,
             fit = predict(ed_fit8)[,"Estimate"],
             low = predict(ed_fit8)[,"Q2.5"],
             high = predict(ed_fit8)[,"Q97.5"],
             model = "best"),
  data.frame(obs = data.species$tls.h,
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

df.QoF.diff <- df.QoF %>% mutate(diff = obs - fit,
                                 fit_low = obs - low,
                                 fit_high = obs - high)

ggplot(data = df.QoF.diff,
       aes(x = obs,
           y = diff,
           color = model)) +
  geom_point() +
  geom_errorbar(aes(ymin = fit_low, ymax = fit_high),width = 0) +
  geom_abline(slope = 0, intercept = 0,linetype = 2) +
  theme_bw()

ggplot(data = df.QoF.diff,
       aes(x = abs(diff),
           fill = model,
           color = model)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = 0,linetype = 2) +
  theme_bw()

df.QoF.diff %>% filter(model == "best") %>% pull(diff) %>% abs() %>% summary()
df.QoF.diff %>% filter(model == "simplest") %>% pull(diff) %>% abs() %>% summary()


dbhs <- seq(20,200)

df.fit <- bind_rows(list(
  data.frame(dbh = dbhs,
             fit = predict(ed_fit1,newdata = data.frame(dbh = dbhs))[,"Estimate"],
             low = predict(ed_fit1,newdata = data.frame(dbh = dbhs))[,"Q2.5"],
             high = predict(ed_fit1,newdata = data.frame(dbh = dbhs))[,"Q97.5"],
             model = "simplest",
             type = "all"),
  data.frame(dbh = c(dbhs,dbhs),
             fit = predict(ed_fit8,newdata = data.frame(dbh = dbhs,liana = c(rep(FALSE,length(dbhs)),
                                                                             rep(TRUE,length(dbhs)))))[,"Estimate"],
             low = predict(ed_fit8,newdata = data.frame(dbh = dbhs,liana = c(rep(FALSE,length(dbhs)),
                                                                             rep(TRUE,length(dbhs)))))[,"Q2.5"],
             high = predict(ed_fit8,newdata = data.frame(dbh = dbhs,liana = c(rep(FALSE,length(dbhs)),
                                                                              rep(TRUE,length(dbhs)))))[,"Q97.5"],
             model = "best",
             type = c(rep("FALSE",length(dbhs)),rep("TRUE",length(dbhs))))))


ggplot() +
  geom_point(data = trees.extracted,
             aes(x = dbh, y = Height, color = as.character(liana))) +
  geom_line(data = df.fit,
            aes(x = dbh,
                y = fit,
                color = type),size = 1) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()


