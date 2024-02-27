rm(list = ls())

library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(brms)
library(bayesplot)

Slenderness <- readRDS("./outputs/Slenderness.RDS") %>%
  mutate(DBH_class = as.factor(floor(dbh/10))) %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high")))


Model <- glmer((S) ~ dbh*liana.cat + (1 | site/sp), family = Gamma(link = "log"),
               data = Slenderness)
#
# Model <- glmer(log(S) ~ liana.cat + (dbh | site/sp),
#                data = Slenderness)

summary(Model)
anova(Model)
Model0<-fixef(Model)
res_lmeFat0=residuals(Model)
qqnorm(res_lmeFat0,main="MLEFat0")
qqline(res_lmeFat0)
hist(res_lmeFat0,main="MLEFat0")
plot(Slenderness$dbh,res_lmeFat0)
shapiro.test(res_lmeFat0)
plot(Model)

library('sjPlot')

plot_model(Model,type='eff')
plot_model(Model,type='pre')


ggplot(data = Slenderness) +
  geom_boxplot(aes(x = DBH_class, y = S)) +
  theme_bw()

OL <- Slenderness %>%
  # group_by(DBH_class) %>%
  mutate(Q1 = quantile(S,0.25),
         Q3 = quantile(S,0.75)) %>%
  mutate(IQR = Q3 - Q1) %>%
  mutate(is.outlier = S < (Q1 - 1.5*IQR) | S > (Q3 + 1.5*IQR))

OL %>%
  group_by(site) %>%
  summarise(N_ol = sum(is.outlier),
            N_ol_small = sum(is.outlier[dbh < 20]),
            N = n()) %>%
  mutate(frac = N_ol/N) %>%
  arrange(desc(frac))

Nchains <- 4
Niter <- 5000
control.list <- list(adapt_delta = 0.8,
                     max_treedepth = 10)

# cfit <- brm(S ~ liana.cat + (1|site/sp) + (1|DBH_class),
#             data=Slenderness %>%
#               ungroup() %>%
#               slice_sample(prop = 0.1),
#             cores = min(Nchains,
#                         parallel::detectCores() - 1),
#             family = Gamma(link = "log"),
#             # prior = priors.list[[model]],
#             control = control.list,
#             chains = Nchains,
#             iter = Niter,
#             silent = 2)

# To test : weibull, skew_normal

cfit <- brm(S ~ dbh*liana.cat+(1|site/sp),
            data = Slenderness %>%
              ungroup() %>%
              slice_sample(prop = 1),
            cores = min(Nchains,
                        parallel::detectCores() - 1),
            family = Gamma(link = "log"),
            # prior = priors.list[[model]],
            control = control.list,
            chains = Nchains,
            iter = Niter,
            silent = 2)

cfit <- readRDS("./outputs/Slenderness.Fit.RDS")
alpha = 0.11
fixef(cfit,
      probs = c(alpha/2, 1-alpha/2))


cdata <- cfit$data

cdata %>%
  # mutate(S = h/dbh) %>%
  group_by(liana.cat) %>%
  summarise(m = mean(S),
            med = median(S))


# saveRDS(cfit,
#         "./outputs/Slenderness.Fit3.RDS")

plot(cfit)
pp_check(cfit,ndraws = 100)

# system2("rsync",
#         paste("-avz",
#               "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/Slenderness.Fit.RDS",
#               "./outputs/"))

# cfit <- readRDS("./outputs/Slenderness.Fit.RDS")

# plot(cfit)
pp_check(cfit,ndraws = 1000)

################################################################################
# rm(list = ls())
#
# library(lme4)
# library(dplyr)
#
# Slenderness <- readRDS("./outputs/Slenderness.RDS") %>%
#   mutate(DBH_class = as.factor(floor(dbh/10))) %>%
#   mutate(liana.cat = factor(liana.cat,
#                             levels = c("no","low","high")))
#
#
# Model <- glmer((S) ~ liana.cat + (1 | site/sp), family = Gamma(link = "log"),
#                data = Slenderness)
#
# summary(Model)
# anova(Model)
# Model0<-fixef(Model)
# res_lmeFat0=residuals(Model)
# qqnorm(res_lmeFat0,main="MLEFat0")
# qqline(res_lmeFat0)
# hist(res_lmeFat0,main="MLEFat0")
# plot(Slenderness$dbh,res_lmeFat0)
# shapiro.test(res_lmeFat0)
# plot(Model)

# saveRDS(cfit,
#         "./outputs/Slenderness.Fit.RDS")

# scp /home/femeunier/Documents/projects/LianaRemovalRevisited/scripts/Slenderness.fit.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/




# lme_S <- lmerTest::lmer((S)~ liana.cat + (1|site/sp),
#                         data =Slenderness,REML=FALSE)
#
#
# summary(lme_S)
# anova(lme_S)
# Model0<-fixef(lme_S)
# res_lmeFat0=residuals(lme_S)
# qqnorm(res_lmeFat0,main="MLEFat0")
# qqline(res_lmeFat0)
# hist(res_lmeFat0,main="MLEFat0")
# plot(Slenderness$dbh,res_lmeFat0)
# shapiro.test(res_lmeFat0)
# plot(predict(lme_S),res_lmeFat0)
