rm(list = ls())

library(dplyr)
library(ggplot2)
library(brms)
library(reshape2)
library(stringr)
library(ggridges)

set.seed(2310)

# Load data
data <- read.csv("~/Documents/projects/LianaRemovalRevisited/data/2019_TLS_data.csv") %>%
  dplyr::select(Species,Final.DBH..cm.,Liana,Height..m.,CPA..m2.) %>%
  rename(dbh = Final.DBH..cm.,
         crown.area = CPA..m2.,
         h = Height..m.) %>%
  mutate(logh = log(h),
         Liana = as.factor(Liana))
levels(data$Liana) <- c("no","low","high")

ggplot(data,
       aes(x = dbh,
           y = h, color = Liana, fill = Liana)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  stat_smooth(method = "lm",se = TRUE) +
  theme_bw()

# BRMS parameters
Nchains = 2
control.list <- list(adapt_delta = 0.8,
                     max_treedepth = 10)
Niter <- 5000


# Default model form and priors
form <- brmsformula(logh ~ a + log(1 - exp(-b*(dbh^k))),
                    a + b + k ~ 1,
                    nl = TRUE)

priors <-  c(set_prior("normal(4,2)",
                       nlpar = "a",
                       coef = "Intercept"),
             set_prior("normal(0.03,0.02)",
                       nlpar = "b",
                       coef = "Intercept"),
             set_prior("normal(0.7,0.2)", nlpar = "k", coef = "Intercept"))

fit0 <- brm(form,
            data,
            cores = min(Nchains,
                        parallel::detectCores() - 1),
            prior = priors,
            control = control.list,
            chains = Nchains,
            iter = Niter,
            silent = 2)


#  Modified model
form.mod <- brmsformula(logh ~ a + log(1 - exp(-b*(dbh^k))),
                        a ~ 1 + Liana,
                        b + k ~ 1,
                        nl = TRUE)

priors.mod <-  priors + c(
  set_prior("normal(0,0.5)",  nlpar = "a", coef = "Lianalow"),
  set_prior("normal(0,0.5)",  nlpar = "a", coef = "Lianahigh"))

fit.mod <- brm(form.mod,
               data,
               cores = min(Nchains,
                           parallel::detectCores() - 1),
               prior = priors.mod,
               control = control.list,
               chains = Nchains,
               iter = Niter,
               silent = 2)

###################################################################################
# Compare fits
fit.all <- list(null.model = fit0,
                Liana.model = fit.mod)
comparison <- loo_compare(lapply(fit.all,
                                 LOO))
comparison

best.model.names <- rownames(comparison)[1]
best.model <- fit.all[[ best.model.names]]
null.model <- fit.all[["null.model"]]
pp.check.best <- pp_check(best.model, ndraws = 500)
pp.check.best # visual check
pp_check(null.model, ndraws = 500)

# Posterior distributions
temp <- as.array(best.model)
CN <- colnames(temp[,1,])
posteriors <-   melt(temp[,,grepl(paste(c("*_Intercept","*Liana"),collapse="|"),
                                  CN)]) %>%
  rename(sample = iteration,
         parname = variable) %>%
  mutate(par.type = case_when(grepl("^b_*",parname) ~ "Fixed effect",
                              TRUE ~ "Other")) %>%
  mutate(param = case_when(par.type == "Fixed effect" ~ sub("\\_.*","",str_replace(parname,"^b_","")),
                           TRUE ~ "Other"),
         fac = case_when(par.type == "Fixed effect" ~ sub(".*\\_","",str_replace(parname,"^b_","")),
                         TRUE ~ "Other"))

posteriors %>%
  dplyr::select(parname,param,fac,par.type) %>%
  distinct()

ggplot(data = posteriors %>%
         filter(fac == "Intercept")) +
  geom_density_ridges(aes(x = value, fill = fac, y = 1),
                      alpha = 0.3) +
  facet_wrap(~ param, scales = "free") +
  theme_bw()

ggplot(data = posteriors %>%
         group_by(param) %>%
         mutate(category = sub("^source","",fac)) %>%
         filter(par.type == "Fixed effect", fac != "Intercept")) +
  geom_density_ridges(aes(x = value, fill = category, y = 0),
                      alpha = 0.3) +
  facet_wrap(~ param, scales = "free") +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = c(0.2,0.9))

# Posterior predictions

dbhs <- seq(floor(min(data$dbh)),
            ceiling(max(data$dbh)),
            length.out = 1000)

newdata <- data.frame(dbh = c(dbhs,dbhs,dbhs),
                      Liana = factor(c(rep("no",length(dbhs)),
                                        rep("low",length(dbhs)),
                                        rep("high",length(dbhs))),
                                      levels = c("no","low","high")))


# See https://bg.copernicus.org/articles/16/847/2019/
ccoef <- as.numeric(exp(summary(best.model)[["spec_pars"]][1]**2/2))
ccoef.null <- as.numeric(exp(summary(null.model)[["spec_pars"]][1]**2/2))

pp <- melt(posterior_epred(best.model,
                             newdata = newdata,
                             re_formula = NA)) %>%
  rename(rep = Var1,
         id = Var2) %>%
  mutate(h = ccoef*exp(value)) %>%
  group_by(id) %>%
  summarise(h.m = mean(h,na.rm = TRUE))

pp.null <- melt(posterior_epred(null.model,
                                  newdata = newdata,
                                  re_formula = NA)) %>%
  rename(rep = Var1,
         id = Var2) %>%
  mutate(h = ccoef.null*exp(value)) %>%
  group_by(id) %>%
  summarise(h.m = mean(h,na.rm = TRUE))


newdata[["h.m"]] <- pp[["h.m"]]
newdata[["h.null.m"]] <- pp.null[["h.m"]]



ggplot(data) +
  geom_point(aes(x = dbh,y = h,
                 color = as.factor(Liana))) +
  geom_line(data = newdata,
            aes(x = dbh, y = h.m,
                color = as.factor(Liana))) +
  geom_line(data = newdata %>%
              filter(Liana == "no"),
            aes(x = dbh, y = h.null.m), color = "black") +
  theme_bw()



