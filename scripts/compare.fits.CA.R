rm(list = ls())

library(brms)
library(gridExtra)
library(abind)
library(reshape2)
library(ggridges)
library(stringr)
library(scales)
library(tidyr)
library(ggforce)
library(dplyr)
library(LianaRemovalRevisited)
library(cowplot)
library(ggplot2)

# Load the data
all.df <- readRDS("./data/BCI.Vol.data.RDS")

models <- c("power")
model.forms <- c("all","none","a","b") # "a","k","b", "ab","ak","bk")

# Transfer the outputs
# transfer.files(paste0("Fit.CA.*"),
#                source = "outputs")

# Compile the outputs

cfiles <- list.files("./outputs/",
                     full.names = FALSE)
print("Reading")

all.possible.files <- crossing(models,model.forms) %>%
  mutate(n = paste0("Fit.CA",
                    ".",
                    as.character(models),
                    "_",
                    as.character(model.forms),
                    ".RDS")) %>%
  pull(n)

tokeep <- cfiles %in% all.possible.files
cfiles.filtered <- cfiles[tokeep]
cnames.filtered <- tools::file_path_sans_ext(cfiles.filtered)

fit.all <- list()

for (ifile in seq(1,length(cfiles.filtered))){

  print(paste("-",ifile/length(cfiles.filtered)))
  fit.all[[cnames.filtered[ifile]]] <-  readRDS(paste0("./outputs/",cfiles.filtered[ifile]))
}


# Select best model
print("Processing")

comparison <- loo_compare(lapply(fit.all, LOO))
best.model.names <- rownames(comparison)[1]
best.model <- fit.all[[ best.model.names]]
pp.check.best <- pp_check(best.model, ndraws = 500)
null.model <- fit.all[["Fit.CA.power_none"]]

plot(conditional_effects(best.model,"dbh:liana.cat",
                         points = TRUE) ,plot = FALSE)[[1]] +
  theme_bw()

mcmc_plot(best.model, variable = "*liana*", regex = TRUE)


temp <- as.array(best.model)
CN <- colnames(temp[,1,])
posteriors <-   melt(temp[,,grepl(paste(c("_Intercept","liana.catlow","liana.cathigh"),collapse="|"),
                                  CN)]) %>%
  rename(sample = iteration,
         parname = variable) %>%
  mutate(par.type = case_when(grepl("^b_*",parname) ~ "Fixed effect",
                              TRUE ~ "Other")) %>%
  mutate(param = case_when(par.type == "Fixed effect" ~ sub("\\_.*","",str_replace(parname,"^b_","")),
                           TRUE ~ "Other"),
         fac = case_when(par.type == "Fixed effect" ~ sub(".*\\_","",str_replace(parname,"^b_","")),
                         TRUE ~ "Other"))

ggplot(data = posteriors %>%
         filter(fac == "Intercept")) +
  geom_density_ridges(aes(x = value, fill = fac, y = 1),
                      alpha = 0.3) +
  facet_wrap(~ param, scales = "free") +
  theme_bw()

ggplot(data = posteriors %>%
         group_by(param) %>%
         mutate(rel.effect = value/mean(value[fac == "Intercept"]),
                category = sub("^liana.cat","",fac)) %>%
         filter(par.type == "Fixed effect", fac != "Intercept")) +
  geom_density_ridges(aes(x = rel.effect, fill = category, y = 0),
                      alpha = 0.3) +
  facet_wrap(~ param, scales = "free") +
  geom_vline(xintercept = 0) +
  labs(x = "Relative effect on a (Power)",
       y = "",
       fill = "Liana category") +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = c(0.2,0.9))


################################################################################
# Predictions
alpha = 0.05
dbhs <- seq(floor(min(all.df$dbh)),
            ceiling(max(all.df$dbh)),
            length.out = 1000)

levels <- as.character(unique(all.df$liana.cat))
newdata <- data.frame()

for (ilevel in seq(1,length(levels))){

  ccdf <- all.df %>% filter(liana.cat == levels[ilevel])

  cdbhs <- dbhs[dbhs>= min(ccdf$dbh,na.rm = TRUE) &
                  dbhs <= max(ccdf$dbh,na.rm = TRUE)]

  newdata <- bind_rows(list(newdata,
                            data.frame(
                              dbh = rep(cdbhs,1),
                              liana.cat = c(rep(levels[ilevel],length(cdbhs))))
  ))
}


ccoef <- as.numeric(exp(summary(best.model)[["spec_pars"]][1]**2/2))
ccoef.null <- as.numeric(exp(summary(null.model)[["spec_pars"]][1]**2/2))

pp <- melt(posterior_predict(best.model,
                             newdata = newdata,
                             re_formula = NA)) %>%
  rename(rep = Var1,
         id = Var2) %>%
  mutate(CA = ccoef*exp(value)) %>%
  group_by(id) %>%
  summarise(CA.m = mean(CA,na.rm = TRUE),
            CA.low = quantile(CA,alpha/2,na.rm = TRUE),
            CA.high = quantile(CA,1 - alpha/2,na.rm = TRUE))

pp.null <- melt(posterior_predict(null.model,
                                  newdata = newdata,
                                  re_formula = NA)) %>%
  rename(rep = Var1,
         id = Var2) %>%
  mutate(CA = ccoef*exp(value)) %>%
  group_by(id) %>%
  summarise(CA.m = mean(CA,na.rm = TRUE),
            CA.low = quantile(CA,alpha/2,na.rm = TRUE),
            CA.high = quantile(CA,1 - alpha/2,na.rm = TRUE))


newdata[["CA.m"]] <- pp[["CA.m"]]
newdata[["CA.null.m"]] <- pp.null[["CA.m"]]

predict.wide <- newdata %>%
  dplyr::select(dbh,liana.cat,CA.m) %>%
  pivot_wider(names_from = liana.cat,
              values_from = CA.m) %>%
  mutate(dbh.cat = factor(case_when(dbh <= 30 ~ "Small",
                                    dbh <= 60 ~ "Intermediate",
                                    TRUE ~ "Large"),
                          levels = c("Small","Intermediate","Large"))) %>%
  mutate(diff.high = high - no,
         diff.high.rel = (high - no)/no,
         diff.low = low - no,
         diff.low.rel = (low - no)/no) %>%
  pivot_longer(cols = c(diff.high,diff.high.rel,diff.low,diff.low.rel)) %>%
  mutate(type = case_when(grepl('rel',name) ~ "relative",
                          TRUE ~ "absolute"),
         liana.cat = case_when(grepl("high",name) ~ "high",
                               grepl("low",name) ~ "low",
                               TRUE ~ "other"))

ggplot(data = predict.wide %>%
         filter(!is.na(value))) +
  geom_line(aes(x = dbh, y = value, color = liana.cat)) +
  facet_wrap(~ type, scales = "free") +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw()

