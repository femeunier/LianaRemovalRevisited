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


ch.correction <- readRDS("./data/ch.data.correction.RDS")
# Load the data
all.df <-   read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/2019_TLS_data.csv",stringsAsFactors = FALSE) %>%
  rename(dbh = Final.DBH..cm.) %>%
  left_join(ch.correction %>%
              dplyr::select(tag,charea) %>%
              rename(Tag = tag),
            by = "Tag") %>%
  rename(CA = charea) %>%
  mutate(liana.cat = case_when(Liana == 0 ~ "no",
                               Liana == 1 ~ "low",
                               Liana == 2 ~ "high"))
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

pp <- melt(posterior_epred(best.model,
                             newdata = newdata,
                             re_formula = NA)) %>%
  rename(rep = Var1,
         id = Var2) %>%
  mutate(CA = ccoef*exp(value)) %>%
  group_by(id) %>%
  filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
  summarise(CA.m = median(CA,na.rm = TRUE),
            CA.low = quantile(CA,alpha/2,na.rm = TRUE),
            CA.high = quantile(CA,1 - alpha/2,na.rm = TRUE))

pp.null <- melt(posterior_epred(null.model,
                                  newdata = newdata,
                                  re_formula = NA)) %>%
  rename(rep = Var1,
         id = Var2) %>%
  mutate(CA = ccoef.null*exp(value)) %>%
  group_by(id) %>%
  filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
  summarise(CA.m = mean(CA,na.rm = TRUE),
            CA.low = quantile(CA,alpha/2,na.rm = TRUE),
            CA.high = quantile(CA,1 - alpha/2,na.rm = TRUE))


newdata[["CA.m"]] <- pp[["CA.m"]]
newdata[["CA.null.m"]] <- pp.null[["CA.m"]]



DBH2test <- 150
alpha = 0.11
newdata2 <- bind_rows(list(data.frame(
  dbh = rep(DBH2test,length(levels)),
  liana.cat = levels)))

newdata2 <- newdata2 %>%
  mutate(id = 1:length(dbh))

cmodel <- best.model
null.model <- null.model

ccoef <- as.numeric(exp( (summary(cmodel)[["spec_pars"]][1]**2)/2))
ccoef.null <- as.numeric(exp(summary(null.model)[["spec_pars"]][1]**2/2))


temp2 <- melt(posterior_epred(cmodel,
                              newdata = newdata2,
                              re_formula = NA)) %>%
  rename(rep = Var1,
         id = Var2) %>%
  left_join(newdata2 %>%
              dplyr::select(c(id,dbh,liana.cat)),
            by = "id") %>%
  mutate(CA = ccoef*exp(value)) %>%
  filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
  ungroup() %>%
  dplyr::select(-c(value,id)) %>%
  pivot_wider(names_from = liana.cat,
              values_from = CA) %>%
  pivot_longer(cols = c("low","high"),
               names_to = "liana.cat") %>%
  group_by(liana.cat) %>%
  mutate(signif_rel = case_when(quantile((value-no)/no*100,probs = 1-alpha/2,na.rm = TRUE) < 0 ~ 0.3,
                                quantile((value-no)/no*100,probs = alpha/2,na.rm = TRUE) > 0 ~ 0.3,
                                TRUE ~ 0.2),
         signif_rel2 = case_when(quantile((value-no)/no*100,probs = 1-alpha/2,na.rm = TRUE) < 0 ~ 1,
                                 quantile((value-no)/no*100,probs = alpha/2,na.rm = TRUE) > 0 ~ 1,
                                 TRUE ~ 0.4))

ThreeD.CA <- readRDS("./outputs/plot3d.CA.RDS")


ggplot() +
  geom_line(data = newdata,
            aes(x = dbh, y = CA.m, color = liana.cat)) +
  geom_point(data = all.df %>%
               filter(Tag %in% c(ThreeD.CA[["Tag"]])),
             aes(x = dbh, y = CA, col = liana.cat), alpha = 0.8, size = 2) +
  geom_point(data = all.df,
             aes(x = dbh, y = CA, col = liana.cat), alpha = 0.2, size = 2) +

  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  scale_x_log10() +
  scale_y_log10(limits = c(1,2000)) +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  labs(x = "", y = "") +
  guides(color = "none")


ggplot(data = temp2 %>%
         filter(!is.na(value)),
       aes(x = 100*(value-no)/no, fill = liana.cat, color = liana.cat, alpha = signif_rel)) +
  stat_halfeye(alpha = 0.2, color = NA) +
  stat_pointinterval(aes(y = -0.0,
                         alpha = signif_rel2),
                     .width = c(.89),
                     position = position_dodge(width = 0.02)) +
  geom_vline(xintercept = 0, linetype = 1) +
  scale_y_continuous(limits = c(-0.1,1), breaks = c()) +
  scale_x_continuous(limits = c(-50,25)) +
  theme_minimal() +
  labs(y = "", x = "") +
  theme(legend.position = c(0.9,0.6),
        text = element_text(size = 24)) +
  scale_fill_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred")) +
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  guides(fill = "none", color = "none", alpha = "none")

temp2 %>%
  filter(!is.na(value)) %>%
  group_by(liana.cat) %>%
  summarise(m = 100*median((value-no)/no,na.rm = TRUE),
            low = 100*quantile((value-no)/no,alpha/2,na.rm = TRUE),
            high = 100*quantile((value-no)/no,1-alpha/2,na.rm = TRUE),

            m.abs = median((value-no),na.rm = TRUE),
            low.abs = quantile((value-no),alpha/2,na.rm = TRUE),
            high.abs = quantile((value-no),1-alpha/2,na.rm = TRUE))

saveRDS(temp2,
        paste0("./outputs/BCI.CA.TLS.",DBH2test,".RDS"))


