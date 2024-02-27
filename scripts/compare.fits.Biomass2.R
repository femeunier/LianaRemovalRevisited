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
library(ggdist)

# Load the data
all.raw.data <-  read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/2019_TLS_data.csv",stringsAsFactors = FALSE)
all.df <-  all.raw.data %>%
  rename(dbh = Final.DBH..cm.,
         CA = CPA..m2., h = Height..m.,
         Vol = Total.vol..L.,
         Biomass = Biomass..kg.) %>%
  mutate(liana.cat = case_when(Liana == 0 ~ "no",
                               Liana == 1 ~ "low",
                               Liana == 2 ~ "high")) %>%
  mutate(dbh = dbh*dbh*WSG*h)

models <- c("power")
model.forms <- c("all","none","a","b") # "a","k","b", "ab","ak","bk")

# Transfer the outputs
# transfer.files(paste0("Fit.Vol.*"),
#                source = "outputs")

# Compile the outputs

cfiles <- list.files("./outputs/",
                     full.names = FALSE)
print("Reading")

all.possible.files <- crossing(models,model.forms) %>%
  mutate(n = paste0("Fit.Biomass2",
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

comparison <- loo_compare(lapply(fit.all, loo))
best.model.names <- rownames(comparison)[1]
best.model <- fit.all[[ best.model.names]]
pp.check.best <- pp_check(best.model, ndraws = 100)
null.model <- fit.all[["Fit.Biomass2.power_none"]]

# plot(conditional_effects(best.model,"dbh:liana.cat",
#                          points = TRUE) ,plot = FALSE)[[1]] +
#   theme_bw()

# mcmc_plot(best.model, variable = "*liana*", regex = TRUE)

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

################################################################################
# Predictions
alpha = 0.11
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

WD = mean(all.df$WSG)

pp <- melt(posterior_epred(best.model,
                           newdata = newdata,
                           re_formula = NA)) %>%
  rename(rep = Var1,
         id = Var2) %>%
  mutate(Vol = ccoef*exp(value)) %>%
  group_by(id) %>%
  summarise(Vol.m = mean(Vol,na.rm = TRUE),
            Vol.low = quantile(Vol,alpha/2,na.rm = TRUE),
            Vol.high = quantile(Vol,1 - alpha/2,na.rm = TRUE)) %>%
  mutate(AGB.m =  Vol.m)

pp.null <- melt(posterior_epred(null.model,
                                newdata = newdata,
                                re_formula = NA)) %>%
  rename(rep = Var1,
         id = Var2) %>%
  mutate(Vol = ccoef.null*exp(value)) %>%
  group_by(id) %>%
  summarise(Vol.m = mean(Vol,na.rm = TRUE),
            Vol.low = quantile(Vol,alpha/2,na.rm = TRUE),
            Vol.high = quantile(Vol,1 - alpha/2,na.rm = TRUE)) %>%
  mutate(AGB.m =  Vol.m)


newdata[["AGB.m"]] <- pp[["AGB.m"]]
newdata[["AGB.null.m"]] <- pp.null[["AGB.m"]]

E <- 0.04635461 # BCI from Chave
df.Chave <- data.frame(dbh = dbhs) %>%
  mutate(AGB = exp(-1.803 - 0.976*E + 0.976*log(WD) + 2.673*log(dbh) - 0.0299*(log(dbh))**2))

# ggplot() +
#   geom_line(data = newdata,
#             aes(x = dbh, y = AGB.m, color = liana.cat)) +
#   geom_line(data = newdata,
#             aes(x = dbh, y = AGB.null.m), color = "black", linetype = 2) +
#   geom_line(data = df.Chave,
#             aes(x = dbh, y = AGB), color = "black") +
#   geom_point(data = all.df,
#              aes(x = dbh, y = Vol*WSG, col = liana.cat), shape = 1 ) +
#   scale_x_log10() +
#   scale_y_log10() +
#   theme_bw()

diagnosic <- readRDS("./outputs/BCI/Diagnostics.RDS") %>%
  filter(fe == "none") %>%
  filter(waic == min(waic))
model <- readRDS(paste0("./outputs/BCI/Fit.BCI.",diagnosic$model.name,".RDS"))

ccoef <- as.numeric(exp(summary(model)[["spec_pars"]][1]**2/2))
h.predicted <- melt(posterior_epred(model,
                                    newdata = data.frame(dbh = 30:160),
                                    re_formula = NA)) %>%
  rename(rep = Var1,
         id = Var2) %>%
  mutate(h = ccoef*exp(value)) %>%
  group_by(id) %>%
  summarise(h.m = mean(h,na.rm = TRUE),
            .groups = "keep")

WD = mean(all.df$WSG,na.rm = TRUE)
df.Chave <- data.frame(dbh = 30:160) %>%
  mutate(h = h.predicted$h.m) %>%
  mutate(h.chave =  exp(0.893 - E + 0.76*log(dbh) - 0.034*(log(dbh)**2))) %>%
  mutate(AGB = 0.0673*(WD*dbh**2*h)**0.976)

plot(df.Chave$dbh,df.Chave$h,type = "l",ylim = c(0,50))
lines(df.Chave$dbh,df.Chave$h.chave,col = "red")

ggplot() +

  geom_point(data = all.df,
             aes(x = dbh/100/100, y = Vol*WSG/1e3, col = liana.cat), alpha = 0.2, size = 2) +

  geom_line(data = newdata,
            aes(x = dbh/100/100, y = AGB.m/1e3), color = "black") +

  geom_line(data = df.Chave,
            aes(x = WD*dbh*dbh*h/100/100, y = AGB/1e3), color = "black", linetype = 2) +

  # geom_abline(slope = 1,intercept = 0,color = "red") +

  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  labs(x = "", y = "") +
  guides(color = "none")


DBH2test <- 50
newdata2 <- bind_rows(list(data.frame(
  dbh = rep(DBH2test,length(levels)),
  liana.cat = levels)))

newdata2 <- newdata2 %>%
  mutate(id = 1:length(dbh))

cmodel <- best.model
null.model <- null.model

ccoef <- as.numeric(exp( (summary(cmodel)[["spec_pars"]][1]**2)/2))
ccoef.null <- as.numeric(exp(summary(null.model)[["spec_pars"]][1]**2/2))

alpha = 0.11
temp2 <- melt(posterior_epred(cmodel,
                              newdata = newdata2,
                              re_formula = NA)) %>%
  rename(rep = Var1,
         id = Var2) %>%
  left_join(newdata2 %>%
              dplyr::select(c(id,dbh,liana.cat)),
            by = "id") %>%
  mutate(Vol = ccoef*exp(value)) %>%
  filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
  ungroup() %>%
  dplyr::select(-c(value,id)) %>%
  pivot_wider(names_from = liana.cat,
              values_from = Vol) %>%
  pivot_longer(cols = c("low","high"),
               names_to = "liana.cat") %>%
  group_by(liana.cat) %>%
  mutate(signif_rel = case_when(quantile((value-no)/no*100,probs = 1-alpha/2,na.rm = TRUE) < 0 ~ 0.3,
                                quantile((value-no)/no*100,probs = alpha/2,na.rm = TRUE) > 0 ~ 0.3,
                                TRUE ~ 0.2),
         signif_rel2 = case_when(quantile((value-no)/no*100,probs = 1-alpha/2,na.rm = TRUE) < 0 ~ 1,
                                 quantile((value-no)/no*100,probs = alpha/2,na.rm = TRUE) > 0 ~ 1,
                                 TRUE ~ 0.4))


ggplot(data = temp2 %>%
         filter(!is.na(value)),
       aes(x = 100*(value - no)/no, y = 0,fill = liana.cat, color = liana.cat,
           alpha = signif_rel)) +
  stat_halfeye(color = NA) +
  stat_pointinterval(aes(y = -0.05,
                         alpha = signif_rel2),
                     .width = c(.89),
                     position = position_dodge(width = 0.02)) +
  geom_vline(xintercept = 0, linetype = 1) +
  # theme_bw() +
  theme_minimal() +
  labs(y = "", x = "AGB change (%)") +
  theme(legend.position = c(0.9,0.6),
        text = element_text(size = 30)) +
  labs(x= "") +
  scale_y_continuous(limits = c(-0.1,1),breaks = c()) +
  scale_x_continuous(limits = c(-40,15)) +
  scale_fill_manual(values = c("no" = "darkgreen",
                               "low" = "orange",
                               "high"= "darkred")) +
  scale_color_manual(values = c("no" = "darkgreen",
                                "low" = "orange",
                                "high"= "darkred")) +
  guides(fill = "none", color = "none",alpha = "none")


temp2 %>%
  filter(!is.na(value)) %>%
  group_by(liana.cat) %>%
  summarise(m = 100*median((value-no)/no,na.rm = TRUE),
            low = 100*quantile((value-no)/no,alpha/2,na.rm = TRUE),
            high = 100*quantile((value-no)/no,1-alpha/2,na.rm = TRUE),

            m.abs = median((value-no)/1e3,na.rm = TRUE),
            low.abs = quantile((value-no)/1e3,alpha/2,na.rm = TRUE),
            high.abs = quantile((value-no)/1e3,1-alpha/2,na.rm = TRUE))

saveRDS(temp2,
        paste0("./outputs/BCI.AGB2.TLS.",DBH2test,".RDS"))



ccoef <- as.numeric(exp(summary(fit.all[["Fit.Biomass2.power_a"]])[["spec_pars"]][1]**2/2))
ccoef.null <- as.numeric(exp(summary(null.model)[["spec_pars"]][1]**2/2))

null.predicted <- ccoef.null*exp(predict(fit.all[["Fit.Biomass2.power_none"]])[,1])
best.predicted <- ccoef*exp(predict(fit.all[["Fit.Biomass2.power_a"]])[,1])
sqrt(1/(length(null.predicted) - 2)*sum((null.predicted-all.df$Biomass)**2,na.rm = TRUE))/1e3
sqrt(1/(length(best.predicted) - 4)*sum((best.predicted-all.df$Biomass)**2,na.rm = TRUE))/1e3

