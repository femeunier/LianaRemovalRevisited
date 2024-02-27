rm(list = ls())

library(ggplot2)
library(dplyr)
library(brms)
library(gridExtra)
library(abind)
library(reshape2)
library(ggridges)
library(stringr)
library(tidyr)
library(ggforce)
library(cowplot)
library(wesanderson)

# Load the data
all.df <- readRDS("./outputs/BCI.CA.data.RDS") %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high"))) %>%
  rename(dbh = DBH) %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10)

years <- unique(all.df$year)
years <- c(2015,2019)

models <- c("gmm","weibull","power")
model.forms <- c("none","all","a","b","ak","bk","k","ab")

models <- c("power")
model.forms <- c("a","none","all","b")

all.possible.files <- crossing(years, models) %>%
  mutate(n = paste(as.character(years),
                   as.character(models),sep = ".")) %>%
  pull(n)

# # # Transfer the outputs
# transfer.files("Fit.BCI.2015.weibull_all.RDS",
#                source = "outputs")

# Compile the outputs
fit.all.years <- list()

print('Reading')
for (iyear in seq(1,length(years))){

  print(paste("-",years[iyear]))
  fit.all.years[[iyear]] <- list()

  cpattern <- paste0("Fit.BCI.CA.",years[iyear],".")
  cfiles <- list.files("./outputs/",
                       pattern = paste0("^",cpattern,"*"),
                       full.names = FALSE)
  cnames <- tools::file_path_sans_ext(sub(cpattern,"",cfiles))

  tokeep <- cnames %in% c(expand.grid(m = models, mf = model.forms) %>%
                            mutate(all = paste0(m,"_",mf)) %>%
                            pull(all))

  cnames.filtered <- cnames[tokeep]
  cfiles.filtered <- cfiles[tokeep]

  for (ifile in seq(1,length(cfiles.filtered))){

    print(paste("--",ifile/length(cfiles.filtered)))

    fit.all.years[[iyear]][[cnames.filtered[ifile]]] <-  readRDS(paste0("./outputs/",cfiles.filtered[ifile]))
  }
}

# Select best model

comparison <- best.model <- pp.check.best <- pp.check.wb <- list()
best.model.names <- c()

print('Processing')
for (iyear in seq(1,length(years))){

  print(iyear/length(years))

  cyear = years[iyear]
  if (length(fit.all.years[[iyear]]) > 1){
    comparison[[iyear]] <- loo_compare(lapply(fit.all.years[[iyear]], LOO))
    best.model.names[iyear] <- rownames(comparison[[iyear]])[1]
  } else {
    comparison[[iyear]] <- NULL
    best.model.names[iyear] <- names(fit.all.years[[iyear]])
  }


  best.model[[iyear]] <- fit.all.years[[iyear]][[ best.model.names[iyear]]]
  pp.check.best[[iyear]] <- pp_check(best.model[[iyear]], ndraws = 500)


}

plot_collection <- grid.arrange(grobs = pp.check.best,
                                nrow = 1, ncol = length(pp.check.best))

ce <- liana.effect <-
  list()

for (iyear in seq(1,length(years))){

  cmodel <- fit.all.years[[iyear]][[best.model.names[iyear]]]
  ce[[iyear]] <- plot(conditional_effects(cmodel,"dbh:liana.cat",
                                          points = TRUE) ,plot = FALSE)[[1]] +
    theme_bw() +
    scale_y_continuous(limits = c(2.5,4)) +
    ggtitle(paste("Year of data collection:",years[iyear]))


  liana.effect[[iyear]] <- mcmc_plot(cmodel, variable = "*liana*", regex = TRUE) +
    # scale_x_continuous(limits = c(-0.02,0)) +
    ggtitle(paste("Year of data collection:",years[iyear]))
}



similar.models <- lapply(fit.all.years, function(x){
  names.x = names(x)
  return(x[[which(grepl("power_a$",names.x))]])})


null.models <- lapply(fit.all.years, function(x){
  names.x = names(x)
  return(x[[which(grepl("power_none$",names.x))]])})


posteriors <- bind_rows(lapply(similar.models,function(x){
  temp <- as.array(x)
  CN <- colnames(temp[,1,])
  return(
    melt(temp[,,grepl(paste(c("_Intercept","liana.catlow","liana.cathigh"),collapse="|"),
                      CN)]))}),.id = "year") %>%
  mutate(year = as.factor(years[as.numeric(year)])) %>%
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
  # filter(par.type == "Fixed effect")
  geom_density_ridges(aes(x = value, fill = fac, y = as.factor(year)),
                      alpha = 0.3) +
  facet_wrap(~ param, scales = "free") +
  theme_bw()


ggplot(data = posteriors %>%
         group_by(year,param) %>%
         mutate(rel.effect = value/mean(value[fac == "Intercept"]),
                category = sub("^liana.cat","",fac)) %>%
         filter(par.type == "Fixed effect", fac != "Intercept")) +
  geom_density_ridges(aes(x = value, fill = category, y = as.factor(year)),
                      alpha = 0.3) +
  # facet_wrap(~ param, scales = "free") +
  geom_vline(xintercept = 0) +
  labs(x = "Effect on a (Weibull)",
       y = "",
       fill = "Liana category") +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = c(0.2,0.9))


################################################################################
alpha = 0.25

temp <- bind_rows((lapply(1:length(years),
                          function(iyear){

                            print(iyear)
                            cdf <- all.df %>%
                              filter(year == years[iyear])

                            dbhs <- seq(floor(min(cdf$dbh)),
                                        ceiling(max(cdf$dbh)),
                                        length.out = 1000)

                            levels <- as.character(unique(cdf$liana.cat))

                            newdata <- data.frame()

                            for (ilevel in seq(1,length(levels))){

                              newdata <- bind_rows(list(newdata,
                                                        data.frame(
                                                          dbh = rep(dbhs,1),
                                                          liana.cat = c(rep(levels[ilevel],length(dbhs))))
                              ))
                            }

                            newdata <- newdata %>%
                              mutate(id = 1:length(dbh),
                                     year = years[iyear])


                            cmodel <- best.model[[iyear]]
                            null.model <- null.models[[iyear]]

                            ccoef <- as.numeric(exp(summary(cmodel)[["spec_pars"]][1]^2/2))
                            ccoef.null <- as.numeric(exp(summary(null.model)[["spec_pars"]][1]^2/2))
                            # ccoef <- 1

                            pp <- melt(posterior_predict(cmodel,
                                                         newdata = newdata,
                                                         re_formula = NA)) %>%
                              rename(rep = Var1,
                                     id = Var2) %>%
                              mutate(CA = ccoef*exp(value)) %>%
                              group_by(id) %>%
                              summarise(CA.m = mean(CA,na.rm = TRUE),
                                        CA.low = quantile(CA,alpha/2,na.rm = TRUE),
                                        CA.high = quantile(CA,1 - alpha/2,na.rm = TRUE))

                            pep <- melt(posterior_epred(cmodel,
                                                        newdata = newdata,
                                                        re_formula = NA)) %>%
                              rename(rep = Var1,
                                     id = Var2) %>%
                              mutate(CA.pred = ccoef*exp(value)) %>%
                              group_by(id) %>%
                              summarise(CA.pred.m = mean(CA.pred,na.rm = TRUE),
                                        CA.pred.low = quantile(CA.pred,alpha/2,na.rm = TRUE),
                                        CA.pred.high = quantile(CA.pred,1 - alpha/2,na.rm = TRUE))

                            pp.null <- melt(posterior_predict(null.model,
                                                              newdata = newdata,
                                                              re_formula = NA)) %>%
                              rename(rep = Var1,
                                     id = Var2) %>%
                              mutate(CA.null = ccoef.null*exp(value)) %>%
                              group_by(id) %>%
                              summarise(CA.null.m = mean(CA.null,na.rm = TRUE),
                                        CA.null.low = quantile(CA.null,alpha/2,na.rm = TRUE),
                                        CA.null.high = quantile(CA.null,1 - alpha/2,na.rm = TRUE))

                            pep.null <- melt(posterior_epred(null.model,
                                                             newdata = newdata,
                                                             re_formula = NA)) %>%
                              rename(rep = Var1,
                                     id = Var2) %>%
                              mutate(CA.null.pred = ccoef.null*exp(value)) %>%
                              group_by(id) %>%
                              summarise(CA.null.pred.m = mean(CA.null.pred,na.rm = TRUE),
                                        CA.null.pred.low = quantile(CA.null.pred,alpha/2,na.rm = TRUE),
                                        CA.null.pred.high = quantile(CA.null.pred,1 - alpha/2,na.rm = TRUE))

                            new.data.full <- newdata

                            new.data.full[["CA.m"]] <- pp[["CA.m"]]
                            new.data.full[["CA.low"]] <- pp[["CA.low"]]
                            new.data.full[["CA.high"]] <- pp[["CA.high"]]

                            new.data.full[["CA.pred.m"]] <- pep[["CA.pred.m"]]
                            new.data.full[["CA.pred.low"]] <- pep[["CA.pred.low"]]
                            new.data.full[["CA.pred.high"]] <- pep[["CA.pred.high"]]

                            new.data.full[["CA.null.m"]] <- pp.null[["CA.null.m"]]
                            new.data.full[["CA.null.low"]] <- pp.null[["CA.null.low"]]
                            new.data.full[["CA.null.high"]] <- pp.null[["CA.null.high"]]

                            new.data.full[["CA.null.pred.m"]] <- pep.null[["CA.null.pred.m"]]
                            new.data.full[["CA.null.pred.low"]] <- pep.null[["CA.null.pred.low"]]
                            new.data.full[["CA.null.pred.high"]] <- pep.null[["CA.null.pred.high"]]

                            return(new.data.full)
                          })))

all.df.title <- all.df %>%
  group_by(year) %>%
  mutate(year.N = paste0(year,", N = ", length(year)," (",length(year[which(liana.cat == "no")]), "-",
                         length(year[which(liana.cat == "low")]), "-",
                         length(year[which(liana.cat == "high")]), ")"),
         N = paste0("N = ", length(year)," (",length(year[which(liana.cat == "no")]), "-",
                    length(year[which(liana.cat == "low")]), "-",
                    length(year[which(liana.cat == "high")]), ")"))



temp.title <- temp %>%
  left_join(all.df.title %>% dplyr::select(year,year.N,N) %>% distinct(),
            by = "year")

# https://bg.copernicus.org/articles/16/847/2019/

dbhs <- seq(floor(min(all.df$dbh)),
            ceiling(max(all.df$dbh)),
            length.out = 1000)

df.Helene <-
  data.frame(dbh = dbhs) %>%
  mutate(CA = 0.66*(dbh**1.34))


ggplot(data = temp.title) +
  geom_point(data = all.df.title,
             aes(x = dbh,y = CA, color = as.factor(liana.cat)),
             size = 0.5, alpha = 0.25) +
  # geom_ribbon(aes(x = dbh, y = h.m, fill = liana.cat,
  #                 ymin = h.low, ymax = h.high), color = NA, alpha = 0.5) +
  geom_line(aes(x = dbh,y = CA.m, color = liana.cat)) +

  # geom_ribbon(aes(x = dbh, y = h.null.pred.m,
  #                 ymin = h.null.pred.low, ymax = h.null.pred.high), color = NA, alpha = 0.5, fill = "darkgrey") +
  geom_line(aes(x = dbh,y = CA.null.pred.m), color = "black") +
  geom_line(data = df.Helene,
            aes(x = dbh,y = CA), color = "black",linetype = 2) +
  facet_wrap(~ year.N) +
  scale_x_log10(limits = c(20,300),
                breaks = c(20,50,100,200)) +
  scale_y_log10() +
  labs(x = "DBH (cm)", y = 'Crown area (m²)', color = "Liana infestation") +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = c(0.08,0.88),
        legend.background=element_rect(fill = alpha("white", 0.5)),
        strip.background = element_blank())


predict.wide <- temp %>%
  dplyr::select(dbh,liana.cat,year,CA.m) %>%
  pivot_wider(names_from = liana.cat,
              values_from = CA.m) %>%
  mutate(dbh.cat = factor(case_when(dbh <= 30 ~ "Small",
                                    dbh <= 60 ~ "Intermediate",
                                    TRUE ~ "Large"),
                          levels = c("Small","Intermediate","Large"))) %>%
  mutate(diff = high - no,
         diff.rel = (high - no)/no)

gg.dist <- ggplot(data = all.df.title) +
  geom_sina(aes(x = as.factor(year), y = dbh, color = as.factor(year)), size = 0.1) +
  coord_flip() +
  theme_minimal() +
  guides(color = "none") +
  labs(x = "", y = "") +
  scale_y_continuous(breaks = c()) +
  theme(text = element_text(size = 20),panel.grid = element_blank())

gg.effect <- ggplot(data = predict.wide) +
  geom_line(aes(x = dbh, y = -diff.rel, color = as.factor(year))) +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  theme_bw() +
  labs(x = "DBH (cm)", y = "Relative difference in crown area (%)") +
  guides(color = "none") +
  theme(text = element_text(size = 20))

plot_grid(gg.dist,gg.effect,align = "hv",rel_heights = c(1,2.5),
          ncol = 1)



predict.wide <- temp %>%
  dplyr::select(dbh,liana.cat,year,CA.m) %>%
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
         filter(!is.na(value),
                type == "absolute")) +
  geom_line(aes(x = dbh, y = value, color = liana.cat)) +
  facet_wrap(type ~ (year),nrow = 1) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw()

ggplot(data = predict.wide %>%
         filter(!is.na(value),
                type == "relative")) +
  geom_line(aes(x = dbh, y = -value*100, color = liana.cat)) +
  facet_grid( ~ (year)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw() +
  labs(x = "DBH (cm)", y = "Relative effect on CA (%)") +
  theme(legend.position = c(0.9,0.9),
        text = element_text(size = 20))


