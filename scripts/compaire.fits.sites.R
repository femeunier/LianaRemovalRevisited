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
all.df <- readRDS("./outputs/All.COI.data.RDS") %>%
  mutate(sp = str_squish(sp)) %>%
  filter(dbh >= 10)

sites <- unique(all.df$site)

models <- c("weibull")
model.forms <- c("all","none","a")

all.possible.files <- crossing(sites, models) %>%
  mutate(n = paste(as.character(sites),
                   as.character(models),sep = ".")) %>%
  pull(n)

# Transfer the outputs
# transfer.files(paste0("Fit.Pasoh*"),
#                source = "outputs")

# Compile the outputs
fit.all.sites <- list()


cfiles <- list.files("./outputs/",
                     full.names = FALSE)
print("Reading")
for (isite in seq(1,length(sites))){

  print(paste("-",sites[isite]))
  fit.all.sites[[isite]] <- list()

  all.possible.files <- crossing(sites[isite], models,model.forms) %>%
    mutate(n = paste0("Fit.",
                      as.character(sites[isite]),
                      ".",
                      as.character(models),
                      "_",
                     as.character(model.forms),
                     ".RDS")) %>%
    pull(n)

  tokeep <- cfiles %in% all.possible.files


  cfiles.filtered <- cfiles[tokeep]
  cnames.filtered <- tools::file_path_sans_ext(cfiles.filtered)

  for (ifile in seq(1,length(cfiles.filtered))){

    print(paste("--",ifile/length(cfiles.filtered)))
    fit.all.sites[[isite]][[cnames.filtered[ifile]]] <-  readRDS(paste0("./outputs/",cfiles.filtered[ifile]))
  }
}

# Select best model

comparison <- best.model <- pp.check.best <- pp.check.wb <- list()
best.model.names <- c()

actual.df <- data.frame()

print("Processing")

for (isite in seq(1,length(sites))){

  print(isite/length(sites))

  csite <- sites[isite]
  actual.df <- bind_rows(list(actual.df,
                              all.df %>% filter(site == csite)))


  if (length(fit.all.sites[[isite]]) > 1){
    comparison[[isite]] <- loo_compare(lapply(fit.all.sites[[isite]], LOO))
    best.model.names[isite] <- rownames(comparison[[isite]])[1]
  } else {
    comparison[[isite]] <- NULL
    best.model.names[isite] <- names(fit.all.sites[[isite]])
  }

  # best.model.names[isite] <-  names(fit.all.sites[[isite]])[
  #   grepl("weibull_k",names(fit.all.sites[[isite]]))]


  best.model[[isite]] <- fit.all.sites[[isite]][[ best.model.names[isite]]]
  pp.check.best[[isite]] <- pp_check(best.model[[isite]], ndraws = 500)

}

plot_collection <- grid.arrange(grobs = pp.check.best, nrow = 1, ncol = length(pp.check.best))

ce <- liana.effect <-
  list()

for (isite in seq(1,length(sites))){

  cmodel <- fit.all.sites[[isite]][[best.model.names[isite]]]
  ce[[isite]] <- plot(conditional_effects(cmodel,"dbh:liana.cat",
                                          points = TRUE) ,plot = FALSE)[[1]] +
    theme_bw() +
    scale_y_continuous(limits = c(2.5,4)) +
    ggtitle(paste("site of data collection:",sites[isite]))


  liana.effect[[isite]] <- mcmc_plot(cmodel, variable = "*liana*", regex = TRUE) +
    scale_x_continuous(limits = c(-0.35,0)) +
    ggtitle(paste("Site:",sites[isite]))
}


plot_collection <- grid.arrange(grobs = ce,
                                ncol = length(sites), nrow = 1)

plot_collection <- grid.arrange(grobs = liana.effect,
                                nrow = length(sites), ncol = 1)

similar.models <- lapply(fit.all.sites, function(x){
  names.x = names(x)
  return(x[[which(grepl("weibull_a$",names.x))]])})

null.models <- lapply(fit.all.sites, function(x){
  names.x = names(x)
  return(x[[which(grepl("weibull_none",names.x))]])})

posteriors <- bind_rows(lapply(similar.models,function(x){
  temp <- as.array(x)
  CN <- colnames(temp[,1,])
  return(
    melt(temp[,,grepl(paste(c("_Intercept","liana.catlow","liana.cathigh"),collapse="|"),
                      CN)]))
}),.id = "site") %>%
  mutate(site = as.factor(sites[as.numeric(site)])) %>%
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
  geom_density_ridges(aes(x = value, fill = fac, y = as.factor(site)),
                      alpha = 0.3) +
  facet_wrap(~ param, scales = "free") +
  theme_bw()

ggplot(data = posteriors %>%
         group_by(site,param) %>%
         mutate(rel.effect = value/mean(value[fac == "Intercept"]),
                category = sub("^liana.cat","",fac)) %>%
         filter(par.type == "Fixed effect", fac != "Intercept")) +
  geom_density_ridges(aes(x = rel.effect, fill = category, y = as.factor(site)),
                      alpha = 0.3) +
  # facet_wrap(~ param, scales = "free") +
  geom_vline(xintercept = 0) +
  labs(x = "Relative effect on a (Weibull)",
       y = "",
       fill = "Liana category") +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = c(0.2,0.9))

################################################################################
alpha = 0.25

temp <- bind_rows((lapply(1:length(sites),
                 function(isite){

                   print(isite)
                   cdf <- all.df %>%
                     filter(site == sites[isite])

                   dbhs <- seq(floor(min(cdf$dbh)),
                               ceiling(max(cdf$dbh)),
                               length.out = 100)

                   levels <- as.character(unique(cdf$liana.cat))

                   newdata <- data.frame()

                   for (ilevel in seq(1,length(levels))){

                     ccdf <- cdf %>% filter(liana.cat == levels[ilevel])

                     cdbhs <- dbhs[dbhs>= min(ccdf$dbh,na.rm = TRUE) &
                                     dbhs <= max(ccdf$dbh,na.rm = TRUE)]

                      newdata <- bind_rows(list(newdata,
                                               data.frame(
                                                 dbh = rep(cdbhs,1),
                                                 liana.cat = c(rep(levels[ilevel],length(cdbhs))))
                   ))
                   }

                   newdata <- newdata %>%
                     mutate(id = 1:length(dbh),
                            site = sites[isite])


                   cmodel <- best.model[[isite]]
                   null.model <- null.models[[isite]]

                   ccoef <- as.numeric(exp(summary(cmodel)[["spec_pars"]][1]**2/2))
                   ccoef.null <- as.numeric(exp(summary(null.model)[["spec_pars"]][1]**2/2))
                   # ccoef <- 1

                   pp <- melt(posterior_predict(cmodel,
                                           newdata = newdata,
                                           re_formula = NA)) %>%
                     rename(rep = Var1,
                            id = Var2) %>%
                     mutate(h = ccoef*exp(value)) %>%
                     group_by(id) %>%
                     summarise(h.m = mean(h,na.rm = TRUE),
                               h.low = quantile(h,alpha/2,na.rm = TRUE),
                               h.high = quantile(h,1 - alpha/2,na.rm = TRUE))

                   pep <- melt(posterior_epred(cmodel,
                                          newdata = newdata,
                                          re_formula = NA)) %>%
                     rename(rep = Var1,
                            id = Var2) %>%
                     mutate(h.pred = ccoef*exp(value)) %>%
                     group_by(id) %>%
                     summarise(h.pred.m = mean(h.pred,na.rm = TRUE),
                               h.pred.low = quantile(h.pred,alpha/2,na.rm = TRUE),
                               h.pred.high = quantile(h.pred,1 - alpha/2,na.rm = TRUE))

                   pp.null <- melt(posterior_predict(null.model,
                                                newdata = newdata,
                                                re_formula = NA)) %>%
                   rename(rep = Var1,
                          id = Var2) %>%
                     mutate(h.null = ccoef*exp(value)) %>%
                     group_by(id) %>%
                     summarise(h.null.m = mean(h.null,na.rm = TRUE),
                               h.null.low = quantile(h.null,alpha/2,na.rm = TRUE),
                               h.null.high = quantile(h.null,1 - alpha/2,na.rm = TRUE))

                   pep.null <- melt(posterior_epred(null.model,
                                               newdata = newdata,
                                               re_formula = NA)) %>%
                     rename(rep = Var1,
                            id = Var2) %>%
                     mutate(h.null.pred = ccoef*exp(value)) %>%
                     group_by(id) %>%
                     summarise(h.null.pred.m = mean(h.null.pred,na.rm = TRUE),
                               h.null.pred.low = quantile(h.null.pred,alpha/2,na.rm = TRUE),
                               h.null.pred.high = quantile(h.null.pred,1 - alpha/2,na.rm = TRUE))

                   new.data.full <- newdata

                   new.data.full[["h.m"]] <- pp[["h.m"]]
                   new.data.full[["h.low"]] <- pp[["h.low"]]
                   new.data.full[["h.high"]] <- pp[["h.high"]]

                   new.data.full[["h.pred.m"]] <- pep[["h.pred.m"]]
                   new.data.full[["h.pred.low"]] <- pep[["h.pred.low"]]
                   new.data.full[["h.pred.high"]] <- pep[["h.pred.high"]]

                   new.data.full[["h.null.m"]] <- pp.null[["h.null.m"]]
                   new.data.full[["h.null.low"]] <- pp.null[["h.null.low"]]
                   new.data.full[["h.null.high"]] <- pp.null[["h.null.high"]]

                   new.data.full[["h.null.pred.m"]] <- pep.null[["h.null.pred.m"]]
                   new.data.full[["h.null.pred.low"]] <- pep.null[["h.null.pred.low"]]
                   new.data.full[["h.null.pred.high"]] <- pep.null[["h.null.pred.high"]]

                   return(new.data.full)
                 })))

all.df.title <- all.df %>%
  group_by(site) %>%
  mutate(site.N = paste0(site,", N = ", length(site)," (",length(site[which(liana.cat == "no")]), "-",
                         length(site[which(liana.cat == "low")]), "-",
                         length(site[which(liana.cat == "high")]), ")"))



temp.title <- temp %>%
  left_join(all.df.title %>% dplyr::select(site,site.N) %>% distinct(),
            by = "site") %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high")))


ggplot(data = temp.title) +
  geom_point(data = all.df.title,
             aes(x = dbh,y = h, color = as.factor(liana.cat)),
             size = 0.5, alpha = 0.25) +
  # geom_ribbon(aes(x = dbh, y = h.pred.m, fill = as.factor(liana.cat),
  #                 ymin = h.pred.low, ymax = h.pred.high), color = NA, alpha = 0.5) +
  geom_line(aes(x = dbh,y = h.m, color = as.factor(liana.cat))) +

  # geom_ribbon(aes(x = dbh, y = h.null.pred.m,
  #                 ymin = h.null.pred.low, ymax = h.null.pred.high), color = NA, alpha = 0.5, fill = "darkgrey") +
  geom_line(aes(x = dbh,y = h.null.pred.m), color = "black") +
  facet_wrap(~ site.N, scales = "free") +
  scale_x_log10(limits = c(10,300),
                breaks = c(10,20,50,100,200)) +
  scale_y_continuous(limits = c(5,60)) +
  labs(x = "DBH (cm)", y = 'Height (m)', color = "Liana infestation", fill = "Liana infestation") +
  theme_bw() +
  theme(text = element_text(size = 20),
        legend.position = c(0.42,0.56))


predict.wide <- temp %>%
  dplyr::select(dbh,liana.cat,site,h.m) %>%
  pivot_wider(names_from = liana.cat,
              values_from = h.m) %>%
  mutate(dbh.cat = factor(case_when(dbh <= 30 ~ "Small",
                             dbh <= 60 ~ "Intermediate",
                             TRUE ~ "Large"),
                          levels = c("Small","Intermediate","Large"))) %>%
  mutate(diff = high - no,
         diff.rel = (high - no)/no)

gg.dist <- ggplot(data = all.df.title %>% filter(site %in% sites)) +
  geom_sina(aes(x = site, y = dbh, color = site), size = 0.1) +
  coord_flip() +
  theme_minimal() +
  guides(color = "none") +
  labs(x = "", y = "") +
  scale_y_continuous(breaks = c()) +
  theme(text = element_text(size = 20),panel.grid = element_blank())

gg.effect <- ggplot(data = predict.wide) +
  geom_line(aes(x = dbh, y = -diff.rel*100, color = site)) +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  theme_bw() +
  labs(x = "DBH (cm)", y = "Relative difference in height (%)") +
  guides(color = "none") +
  theme(text = element_text(size = 20))

plot_grid(gg.dist,gg.effect,align = "hv",rel_heights = c(1,2.5),
          ncol = 1)

predict.wide <- temp %>%
  dplyr::select(dbh,liana.cat,h.m,site) %>%
  pivot_wider(names_from = liana.cat,
              values_from = h.m) %>%
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
  geom_line(aes(x = dbh, y = -value, color = site)) +
  facet_wrap(type ~ liana.cat, scales = "free") +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw()


################################################################################

Levels <- c("no","low","high")
temp2 <- bind_rows((lapply(1:length(sites),
                          function(isite){

                            print(isite)
                            cdf <- all.df %>%
                              filter(site == sites[isite])


                            newdata <- cdf %>%
                              mutate(id = 1:length(dbh),
                                     liana.cat = relevel(as.factor(as.character(liana.cat)),"no"),
                                     site = sites[isite])

                         # best_model
                          cmodel <- best.model[[isite]]
                          ccoef <- as.numeric(exp(summary(cmodel)[["spec_pars"]][1]**2/2))
                          # ccoef <- 1
                          pp <- posterior_predict(cmodel,
                                                  newdata = newdata,
                                                  re_formula = NA)


                          # Null_model
                          null.model <- null.models[[isite]]
                          ccoef.null <- as.numeric(exp(summary(null.model)[["spec_pars"]][1]**2/2))
                          # ccoef <- 1
                          pp.null <- posterior_predict(null.model,
                                                       newdata = newdata,
                                                       re_formula = NA)

                          new.data.full <- newdata %>%
                            left_join(melt(pp) %>%
                                        rename(rep = Var1,
                                               id = Var2),
                                      by = "id") %>%
                            mutate(h = ccoef*exp(value)) %>%
                            group_by(site,id,liana.cat,dbh) %>%
                            summarise(h.m = mean(h,na.rm = TRUE),
                                      .groups = "keep") %>%


                            left_join(melt(pp.null) %>%
                                        rename(rep = Var1,
                                               id = Var2),
                                      by = "id") %>%
                            mutate(h.null = ccoef.null*exp(value)) %>%
                            group_by(site,id,liana.cat,dbh) %>%
                            summarise(h.m = unique(h.m),
                                      h.null.m = mean(h.null,na.rm = TRUE),
                                      .groups = "keep")


                            return(new.data.full)
                          })))



predict.wide2 <- temp2 %>%
  mutate(dbh.cat = factor(case_when(dbh <= 30 ~ "Small",
                                    dbh <= 60 ~ "Intermediate",
                                    TRUE ~ "Large"),
                          levels = c("Small","Intermediate","Large")),
         liana.cat = factor(as.character(liana.cat),
                            levels = c("no","low","high"))) %>%
  mutate(diff = h.m - h.null.m,
         diff.rel = (h.m - h.null.m)/h.null.m)


ggplot(data = predict.wide2) +
  geom_boxplot(aes(x = as.factor(liana.cat), y = -diff.rel, fill = site)) +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  labs(x = "") +
  theme_bw()

