rm(list = ls())
gc()

library(ggplot2)
library(tidyverse)
library(dplyr)
library(magrittr)
library(tools)
library(stats)
library(brms)
library(dplyr)
library(BayesianTools)
library(bayesplot)
library(bayestestR)
library(cowplot)
library(ggpubr)
library(patchwork)
library(grid)
library(gridExtra)
library(purrr)
library(Metrics)
library(ggpubr)
library(boot)
library(grid)
library(gtable)
load("~/Downloads/tls_all2.Rda")
all.df <- tls.all2 %>%
  mutate(dbh = tls.dbh/10) %>%
  rename(h = tls.h,
         sp = Cod.Especies) %>%
  dplyr::filter(dbh >= 19.5) %>%
  group_by(sp) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  mutate(liana.cat = factor(case_when(Treatment =='C' ~ "high",
                                      Treatment =='R' ~ "no"),
                            levels = c("no","high")),
         slender = h/tls.dbh) %>%
  dplyr::select(-N)

alpha = 0.05
dbhs <- seq(20,150,1)
levels <- as.character(unique(all.df$liana.cat))

newdata <- data.frame()

for (ilevel in seq(1,length(levels))){
  ccdf <- all.df %>%
    dplyr::filter(liana.cat == levels[ilevel])
  cdbhs <- dbhs
  newdata <- bind_rows(list(newdata,
                            data.frame(
                              dbh = rep(cdbhs,1),
                              liana.cat = c(rep(levels[ilevel],length(cdbhs))))
  ))
}


cmodel <- readRDS("~/Downloads/Fit.weibull_a.RDS")
null.model <- readRDS("~/Downloads/Fit.weibull_none.RDS")


ccoef <- as.numeric(exp((summary(cmodel)[["spec_pars"]][1]**2)/2))
ccoef.null <- as.numeric(exp(summary(null.model)[["spec_pars"]][1]**2/2))


newdata <- newdata %>%
  mutate(liana.cat = case_when(liana.cat == "no" ~ 0,
                               liana.cat == "high" ~ 4,
                               TRUE ~ NA_integer_)) %>%
  ungroup() %>%
  mutate(id = 1:n())

# pp <- reshape2::melt(posterior_predict(cmodel,
#                                        newdata = newdata,
#                                        re_formula = NA)) %>%
#   rename(rep = Var1,
#          id = Var2) %>%
#   group_by(id) %>%
#   dplyr::filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
#   mutate(h = ccoef*exp(value)) %>%
#   summarise(h.m = mean(h,na.rm = TRUE),
#             h.low = quantile(h,alpha/2,na.rm = TRUE),
#             h.high = quantile(h,1 - alpha/2,na.rm = TRUE))
#
# pp.null <- reshape2::melt(posterior_predict(null.model,
#                                             newdata = newdata,
#                                             re_formula = NA)) %>%
#   rename(rep = Var1,
#          id = Var2) %>%
#   group_by(id) %>%
#   dplyr::filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
#   mutate(h.null = ccoef.null*exp(value)) %>%
#   summarise(h.null.m = mean(h.null,na.rm = TRUE),
#             h.null.low = quantile(h.null,alpha/2,na.rm = TRUE),
#             h.null.high = quantile(h.null,1 - alpha/2,na.rm = TRUE))


pep <- reshape2::melt(posterior_epred(cmodel,
                                      newdata = newdata,
                                      re_formula = NA)) %>%
  rename(rep = Var1,
         id = Var2) %>%
  group_by(id) %>%
  dplyr::filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
  mutate(h.pred = ccoef*exp(value))

new.data.h <- newdata %>%
  left_join(pep,
            by = "id") %>%
  dplyr::select(-c(value,id)) %>%
  pivot_wider(names_from = liana.cat,
              values_from = h.pred) %>%
  rename(no = `0`,
         high = `4`) %>%
  mutate(diff = -(high - no),
         diff_rel = -(high - no)/no)

# pep.null <- reshape2::melt(posterior_epred(null.model,
#                                            newdata = newdata,
#                                            re_formula = NA)) %>%
#   rename(rep = Var1,
#          id = Var2) %>%
#   group_by(id) %>%
#   dplyr::filter((abs(value - median(value,na.rm = TRUE)) < 2*sd(value,na.rm = TRUE))) %>%
#   mutate(h.null.pred = ccoef.null*exp(value)) %>%
#   summarise(h.null.pred.m = mean(h.null.pred,na.rm = TRUE),
#             h.null.pred.low = quantile(h.null.pred,alpha/2,na.rm = TRUE),
#             h.null.pred.high = quantile(h.null.pred,1 - alpha/2,na.rm = TRUE))

new.data.tls <- new.data.h %>%
  group_by(dbh) %>%
  summarise(diff.m = mean(diff,na.rm = TRUE),
            diff.low = quantile(diff,alpha/2,na.rm = TRUE),
            diff.high = quantile(diff,1 - alpha/2,na.rm = TRUE),

            diff_rel.m = mean(diff_rel,na.rm = TRUE),
            diff_rel.low = quantile(diff_rel,alpha/2,na.rm = TRUE),
            diff_rel.high = quantile(diff_rel,1 - alpha/2,na.rm = TRUE),
            .groups = "keep")

# new.data.tls[["h_m"]] <- pp[["h.m"]]
# new.data.tls[["h_low"]] <- pp[["h.low"]]
# new.data.tls[["h_high"]] <- pp[["h.high"]]
#
# new.data.tls[["h.pred_m"]] <- pep[["h.pred.m"]]
# new.data.tls[["h.pred_low"]] <- pep[["h.pred.low"]]
# new.data.tls[["h.pred_high"]] <- pep[["h.pred.high"]]
#
# new.data.tls[["h.null_m"]] <- pp.null[["h.null.m"]]
# new.data.tls[["h.null_low"]] <- pp.null[["h.null.low"]]
# new.data.tls[["h.null_high"]] <- pp.null[["h.null.high"]]

# new.data.tls[["h.null.pred_m"]] <- pep.null[["h.null.pred.m"]]
# new.data.tls[["h.null.pred_low"]] <- pep.null[["h.null.pred.low"]]
# new.data.tls[["h.null.pred_high"]] <- pep.null[["h.null.pred.high"]]
#
# new.data.tls2 <- new.data.tls %>%
#   tidyr::pivot_longer(cols = -c(dbh, liana.cat),
#                       names_to = c("model","para"),
#                       names_sep = '_',
#                       values_to = "Height",
#                       values_drop_na = FALSE) %>%
#   group_by(dbh,model,para) %>%
#   pivot_wider(names_from = para,
#               values_from = Height) %>%
#
#   mutate(diff.h = Height[liana.cat == 0] - Height,
#          diff.perc = 100* diff.h/Height[liana.cat ==0]) %>%
#   dplyr::filter(liana.cat == 4) %>%
#   pivot_wider(names_from = para,
#               values_from = c('Height','diff.h','diff.perc'))


## dual yaxis-----------------------------


ggplot(data = new.data.tls,
         aes(x = dbh))+
  geom_line(aes(y = diff.m),color = '#b21825')+
  geom_ribbon(aes(x = dbh,ymin = diff.low,ymax = diff.high),
              color = NA,fill = '#b21825',alpha=0.3)+
  geom_line(aes(y = (100*diff_rel.m)),color = '#252525')+
  geom_ribbon(aes(x = dbh,ymin = (100*diff_rel.low),ymax = (100*diff_rel.high)),
              color = NA,fill = '#252525',alpha=0.3) +
  scale_x_continuous(breaks = seq(0,125,20),limits = c(20,125))+
  scale_y_continuous(limits = c(0,10),
    name = "Height difference (m)",
    sec.axis = sec_axis(~(./1)+0,
                        name="Relative height difference(%)")) +
  xlab('DBH (cm)')+
  # annotate(geom ='text', x=50,y = c(2.1,2.05),
  #          label = paste0(c('Mean height difference: ',
  #                           'Mean percentage: '),
  #                         new.data.tls2.descrip %>% dplyr::filter(model =='h.pred') %>%
  #                           dplyr::select(c(diff.h_m.m, diff.perc_m.m)),
  #                         c(' (m)',' (%)')),
  #          color=c('#b21825','#252525'),
  #          size =6)+
  theme_bw()+
  theme(axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(color = '#b21825', size=15),
        axis.title.y.right = element_text(color = '#252525', size=15, vjust =2),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.y.left = element_line(color = "#b21825"),
        axis.text.y.left = element_text(color = "#b21825"),
        panel.border = element_blank(),
        panel.background = element_blank())

