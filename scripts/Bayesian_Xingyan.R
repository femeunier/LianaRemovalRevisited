rm(list = ls())

library(tidyverse)
library(brms)
library(dplyr)
library(ggplot2)
library(BayesianTools)
library(bayesplot)
library(bayestestR)
library(tidyr)
library(ggpubr)
library(minpack.lm)
library(tidybayes)


load("/home/femeunier/Downloads/Model_weibull_output_4plots_2.RData")

trees.extracted <- bind_rows(list(read.csv("/home/femeunier/Downloads/TreeAttri_Rplot.csv") %>%
                                    dplyr::select(DBH,Height) %>%
                                    rename(dbh = DBH) %>%
                                    mutate(plot = 11,
                                           treatment = "removal"),
                                  read.csv("/home/femeunier/Downloads/TreeAttri_Cplot.csv") %>%
                                    dplyr::select(DBH,Height) %>%
                                    rename(dbh = DBH) %>%
                                    mutate(plot = 13,
                                           treatment = "control"))) %>%
  mutate(liana = case_when(treatment == "control" ~ TRUE,
                           TRUE ~ FALSE)) %>%
  group_by(treatment) %>%
  filter(dbh >= 20)

# #3.73641086e+01, 2.64599993e+01, 1.52154763e-03
# #4.54880973e+01, 3.45741745e+01, 7.82741910e-04
# ##################################################################################################
# # ED2-type of allometry
#
prior_ED <- c(set_prior("normal(62,20)",  nlpar = "href"),
              set_prior("normal(0.0352,0.02)", nlpar = "b1Ht"),
              set_prior("normal(0.694,0.3)", nlpar = "b2Ht"))

ed_form1 <- brmsformula(Height ~ href*(1-exp(-b1Ht*(dbh**b2Ht))),
                        href + b1Ht + b2Ht ~ 1,
                        nl = TRUE)

ed_form8 <- brmsformula(Height ~ href*(1-exp(-b1Ht*(dbh**b2Ht))),
                        href + b1Ht + b2Ht ~ 1 + liana,
                        nl = TRUE)
ed_fit1 = brm(ed_form1,
              data = trees.extracted,
              prior = prior_ED,
              chains = 2, iter = 5000,
              control = list(adapt_delta = 0.95,max_treedepth = 20))
ed_fit8 = brm(ed_form8,
              data=trees.extracted,
              prior=prior_ED,
              chains = 2, iter = 5000,
              control = list(adapt_delta = 0.99,max_treedepth = 20))

plot(ed_fit1)
plot(ed_fit8)

waic1 <- waic(ed_fit1)
waic8 <- waic(ed_fit8)


census.data.new <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/c97to18.csv",stringsAsFactors = FALSE)
census.data.new.liana <-
  census.data.new %>%
  rename(dbh = dbhtot18,
         h  = hght18) %>%
  mutate(liana.cat = case_when(liana18 %in% c(0) ~ "no",
                               liana18 %in% c(1,2) ~ "low",
                               TRUE ~ "high")) %>%
  filter(dbh > 1,
         !is.na(h),
         !is.na(dbh),
         !is.na(liana18)) %>%
  mutate(liana.cat = factor(liana.cat,
                            levels = c("no","low","high")))

# better fit

test <- census.data.new.liana %>%
  filter(dbh > 100) %>%
  group_by(liana.cat) %>%
  slice_head(n = 100)

ed_form1bis <- brmsformula(h ~ href*(1-exp(-b1Ht*(dbh**b2Ht))),
                        href + b1Ht + b2Ht ~ 1,
                        nl = TRUE)

ed_fit1.new = brm(ed_form1bis,
              data = test,
              prior = prior_ED,
              chains = 2, iter = 5000,
              control = list(adapt_delta = 0.95,max_treedepth = 20))

ed_form8bis <- brmsformula(h ~ href*(1-exp(-b1Ht*(dbh**b2Ht))),
                        href + b1Ht + b2Ht ~ 1 + liana.cat,
                        nl = TRUE)

ed_fit8.new = brm(ed_form8bis,
              data=test,
              prior=prior_ED,
              chains = 2, iter = 5000,
              control =
                list(adapt_delta = 0.99,max_treedepth = 20))



# waic1 <- waic(ed_fit1.new)
# waic8 <- waic(ed_fit8.new)


# compare both models
loo_compare(waic1,
            waic8)

pp_check(ed_fit8)


df.QoF <- bind_rows(list(
  data.frame(obs = trees.extracted$Height,
             fit = predict(ed_fit8)[,"Estimate"],
             low = predict(ed_fit8)[,"Q2.5"],
             high = predict(ed_fit8)[,"Q97.5"],
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

df.fit2 <- df.fit %>% dplyr::filter(type != 'all')

plot.infor <- data.frame(x= c(40,40),y =c(42,45),
                         infor = c("Control plot: 99 trees",'Removal plot:90 trees'),
                         Treatment = c('C','R')) %>%
  group_by(Treatment)


census.data <- read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/c97to13.csv",stringsAsFactors = FALSE)

census.data.filt <- census.data %>%
  dplyr::select(tag,sp,sp13,
                dbhtot98,dbhtot13,code13,gx13,gy13,origin,
                status98,status13,
                hght98,hght13,plot2030,plot3030,plot4040,block,repl,tmt)
control.plots <- census.data.filt %>% filter(tmt == "CTL")


# df.height <- data.frame(dbh = c(trees$dbhtot98,trees$dbhtot13)/10,
#                         h = c(trees$hght98,census.data$hght13)) %>%
#   filter(dbh > 1,!is.na(h),!is.na(dbh))

df.height <- bind_rows(list(
  data.frame(dbh = control.plots %>% filter(status13 == "A") %>% pull(dbhtot13)/10,
             h = control.plots %>% filter(status13 == "A") %>% pull(hght13)) %>%
    mutate(time = 2013),
  data.frame(dbh = control.plots %>% filter(status98 == "A") %>% pull(dbhtot98)/10,
             h = control.plots %>% filter(status98 == "A") %>% pull(hght98)) %>%
    mutate(time = 1998))) %>%
  filter(dbh > 1,
         !is.na(h),
         !is.na(dbh))




ggplot(data = census.data.new.liana) +
  geom_density(aes(x = dbh/10, fill = liana.cat),
               color = NA, alpha = 0.5) +
  theme_bw()

census.data.new.liana %>%
  filter(dbh > 100) %>%
  group_by(liana.cat) %>%
  summarise(dbh.min = min(dbh),
            dbh.max = max(dbh),
            dbh.m = mean(dbh),
            dbh.med = median(dbh),
            dbhQ1 = quantile(dbh,0.25),
            dbhQ3 = quantile(dbh,0.75),
            n = n())

ggplot(data = census.data.new.liana %>%
         filter(dbh > 100),
       aes(x = dbh/10, y = h, color = as.factor(liana.cat))) +
  geom_point(alpha = 0.5, size = 0.25) +
  stat_smooth(se = FALSE,method = "lm") +
  scale_x_log10() +
  scale_y_log10(limits = c(3,50)) +
  labs(x = "DBH (cm)", y = "Height (m)", color = "Liana infestation") +
  theme_bw() +
  theme(legend.position = c(0.14,0.85),
        text = element_text(size = 20))

census.data.new.liana %>%
  filter(dbh > 100) %>%
  group_by(liana.cat) %>%
  summarise(n = n())

summary(lm(data = census.data.new.liana,
   formula = log(h) ~ log(dbh/10)))

summary(lm(data = census.data.new.liana,
           formula = log(h) ~ log(dbh/10)*as.factor(liana.cat)))


df.height <- bind_rows(list(
  data.frame(dbh = census.data.filt %>% pull(dbhtot13)/10,
             tmt = census.data.filt %>% pull(tmt),
             h = census.data.filt %>% pull(hght13)) %>%
    mutate(time = 2013),
  data.frame(dbh = census.data.filt %>% pull(dbhtot98)/10,
             tmt = census.data.filt %>% pull(tmt),
             h = census.data.filt %>% pull(hght98)) %>%
    mutate(time = 1998))) %>%
  filter(dbh > 1,
         !is.na(h),
         !is.na(dbh))

df.height %>%
  group_by(tmt,time) %>%
  summarise(N = n())

allom.pnas <- data.frame(dbh = dbhs) %>%
  mutate(h.orig = 51.38 * (1 - exp(-0.01322 * ((dbh*10)**0.6465))))


m0 <- nlsLM(data = df.height %>%
              filter(time == 1998,
                     dbh > 20),
            h ~ a*(1 - exp(-b*((dbh*10)**c))),
            start=list(a=51.38, b=0.01322, c = 0.6465),
            lower = c(0,-Inf,0),
            upper = c(Inf,Inf,Inf),
            control = nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024/10,
                                  printEval = TRUE, warnOnly = TRUE))

df.fit.LM <- data.frame(dbh = dbhs,
                        h = predict(m0,
                                    newdata = data.frame(dbh = dbhs)))


ggplot() +
  geom_point(data = df.height %>% filter(dbh >= 0),
             aes(x = dbh,y = h),
             color = "darkgrey",
             size = 0.2,
             alpha = 0.5) +

  geom_point(data = trees.extracted,
             aes(x = dbh, y = Height, color = liana,),
             size = 1,
             show.legend=FALSE) +

  geom_line(data = df.fit2,
            aes(x = dbh,y = fit,color = type),size = 1) +

  geom_line(data = allom.pnas,
            aes(x = dbh, y = h.orig), color = "black", linetype = 2) +

  geom_line(data = df.fit.LM,
            aes(x = dbh, y = h), color = "red") +


  # geom_point(data = df.height, aes(x= dbh, y = h),color = '#636363',size = 0.5) +

  labs(x = "DBH (cm)", y = "Height (m)") +
  scale_x_continuous(limits = c(0,200)) +

  # facet_wrap(~ time) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 20))


# dbh = 26
# fixef.med <- apply(fixef(ed_fit8, summary = FALSE),2,median)
# fixef.m <- apply(fixef(ed_fit8, summary = FALSE),2,mean)
# model.output <- data.frame(href = c(fixef.med["href_Intercept"] + fixef.med["href_lianaTRUE"], fixef.med["href_Intercept"],
#                                     fixef.m["href_Intercept"] + fixef.m["href_lianaTRUE"], fixef.m["href_Intercept"]),
#                            b1Ht = c(fixef.med["b1Ht_Intercept"] + fixef.med["b1Ht_lianaTRUE"], fixef.med["b1Ht_Intercept"],
#                                     fixef.m["b1Ht_Intercept"] + fixef.m["b1Ht_lianaTRUE"], fixef.m["b1Ht_Intercept"]),
#                            b2Ht = c(fixef.med["b2Ht_Intercept"] + fixef.med["b2Ht_lianaTRUE"], fixef.med["b2Ht_Intercept"],
#                                     fixef.m["b2Ht_Intercept"] + fixef.m["b2Ht_lianaTRUE"], fixef.m["b2Ht_Intercept"]),
#                            value = c('med','med','m','m'),
#                            treatment = c('C','R','C','R')) %>% group_by(value,treatment) %>%
#   mutate(h = href*(1-exp(-b1Ht*(dbh**b2Ht))))
#
# predict(ed_fit8,newdata = data.frame(dbh = c(dbh,dbh),liana = c(TRUE,FALSE))) %>%
#   as.data.frame() %>% mutate(Treatment = c('R','C'))



model_fit <- bind_rows(list(data.frame(dbh = c(26),
                        liana = c(TRUE)) %>%
    add_predicted_draws(ed_fit8) %>%
  rename(pred_le = .prediction) %>%
  dplyr::select(-c(.chain,.iteration)) %>% mutate(le = TRUE),

  data.frame(dbh = c(26),
                 liana = c(FALSE)) %>%
        add_predicted_draws(ed_fit8) %>%
        rename(pred_le = .prediction) %>%
        dplyr::select(-c(.chain,.iteration)) %>% mutate(le = FALSE)
  ))


ggplot(data = model_fit) +
  geom_density(aes(x = pred_le, fill = le),
               color = NA, alpha = 0.5) +
  theme_bw()

model_fit %>%
  group_by(le) %>%
  summarise(m = mean(pred_le),
            med = median(pred_le),
            max = max(pred_le),
            min = min(pred_le))

q1 <- ggplot() +
  geom_point(data = trees.extracted,
             aes(x = dbh, y = Height, color = liana,),
             size = 2,show.legend=FALSE) +
  geom_point(data = df.height, aes(x= dbh, y = h),color = '#636363',size = 0.5)+
  geom_line(data = df.fit2,aes(x = dbh,y = fit,color = type),size = 1.5) +
  geom_line(data = allom.pnas,aes(x= dbh, y = h.orig), color = '#636363') +
  geom_ribbon(data = df.fit2,
              aes(x= dbh, ymin = low, ymax = high, fill = type), alpha = 0.2,
              show.legend=FALSE) +
  geom_text(data = plot.infor, aes(label = infor, x = x, y = y),
            size = 5, color = c("#00BFC4","#F8766D")) +
  scale_color_manual(labels = c("R", "C"), values = c("#F8766D", "#00BFC4"))+
  labs(color = 'Treatment') +
  # scale_x_log10() +
  # scale_y_log10() +
  scale_y_continuous(limits = c(0,50)) +
  scale_x_continuous(limits = c(0, 120))

q2 <- ggplot() +
  geom_boxplot(data = trees.extracted,
               aes(x = treatment, y = Height, fill = liana),alpha = 0.2)+
  scale_y_continuous(limits = c(0,50))

get_legend(q1)




q3 <- ggplot() +
  geom_boxplot(data = trees.extracted,
               aes(x = treatment, y = dbh, fill = liana),alpha = 0.2) +
  scale_y_continuous(limits = c(0, 120)) +
  coord_flip()


library(cowplot)



legend.allom <- get_legend(q1 + guides(color = guide_legend(ncol = 1)))

fig.allom<- plot_grid(q2 + rremove('xlab') +
                        theme(legend.position="none"),
                      q1 + rremove('xlab') + rremove('ylab') +
                        theme(axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.title.y = element_blank(),
                              axis.text.x = element_blank(),
                              axis.ticks.x = element_blank(),
                              axis.title.x = element_blank())+
                        theme(legend.position="none"),
                      NULL,
                      q3 + rremove('ylab') +
                        theme(legend.position="none"),
                      align = "hv",rel_widths = c(1,3),rel_heights = c(3,1.5))

annotate_figure(fig.allom,
                top = text_grob ("Allometric function comparison",size = 14))



h.True <- df.fit2 %>% filter(type == TRUE &dbh>=20 & dbh<=120) %>% select(fit:high)
h.False <- df.fit2 %>% filter(type == FALSE &dbh>=20 & dbh<=120) %>% select(fit:high)
h.diff <- h.False - h.True
dbh.diff <- bind_cols(dbh = seq(20,120),
                      h.diff)
ggplot(data = dbh.diff) +
  geom_line(aes(x= dbh, y = fit),size = 1) +
  geom_ribbon(aes(x= dbh, ymin = low, ymax = high), alpha = 0.1) +
  ylab("Heigh diff") + xlab("DBH") +
  ggtitle("Height difference between liana treatment")

