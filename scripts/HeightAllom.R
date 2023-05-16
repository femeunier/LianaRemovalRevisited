rm(list = ls())

library(ggplot2)
library(minpack.lm)
library(LVLRE.long)
library(dplyr)
library(Hmisc)
library(cowplot)
library(tidyr)

# trees.extracted <- bind_rows(list(read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/plot11_2016_tree.csv") %>%
#                                     dplyr::select(DBH_HT.cm.,Height) %>%
#                                     rename(dbh = DBH_HT.cm.,
#                                            h = Height) %>%
#                                     mutate(plot = 11,
#                                            treatment = "removal"),
#                                   read.csv("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/plot13_para.csv") %>%
#                                     dplyr::select(DBH_LSR,Height) %>%
#                                     rename(dbh = DBH_LSR,
#                                            h = Height) %>%
#                                     mutate(plot = 13,
#                                            treatment = "control"))) %>%
#   mutate(ltreat = case_when(treatment == "control" ~ 0,
#                             TRUE ~ 1))


trees.extracted <- bind_rows(list(read.csv("/home/femeunier/Downloads/para11.csv") %>%
                                    dplyr::select(DBH_HT,Height) %>%
                                    rename(dbh = DBH_HT,
                                           h = Height) %>%
                                    mutate(plot = 11,
                                           treatment = "removal"),
                                  read.csv("/home/femeunier/Downloads/para13.csv") %>%
                                    dplyr::select(DBH_HT,Height) %>%
                                    rename(dbh = DBH_HT,
                                           h = Height) %>%
                                    mutate(plot = 13,
                                           treatment = "control") )) %>%
  mutate(ltreat = case_when(treatment == "control" ~ 0,
                            TRUE ~ 1))


Slenderness <- trees.extracted %>% mutate(S = h/dbh)

ggplot(data = Slenderness) +
  geom_boxplot(aes(x = treatment, y = S)) +
  theme_bw()

summary(aov(data = Slenderness %>% filter(treatment == "control" | (treatment == "removal" & S < 1.5)),
            formula = S ~ treatment))

trees.extracted %>% group_by(treatment) %>% summarise(N = length(h),
                                                      .groups = "keep")

LM <- lm(trees.extracted,formula = h~dbh)
summary(LM)

y0 <- trees.extracted$h
f0 <- predict(LM)
1-sum((y0-f0)^2)/(length(y0)*var(y0))


href <- 61.7;b1Ht <- 0.0352;b2Ht <- 0.694
m0 <- nlsLM(data = trees.extracted,
            h ~ href*(1 -exp(-b1Ht*((dbh)**b2Ht))),
            start=list(href=href, b1Ht=b1Ht, b2Ht = b2Ht), control = nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024/10,
                                                                                 printEval = TRUE, warnOnly = TRUE))
y <- trees.extracted$h
f <- predict(m0)
1-sum((y-f)^2)/(length(y)*var(y))

href0 <- coef(m0)[1]; b1Ht0 <- coef(m0)[2]; b2Ht0 <- coef(m0)[3]
dbhs <- seq(1,200)
df.predicted <- data.frame(dbh = dbhs, h = href0*(1 -exp(-b1Ht0*((dbhs)**b2Ht0))))


a <- 0; b <- 0 ; c <- 0
m1 <- nlsLM(data = trees.extracted,
            h ~ (href + a*ltreat)*(1 -exp(-(b1Ht + b*ltreat)*((dbh)**(b2Ht + c*ltreat)))),
            start=list(href=as.numeric(href), b1Ht=as.numeric(b1Ht), b2Ht = as.numeric(b2Ht),
                       a = a, b = b, c = c),
            control = nls.control(maxiter = 500, tol = 1e-05,  minFactor = 1/1024/100000,
                                  printEval = TRUE, warnOnly = TRUE))
y <- trees.extracted$h
f <- predict(m1)
1-sum((y-f)^2)/(length(y)*var(y))


href1 <- as.numeric(coef(m1)[1]); b1Ht1 <- as.numeric(coef(m1)[2]); b2Ht1 <- as.numeric(coef(m1)[3])
a <- as.numeric(coef(m1)[4]); b <- as.numeric(coef(m1)[5]); c <- as.numeric(coef(m1)[6])

df.predicted1 <- bind_rows(list(data.frame(dbh = dbhs, h = (href1)*(1 -exp(-b1Ht1*((dbhs)**b2Ht1))), treatment = "control"),
                                data.frame(dbh = dbhs, h = (href1 + a)*(1 -exp(-(b1Ht1 + b)*((dbhs)**(b2Ht1 + c)))), treatment = "removal")))


##########################################################################################
# Geertje allometry
df.default <- data.frame(dbh = dbhs, h = 51.38*(1 - exp(-0.01322*(dbhs*10)**0.6465)))

y1 <- trees.extracted$h
f1 <- 51.38*(1 - exp(-0.01322*(trees.extracted$dbh*10)**0.6465))

1-sum((y1-f1)^2)/(length(y1)*var(y1))

##########################################################################################
# Joe Wright's plots

census.data <- read.csv(file.path(getwd(),"data","c97to13.csv"),stringsAsFactors = FALSE)

census.data.filt <- census.data %>% dplyr::select(tag,sp,sp13,dbhtot13,code13,gx13,gy13,origin,status13,hght13,plot2030,plot3030,plot4040,
                                                  block,repl,tmt)

control.plots <- census.data.filt %>% filter(tmt == "CTL")
trees <- control.plots %>% filter(status13 %in% c("A","P"))

df.height <- data.frame(dbh = c(census.data$dbhtot98,census.data$dbhtot13)/10,
                        h = c(census.data$hght98,census.data$hght13)) %>% filter(dbh > 1,!is.na(h),!is.na(dbh))

ggplot(data = df.height) +
  geom_point(aes(x = dbh,y = h)) +
  theme_bw()

# Fit dbh vs tree allometry
href <- 61.7;b1Ht <- 0.0352; b2Ht <- 0.694
m_joe <- nlsLM(data = df.height,
            h ~ href*(1 -exp(-b1Ht*((dbh)**b2Ht))),
            start=list(href=href, b1Ht=b1Ht, b2Ht = b2Ht), control = nls.control(maxiter = 500, tol = 1e-05, minFactor = 1/1024/10,
                                                                                 printEval = TRUE, warnOnly = TRUE))



df.Joe <- data.frame(dbh = dbhs,
                     h = coef(m_joe)[1]*(1 -exp(-coef(m_joe)[2]*((dbhs)**coef(m_joe)[3]))))

# plot(df.height$dbh/10,df.height$h,xlab = "DBH (mm)",ylab = "H (m)")
# lines(dbhs,coef(m_joe)[1]*(1 -exp(-coef(m_joe)[2]*((dbhs*10)**coef(m_joe)[3]))),col = 'green')
# lines(dbhs,51.38 * (1 - exp(-0.01322*((dbhs*10)**0.6465))),col = 'red')


extr <- c(min(trees.extracted$dbh),max(trees.extracted$dbh))

Plot.allom <- ggplot(data = trees.extracted,
       aes(x = dbh,y = h)) +
  geom_point(data = df.height,
             aes(x = dbh,y = h),size = 0.1,color = "darkgrey") +           # Joe
  geom_point(aes(color = treatment)) +                           # TLS data
  geom_point(shape = 1, color = "black") +                           # TLS data
  geom_line(data = df.predicted,
            aes(x = dbh, y = h), color = "black") +       # Best fit
  # geom_line(data = df.default,
  #           aes(x = dbh, y = h), color = "green") +     # Geertje
  geom_line(data = df.predicted1,
            aes(x = dbh, y = h, color = treatment)) +       # Best fit multiple pfts
  # stat_smooth(aes(color = treatment),
  #             method = "lm", se = FALSE) +
  geom_line(data = df.Joe,
            aes(x = dbh, y = h), color = "darkgrey",linetype = 2) +     # Joe best fit
  scale_x_continuous(limits = c(1,200)) +
  # scale_x_log10(breaks = c(1,10,100),limits = c(1,200)) +
  # scale_y_log10() +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = c(0.2,0.85))

Plot.allom
# N.select <- 10
# N.loop.max <- 1000
# Nobs.max <- 100
# obj.mean <- mean(trees.extracted$DBH_HT.cm.)
# obj.median <- median(trees.extracted$DBH_HT.cm.)
#
# df.height.cat <- df.height %>%
#   mutate(cat = case_when(dbh < 5 ~ 0,
#                          dbh < 20 ~ 1,
#                          dbh < 30 ~ 2,
#                          dbh < 40 ~ 3,
#                          dbh < 50 ~ 4,
#                          dbh < 60 ~ 5,
#                          TRUE ~ 6)) %>%
#   dplyr::filter(cat > 0)
#
# i <- 1; err <- Inf ; Nobs <- 1 ; cmedian.diff <- cmean.diff <- c()
# while(i <= N.loop.max & Nobs < Nobs.max){
#
#   cdf <- df.height.cat %>% dplyr::filter(dbh >= extr[1],
#                                          dbh<= (extr[2]*2)) %>%
#     group_by(cat) %>% sample_n(size = N.select,replace = FALSE)
#
#   # hist(cdf$dbh)
#
#   cmean <- mean(cdf$dbh)
#   cmedian <- median(cdf$dbh)
#   cerr <- sqrt((cmean-obj.mean)**2 + (obj.median - cmedian)**2)
#
#   if (cerr < err){
#     df2keep <- cdf
#     err <- cerr
#     print(err)
#   }
#   i <- i+ 1
#
#   if (cerr < 5){
#     cmedian.diff[Nobs] <- median(trees.extracted$Height) - median(cdf$h)
#     cmean.diff[Nobs] <- mean(trees.extracted$Height) - mean(cdf$h)
#
#     Nobs <- Nobs + 1
#   }
# }

all.h <- bind_rows(list(trees.extracted %>% dplyr::select(dbh,h,treatment) %>% mutate(type = "TLS"),
                        df.height %>% mutate(treatment = "All",
                                             type = "Joe")))

H.plot <- ggplot(data = all.h) +
  geom_boxplot(aes(x = treatment, y = h, fill = treatment),outlier.shape = NA) +
  # scale_y_log10() +
  scale_fill_manual(values = c("white","#F8766D","#00BFC4")) +
  labs(x = "", y = "Height (m)") +
  theme_bw() +
  guides(fill = "none")

DBH.plot <- ggplot(data = all.h) +
  geom_boxplot(aes(x = treatment, y = dbh,fill = treatment),outlier.shape = NA) +
  scale_y_continuous(limits = c(1,200)) +
  # scale_y_log10(breaks = c(1,10,100),limits = c(1,200)) +
  scale_fill_manual(values = c("white","#F8766D","#00BFC4")) +
  coord_flip() +
  labs(x = "", y = "DBH (cm)") +
  theme_bw() +
  guides(fill = "none")

void <- ggplot() + theme_void()

plot_grid(H.plot,Plot.allom,void,DBH.plot, align = "hv",
          nrow = 2, ncol = 2,rel_widths = c(1,3), rel_heights = c(3,1))


df.diff <- df.predicted1 %>% pivot_wider(names_from = treatment,
                                         values_from = h) %>%
  mutate(diff = removal - control)

hist(df.diff$diff)

dbhs <- seq(1,200)
summary(((0.0673 * 0.5 * ((df.diff$control) * (dbhs ** 2)) ** 0.976) -
  (0.0673 * 0.5 * ((df.diff$removal) * (dbhs ** 2)) ** 0.976)) /
  (0.0673 * 0.5 * ((df.diff$removal) * (dbhs ** 2)) ** 0.976))

