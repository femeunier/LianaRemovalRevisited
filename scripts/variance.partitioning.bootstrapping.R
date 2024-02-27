rm (list = ls())

library(dplyr)
library(geodata)
library(stringr)
library(plotbiomes)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(YGB)
library(xlsx)
library(reshape2)
library(raster)
library(stringr)
library(MASS)
library(caret)

op.delta.loss <- readRDS(file = "./outputs/Bootstrap.RDS")

op.delta.loss.long <- op.delta.loss %>%
  dplyr::select(map,op,op2,op3) %>%
  pivot_longer(cols = -c(map))

ggplot() +
  geom_density(data = op.delta.loss.long %>%
                 filter(name == "op"),
               aes(x = value,
                   group = as.factor(map)),
               fill = "darkgrey",
               alpha = 0.5, color = "black") +
  geom_density(data = op.delta.loss.long %>%
                 filter(name != "op"),
               aes(x = value),
               fill = "darkgrey",
               alpha = 0.5, color = "black") +
  facet_wrap(~name, scales = "free") +
  theme_bw()

op.selections <- c("op","op2","op3")

Ntest = 10

df.all <- df.part.all <- df2keep.all <-
  data.frame()

for (op.selection in op.selections){

  print(op.selection)
  # ggplot(data = op.delta.loss) +
  #   geom_boxplot(aes(x = as.factor(map), y = op)) +
  #   theme_bw()
  #
  # ggplot(data = op.delta.loss) +
  #   geom_density(aes(x = op2)) +
  #   theme_bw()
  #
  # ggplot(data = op.delta.loss) +
  #   geom_boxplot(aes(x = as.factor(w), y = op2)) +
  #   theme_bw()
  #
  # ggplot(data = op.delta.loss) +
  #   geom_boxplot(aes(x = as.factor(target), y = op2)) +
  #   theme_bw()
  #
  # ggplot(data = op.delta.loss,
  #        aes(x = op2, y = op3)) +
  #   geom_point() +
  #   stat_smooth(method = "lm", se = FALSE) +
  #   facet_wrap(~ map) +
  #   theme_bw()
  #
  # ggplot(data = op.delta.loss,
  #        aes(x = m.high, y = op2, color = as.factor(map))) +
  #   geom_point() +
  #   stat_smooth(method = "lm") +
  #   facet_wrap(~ as.factor(dbh)) +
  #   theme_bw()
  #
  # op.delta.loss %>%
  #   summarise(low = quantile(op2,0.025),
  #             m = mean(op2),
  #             high = quantile(op2,0.975),
  #             .groups = "keep")

  vars2remove <- c("i","op","op2","op3")
  vars2remove[vars2remove == op.selection] <- NA
  vars2remove <- vars2remove[!is.na(vars2remove)]

  unique.var <- colnames(op.delta.loss %>%
                           dplyr::select(-all_of(c(op.selection,vars2remove))))

  second <- expand.grid(Var1 = unique.var,
                        Var2 = unique.var) %>%
    mutate(var = paste0(Var1,":",Var2)) %>%
    pull(var)

  # third <- expand.grid(Var1 = unique.var,
  #                      Var2 = unique.var,
  #                      Var3 = unique.var) %>%
  #   mutate(var = paste0(Var1,":",Var2,":",Var3)) %>%
  #   filter(!((Var1 == Var2) & (Var3 == Var2))) %>%
  #   pull(var)

  df2keep <- data.frame()

  for (itest in seq(1,Ntest)){

    print(itest/Ntest)

    data2test <- op.delta.loss %>%
      ungroup() %>%
      filter(i %in% sort(sample(unique(i),
                                size = round(0.1*length(unique(i))),
                                replace = FALSE))) %>%
      dplyr::select(-all_of(vars2remove)) %>%
      mutate(dbh = factor(dbh))

    form <- paste(c(paste(unique.var,collapse = "+"),
                    sample(second,
                           size = sample(1:length(second),
                                         1),
                           replace = FALSE)),
                  collapse = "+")

    mod.all <- lm(as.formula(paste0(op.selection,"~",form)), data=data2test)

    anovobj <- aov(mod.all)
    allssq <- summary(anovobj)[[1]][,2]
    allssq/sum(allssq)*100
    print(sum(allssq[1:(length(allssq) - 1)])/sum(allssq)*100)


    df <- data.frame(name = c(rownames(summary(anovobj)[[1]])),
                     ssq = c(allssq)) %>%
      mutate(name = trimws(name, which = "right")) %>%
      mutate(p.var = 100*c(allssq/sum(allssq))) %>%
      arrange(desc(p.var)) %>%
      mutate(c.var = cumsum(p.var))

    df2keep <- bind_rows(df2keep,
                         df %>%
                           mutate(count = itest) %>%
                           filter(name != "Residuals") %>%
                           filter(p.var > 0.1)) %>%
      # dplyr::select(name,p.var) %>%
      distinct()

  }

  df2keep.all <- bind_rows(df2keep.all,
                           df2keep %>%
                             mutate(op = op.selection))

  Av <- df2keep %>%
    group_by(name) %>%
    summarise(p.var.m = mean(p.var)) %>%
    arrange(desc(p.var.m))

  all.vars <- unique(c(unique.var,
                       Av$name))

  anovobj2 <- aov(lm(formula =
                       as.formula(paste0(op.selection,"~",
                                         paste(all.vars,collapse = "+"))),
                     data = op.delta.loss %>%
                       # slice_sample(prop = 0.1) %>%
                       dplyr::select(-all_of(vars2remove)) %>%
                       mutate(dbh = factor(dbh)),
                     na.action=na.exclude))
  allssq2 <- summary(anovobj2)[[1]][,2]
  allssq2/sum(allssq2)*100
  print(sum(allssq2[1:(length(allssq2) - 1)])/sum(allssq2)*100)

  df2 <- data.frame(name = c(rownames(summary(anovobj2)[[1]]),"Total"),
                    ssq = c(allssq2,sum(allssq2))) %>%
    mutate(name = trimws(name, which = "right")) %>%
    mutate(p.var = 100*c(allssq2/sum(allssq2),1)) %>%
    arrange(desc(p.var)) %>%
    mutate(c.var = cumsum(p.var) - 100)

  df2.prep <- df2 %>%
    filter(name != "Total") %>%
    mutate(N = 1 + str_count(name,":"))


  df.part <- data.frame()
  for (cvar in unique.var){
    cdf.all <- df2.prep %>%
      filter(grepl(cvar,name, fixed = TRUE)) %>%
      ungroup() %>%
      summarise(p.var = sum(p.var/N),
                .groups = "keep")

    cdf.uni <- df2.prep %>%
      filter(name == cvar) %>%
      summarise(p.var = sum(p.var/N),
                .groups = "keep")

    df.part <- bind_rows(df.part,
                         data.frame(var = cvar,
                                    p.var.all = cdf.all[["p.var"]],
                                    p.var.uni = cdf.uni[["p.var"]]))

  }

  df.part.all <- bind_rows(df.part.all,
                           df.part %>%
                             mutate(op = op.selection))

  df.part.reclass <- df.part %>%
    mutate(var.reclass = case_when(var %in% c("var1","var2","intercept","m.high") ~ "LM",
                                   var %in% c("target","dbh") ~ "DBH.classes",
                                   TRUE ~ var)) %>%
    dplyr::select(-var) %>%
    rename(var = var.reclass) %>%
    group_by(var) %>%
    summarise(p.var.all = sum(p.var.all),
              p.var.uni = sum(p.var.uni))


  Labels <- df.part.reclass %>%
    arrange((p.var.all)) %>%
    pull(var)

  df.part.long <- df.part.reclass %>%
    rename(p.var.first = p.var.uni) %>%
    arrange((p.var.all)) %>%
    mutate(var = factor(var,
                        Labels)) %>%
    mutate(p.var.second = p.var.all - p.var.first) %>%
    dplyr::select(var,p.var.first,p.var.second) %>%
    pivot_longer(cols = c(p.var.first,p.var.second)) %>%
    mutate(long.name = paste0(var,"_",name)) %>%
    arrange(var,desc(name))


  Labels2 <- df.part.long %>%
    pull(long.name)

  df.part.long <- df.part.long %>%
    mutate(long.name = factor(long.name,
                              Labels2))

  df.all <- bind_rows(df.all,
                      df.part.long %>%
                        mutate(op = op.selection))

}

mycolors = c(brewer.pal(name="Blues", n = 4)[c(3,4)],
             brewer.pal(name="Purples", n = 4)[c(1,3)],
             brewer.pal(name="Oranges", n = 4)[c(1,3)],
             brewer.pal(name="Reds", n = 4)[c(3,4)])


ggplot() +
  geom_bar(data = data.frame(op = c("op","op2","op3"),
                      value = c(100,100,100)),
           aes(x=op, y=value),
           fill = NA, color = "black",
           stat = "identity") +
  geom_bar(data= df.all,
           aes(x=op, y=value, fill=long.name),
           stat="identity") +
  # scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_manual(values = mycolors) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())





# mod.all <- lm(op2 ~ .^2, data=data2test)
# AIC(mod.all)
# summary(mod.all)$r.squared
# # Forward
# fmod <- stepAIC(object = lm(data = data2test,
#                             formula = "op2 ~ 1"),
#                 direction = "forward",
#                 scope = formula(mod.all), trace=FALSE)

# # Backwards
# bmod <- stepAIC(mod.all,
#                 direction = "backward",
#                 scope = formula(lm(data = data2test,
#                                    formula = "op2 ~ 1")),
#                 trace=FALSE)
# AIC(bmod)
# summary(bmod)$r.squared
#
# set_train <- trainControl(method="repeatedcv", number=10, repeats=3)
# cvmod <- train(op2 ~ .,
#                data=data2test,
#                scope = formula(lm(data = data2test,
#                                   formula = "op2 ~ 1")),
#                method="lmStepAIC", direction="backward", trace=FALSE, trControl=set_train)
#
# AIC(cvmod$finalModel)
# summary(cvmod)$r.squared
#
# anovobj <- aov(cvmod$finalModel)
# allssq <- summary(anovobj)[[1]][,2]
# allssq/sum(allssq)*100
# sum(allssq[1:(length(allssq) - 1)])/sum(allssq)*100
#
#
# df <- data.frame(name = c(rownames(summary(anovobj)[[1]]),"Total"),
#                  ssq = c(allssq,sum(allssq))) %>%
#   mutate(name = trimws(name, which = "right")) %>%
#   mutate(p.var = 100*c(allssq/sum(allssq),1)) %>%
#   arrange(desc(p.var)) %>%
#   mutate(c.var = cumsum(p.var) - 100)


####################################################################################
# anovobj <- aov(lm(formula = as.formula(paste0("op2 ~ target + map + w + intercept + var1 + var2 + m.high +
#                                               target*dbh*m.high +
#                                               target*w +
#                                               w*var1 + w*var2 + w*dbh + target*dbh +
#                                               target*var1*m.high + target*var2*m.high +
#                                               target*var1*var2*m.high +
#                                               intercept*dbh +
#                                               var1*dbh + var2*dbh + var1*var2*dbh +
#                                               var1*var2*dbh*m.high")),
#                   data = data2test ,
#                   na.action=na.exclude))
# allssq <- summary(anovobj)[[1]][,2]
# allssq/sum(allssq)*100
# sum(allssq[1:(length(allssq) - 1)])/sum(allssq)*100
#
# df <- data.frame(name = c(rownames(summary(anovobj)[[1]]),"Total"),
#                  ssq = c(allssq,sum(allssq))) %>%
#   mutate(name = trimws(name, which = "right")) %>%
#   mutate(p.var = 100*c(allssq/sum(allssq),1)) %>%
#   arrange(desc(p.var)) %>%
#   mutate(c.var = cumsum(p.var) - 100)
#
# df %>% filter(p.var > 0.05) %>% pull(name)

save.image(file = "./outputs/VarPart.RData")
