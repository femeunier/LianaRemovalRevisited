rm(list = ls())

cDBHtarget = 50

Main.OP <- readRDS(paste0("./outputs/All.COI.data.RDS")) %>%
  filter(dbh >= cDBHtarget) %>%
  group_by(site) %>%
  summarise(coi.m = mean(coi,
                         na.rm = TRUE),
            coi.wm = weighted.mean(coi,dbh**2,
                                   na.rm = TRUE),
            N = sum(!is.na(coi)),
            .groups = "keep")

Main.OP2 <- readRDS(paste0("./outputs/Main.OP.",cDBHtarget,".RDS")) %>%
  group_by(site) %>%
  filter(liana.cat == "high") %>%
  summarise(delta = mean(diff_h, na.rm = TRUE),
            .groups = "keep")

all <- Main.OP %>%
  left_join(Main.OP2,
            by = "site")


ggplot(data = all %>%
         filter(N > 10),
       aes(x = coi.wm, y = delta)) +
  geom_point(aes(size = N)) +
  stat_smooth(method = "lm") +
  theme_bw()


summary(lm(data = all %>%
     filter(N > 10),
   formula = delta ~ coi.wm,
   weights = N))
