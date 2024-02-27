rm(list = ls())

alpha = 0.11

all <- bind_rows(readRDS("./outputs/BCI.H.TLS.25.RDS") %>%
                   mutate(target = 25),
                 readRDS("./outputs/BCI.H.TLS.50.RDS") %>%
                   mutate(target = 50),
                 readRDS("./outputs/BCI.H.TLS.100.RDS") %>%
                   mutate(target = 100),
                 readRDS("./outputs/BCI.H.TLS.150.RDS") %>%
                   mutate(target = 150))

all %>%
  filter(!is.na(value)) %>%
  group_by(target,liana.cat) %>%
  summarise(m = 100*median((value-no)/no,na.rm = TRUE),
            low = 100*quantile((value-no)/no,alpha/2,na.rm = TRUE),
            high = 100*quantile((value-no)/no,1-alpha/2,na.rm = TRUE),

            m.abs = median((value-no),na.rm = TRUE),
            low.abs = quantile((value-no),alpha/2,na.rm = TRUE),
            high.abs = quantile((value-no),1-alpha/2,na.rm = TRUE)) %>%
  arrange(liana.cat,target)







all.CA <- bind_rows(readRDS("./outputs/BCI.CA.TLS.25.RDS") %>%
                   mutate(target = 25),
                 readRDS("./outputs/BCI.CA.TLS.50.RDS") %>%
                   mutate(target = 50),
                 readRDS("./outputs/BCI.CA.TLS.100.RDS") %>%
                   mutate(target = 100),
                 readRDS("./outputs/BCI.CA.TLS.150.RDS") %>%
                   mutate(target = 150))

all.CA %>%
  filter(!is.na(value)) %>%
  group_by(target,liana.cat) %>%
  summarise(m = 100*median((value-no)/no,na.rm = TRUE),
            low = 100*quantile((value-no)/no,alpha/2,na.rm = TRUE),
            high = 100*quantile((value-no)/no,1-alpha/2,na.rm = TRUE),

            m.abs = median((value-no),na.rm = TRUE),
            low.abs = quantile((value-no),alpha/2,na.rm = TRUE),
            high.abs = quantile((value-no),1-alpha/2,na.rm = TRUE)) %>%
  arrange(liana.cat,target)



all.AGB <- bind_rows(readRDS("./outputs/BCI.AGB.TLS.25.RDS") %>%
                      mutate(target = 25),
                    readRDS("./outputs/BCI.AGB.TLS.50.RDS") %>%
                      mutate(target = 50),
                    readRDS("./outputs/BCI.AGB.TLS.100.RDS") %>%
                      mutate(target = 100),
                    readRDS("./outputs/BCI.AGB.TLS.150.RDS") %>%
                      mutate(target = 150))

all.AGB %>%
  filter(!is.na(value)) %>%
  group_by(target,liana.cat) %>%
  summarise(m = 100*median((value-no)/no,na.rm = TRUE),
            low = 100*quantile((value-no)/no,alpha/2,na.rm = TRUE),
            high = 100*quantile((value-no)/no,1-alpha/2,na.rm = TRUE),

            m.abs = median((value-no),na.rm = TRUE),
            low.abs = quantile((value-no),alpha/2,na.rm = TRUE),
            high.abs = quantile((value-no),1-alpha/2,na.rm = TRUE)) %>%
  arrange(liana.cat,target)
