

site.loc <- readRDS("./outputs/site.loc.RDS") %>%
  dplyr::select(-site.group) %>%
  distinct()

# Load the data
all.df <- (readRDS(  "./outputs/All.COI.data.RDS") %>%
             mutate(sp = str_squish(sp)) %>%
             filter(dbh >= 10)) %>%
  mutate(genus = stringr::word(sp,1),
         species = word(sp, 2, str_count(sp, '\\s')+1)) %>%
  left_join(site.loc %>%
              rename(site = site.common) %>%
              distinct(),
            by = "site")


df.species <- all.df %>%
  group_by(site,sp,liana.cat) %>%
  summarise(N = n(),
            .groups = "keep") %>%
  pivot_wider(names_from = liana.cat,
              values_from = N) %>%
  rowwise() %>%
  mutate(tot = sum(no,low,high,na.rm = TRUE)) %>%
  filter(no >= 10, low >= 10, high >= 10,
         (no + low + high) >= 50) %>%
  filter(!(sp %in% c("Unknown_Australia","indet indet")))

df.species.sum <- df.species %>%
  group_by(site) %>%
  summarise(N = n()) %>%
  arrange(desc(N)) %>%
  left_join(site.loc %>%
              dplyr::select(-N) %>%
              rename(site = site.common),
            by = "site")


# load PFT map
r <- raster("/home/femeunier/Documents/projects/LianaRemovalRevisited/data/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1_pantropical_aggr.tif")
df.r <- as.data.frame(r,xy = TRUE) %>%
  rename(lon =  x,
         lat = y,
         LU = C3S.LC.L4.LCCS.Map.300m.P1Y.2020.v2.1.1_pantropical_aggr) %>%
  filter(!is.na(LU))


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


ggplot() +
  geom_raster(data = df.r,
              aes(x = lon, y = lat, fill = as.factor(LU)),
              alpha = 0.4,show.legend = FALSE) +
  geom_sf(data = world,
          fill = NA,
          color = "black",
          alpha = 0.5) +
  geom_point(aes(x = lon, y = lat),
             data = df.species.sum, shape = 1,
             alpha = 1,
             size = 1, color = "red") +
  scale_fill_manual(values = c("white",c("#72a83d"),"darkgreen")) +
  scale_y_continuous(limits = c(-30,10)) +
  scale_x_continuous(limits = c(-85,160),expand = c(0,0)) +
  scale_size_continuous(range = c(0.1, 2)) +
  labs(x = "", y = "") +
  theme_map() +
  guides(size = "none") +
  theme(text = element_text(size = 20))

