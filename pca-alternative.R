
### Juan's Take on PCA
# Juan 
# Source: https://twitter.com/juanleitonm/status/1406790143600640001
library(tidyverse)
library(lubridate)

data_processed <- read.csv("https://raw.githubusercontent.com/howardbaek/Upwelling-Image-Analysis/main/pilot_data_processed.csv")
data_processed %>% View()


data_processed %>%
  ggplot(aes(lon, lat, fill = sst)) +
  geom_raster() + 
  scale_fill_viridis_c() +
  facet_wrap(~date)

data_mean_sd <- data_processed %>% 
  group_by(lat, lon) %>% 
  summarise(d.mean = mean(sst),
            d.sd = sd(sst)) %>% 
  ungroup()

data_stand <- data_processed %>% 
  left_join(data_mean_sd) %>% 
  mutate(sst = (sst - d.mean) / d.sd) %>% 
  select(-d.mean, -d.sd)

data_resid <- data_processed %>% 
  drop_na() %>% 
  group_by(lat, lon) %>% 
  nest() %>% 
  mutate(res = map(.x = data, .f = ~lm(sst ~ date, data = .x) %>% resid)) %>% 
  unnest(c(data, res) ) %>% 
  select(-sst) %>% 
  rename(sst = res) %>% 
  mutate(sst = sst / sd(sst))

data_wide <- data_stand %>% 
  drop_na() %>% 
  pivot_wider(names_from = c(lat, lon), values_from = sst)

x <- data_wide %>% 
  select(-date) %>% 
  as.matrix()

svd_res <- svd(x)
A <- svd_res$u %*% diag(svd_res$d)
H <- t(svd_res$v)

A[, 1:2]

data_pca <- as_tibble(H) %>% 
  setNames(colnames(x)) %>% 
  rownames_to_column(var = "pc") %>% 
  mutate(pc = as.numeric(pc)) %>% 
  pivot_longer(names_to = "loc", values_to = "sst", cols = -pc) %>% 
  separate(col = loc, into = c("lat", "lon"), sep = "_", convert = TRUE)

data_pca %>% 
  filter(pc <= 10) %>% 
  ggplot(aes(lon, lat, fill = sst)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_wrap(~pc) +
  ggtitle("Tile")

data_pca %>% 
  filter(pc <= 10) %>% 
  ggplot(aes(lon, lat, fill = sst)) +
  geom_raster() +
  scale_fill_viridis_c() +
  facet_wrap(~pc) +
  ggtitle("Raster")