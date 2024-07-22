library(tidyverse)
library(tidybayes)
library(brms)
library(rstan)
library(bayesplot)
library(ggthemes)
library(janitor)
library(cowplot)
library(modelr)
source("code/functions.R")

rstan_options(auto_write = TRUE)

# Load data ----------------------------------------------------------------
#####################
#####################
## These are not the original data!! For the original data, contact Dr. Greg Ruggerone.
## The data loaded below are modified from the original and should be used only for 
## ensuring that the code runs.
#####################

millions_fish = fish_escapement <- read_csv("data/raw_data/fish_escapement.csv") %>% clean_names() %>% 
  pivot_longer(cols = c(-year, -metric, -source)) %>% 
  mutate(species = case_when(grepl("pink", name) ~ "Pink",
                             grepl("chum", name) ~ "Chum",
                             grepl("sockeye", name) ~ "Sockeye",
                             grepl("chinook", name) ~ "Chinook",
                             TRUE ~ "Coho"),
         location = case_when(grepl("bering", name) ~ "BeringSea",
                              grepl("central", name) ~ "CentralAK",
                              grepl("seak", name) ~ "SEAK",
                              grepl("bc_wc", name) ~ "BCWC")) %>% 
  select(year, name, value) %>% 
  rename(millions_of_fish = value) %>% 
  mutate(source = "For details of collection, contact Dr. Greg Ruggerone")

write_csv(millions_fish, file = "data/millions_fish.csv")

kg_ind_fish = read_csv("data/raw_data/fish_mass_kgww_of_individual_fish.csv") %>% 
  clean_names() %>% 
  pivot_longer(cols = -c(year, units, source)) %>% 
  select(year, name, value) %>% 
  rename(kg_ind = value) %>% 
  mutate(source = "For details of collection, contact Dr. Greg Ruggerone")

write_csv(kg_ind_fish, file = "data/kg_ind_fish.csv")

escapement_mt = left_join(millions_fish, kg_ind_fish) %>% 
  mutate(kg_escape = kg_ind*millions_of_fish*1e6,
         mt_escape = kg_escape/1000) %>% 
  mutate(location = case_when(grepl("bering", name) ~ "BeringSea",
                              grepl("central", name) ~ "CentralAK",
                              grepl("seak", name) ~ "SEAK",
                              TRUE ~ "BCWC")) %>% 
  mutate(species = case_when(grepl("pink", name) ~ "Pink",
                             grepl("chum", name) ~ "Chum",
                             grepl("sockeye", name) ~ "Sockeye",
                             grepl("coho", name) ~ "Coho",
                             TRUE ~ "Chinook")) %>% 
  select(-name) 

d = escapement_mt %>% 
  select(year, species, location, mt_escape) %>% 
  clean_names() %>% 
  mutate(y = mt_escape) %>% 
  drop_na(y) %>% 
  group_by(species, location) %>% 
  mutate(time = year - min(year) + 1) %>% 
  ungroup() %>% 
  mutate(y_10000 = y/10000)

write_csv(d, file = "data/raw_data/salmon_metric_tons.csv")

d_short <- d %>% filter(year >= 1976) %>% 
  mutate(species_location = paste(species,"_",location))

saveRDS(d_short, file = "data/d_short.rds")

# Gamma regression model -------------------------------------------------

# Fit data prior to 1976. Use posteriors from this model as priors

d_long <- d %>% filter(year < 1976) %>% 
  mutate(species_location = paste(species,"_",location)) 

gam_salmon1 <- brm(y_10000 ~ s(year, by = species_location) + (1|species_location),
                   data = d_long, family = Gamma(link = "log"),
                   iter = 1000, chains = 1,
                   prior = c(prior(exponential(1), class = "sd"),
                             prior(normal(2, 10), class = "Intercept"),
                             prior(normal(0, 20), class = "b"),   # prior is posterior from gam_salmon1
                             prior(gamma(4,1), class = "shape")), # prior is posterior from gam_salmon1)
                   file = "models/dont_post/gam_salmon1.rds",
                   file_refit = "on_change",
                   cores = 4)



# plot(conditional_effects(gam_salmon1), points = T)

# Fit final model
gam_salmon2 <- brm(y_10000 ~ s(year, by = species_location) + (1|species_location),
                   data = d_short, family = Gamma(link = "log"),
                   iter = 2000, chains = 4,
                   prior = c(prior(exponential(1), class = "sd"),
                             prior(normal(2, 3), class = "Intercept"),
                             prior(normal(0, 5), class = "b"),   # prior is posterior from gam_salmon1
                             prior(gamma(4,1), class = "shape")), # prior is posterior from gam_salmon1
                   file = "models/dont_post/gam_salmon2.rds",
                   file_refit = "on_change",
                   cores = 4)

saveRDS(gam_salmon2, file = "models/gam_salmon2.rds")

# extract posteriors
gam_salmon_posts <- gam_salmon2$data %>% 
  data_grid(species_location, year) %>% 
  add_epred_draws(gam_salmon2, re_formula = NULL, ndraws = 1000) %>% 
  mutate(metric_tons = .epred*10000,
         kg = metric_tons*1000) %>% 
  separate(species_location, c("species", "location")) %>%  
  select(species, location, year, .draw, kg, metric_tons) %>% ungroup() %>% select(-.row)
  
saveRDS(gam_salmon_posts, file = "posteriors/gam_salmon_posts.rds")


# re-run without outlier of 1980
gam_salmon2 <- readRDS("models/gam_salmon2.rds")

d_short_removestrike = gam_salmon2$data %>% as_tibble() %>% 
  filter(species_location != "Sockeye _ BeringSea" | year != 1980)

gam_salmon2_removestrike = update(gam_salmon2, newdata = d_short_removestrike, iter = 1000, chains = 1)
saveRDS(gam_salmon2_removestrike, file = "models/gam_salmon2_removestrike.rds")

# Fit prior only
gam_salmon2_prior <- brm(y_10000 ~ s(year, by = species_location) + (1|species_location),
                   data = d_short, family = Gamma(link = "log"),
                   iter = 1000, chains = 1,
                   prior = c(prior(exponential(1), class = "sd"),
                             prior(normal(2, 3), class = "Intercept"),
                             prior(normal(0, 5), class = "b"),   # prior is posterior from gam_salmon1
                             prior(gamma(4,1), class = "shape")), # prior is posterior from gam_salmon1
                   file = "models/gam_salmon2_prior.rds",
                   sample_prior = "only",
                   cores = 4)
