library(tidyverse)
library(tidybayes)
library(brms)
library(bayesplot)
library(ggthemes)
library(janitor)
library(cowplot)
library(modelr)
source("code/functions.R")

rstan_options(auto_write = TRUE)

# Load data ----------------------------------------------------------------


d <- read_csv("data/salmon_metric_tons.csv") %>% 
  pivot_longer(cols = c(-units, -Year), names_to = "name", values_to = "y") %>% 
  separate(name, c("location", "species"), sep = "_", remove = F) %>% 
  separate(species, c("species", "family")) %>% 
  mutate(location = str_replace(location, " ", "")) %>% 
  clean_names() %>% 
  drop_na(y) %>% 
  group_by(name) %>% 
  mutate(time = year - min(year) + 1) %>% 
  ungroup() %>% 
  mutate(y_10000 = y/10000)



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
                   file = "models/gam_salmon1.rds",
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
                   file = "models/gam_salmon2.rds",
                   file_refit = "on_change",
                   cores = 4)

# extract posteriors
gam_salmon_posts <- gam_salmon2$data %>% 
  data_grid(species_location, year) %>% 
  add_epred_draws(gam_salmon2, re_formula = NULL, ndraws = 1000) %>% 
  mutate(metric_tons = .epred*10000,
         kg = metric_tons*1000) %>% 
  separate(species_location, c("species", "location")) %>%  
  select(species, location, year, .draw, kg, metric_tons) %>% ungroup() %>% select(-.row)
  
saveRDS(gam_salmon_posts, file = "posteriors/gam_salmon_posts.rds")

