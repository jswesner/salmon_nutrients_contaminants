library(tidyverse)
library(tidybayes)
library(brms)
library(rstan)
library(bayesplot)
library(ggthemes)
library(janitor)
library(cowplot)
library(modelr)

# load models
gam_salmon2 <- readRDS("models/gam_salmon2.rds") # original model
gam_salmon2_removestrike <- readRDS(file = "models/gam_salmon2_removestrike.rds") # model without 1980 Bering Chinook

# extract posteriors
gam_salmon_posts <- gam_salmon2$data %>% 
  data_grid(species_location, year) %>% 
  add_epred_draws(gam_salmon2, re_formula = NULL, ndraws = 1000) %>% 
  mutate(metric_tons = .epred*10000,
         kg = metric_tons*1000,
         model = "original") %>% 
  separate(species_location, c("species", "location")) %>%  
  select(species, location, year, .draw, kg, metric_tons, model) %>% ungroup() %>% select(-.row)

gam_salmon_posts_removestrike <- gam_salmon2_removestrike$data %>% 
  data_grid(species_location, year) %>% 
  add_epred_draws(gam_salmon2_removestrike, re_formula = NULL, ndraws = 500) %>% 
  mutate(metric_tons = .epred*10000,
         kg = metric_tons*1000,
         model = "removestrike") %>% 
  separate(species_location, c("species", "location")) %>%  
  select(species, location, year, .draw, kg, metric_tons, model) %>% ungroup() %>% select(-.row)

# summarize posteriors
gam_salmon_posts_summary = gam_salmon_posts %>% 
  group_by(species, location, year, model) %>% 
  median_qi(metric_tons) 

gam_salmon_posts_removestrike_summary = gam_salmon_posts_removestrike %>% 
  group_by(species, location, year, model) %>% 
  median_qi(metric_tons) 

# wrangle raw data
data_for_posts = gam_salmon2$data %>% mutate(model = "original") %>% 
  bind_rows(gam_salmon2_removestrike$data %>% mutate(model = "removestrike")) %>% 
  separate(species_location, c("species", "location", sep = " _ "))

# plot
bind_rows(gam_salmon_posts_summary, 
          gam_salmon_posts_removestrike_summary) %>% 
  ggplot(aes(x = year, y = metric_tons, fill = species, group = species)) + 
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.2) +
  facet_grid(location ~ model) +
  geom_point(data = data_for_posts, aes(y = y_10000*10000, color = species))

# compare
bind_rows(gam_salmon_posts_summary, 
          gam_salmon_posts_removestrike_summary) %>% 
  select(species, location, model, year, metric_tons) %>% 
  filter(year == 1976 | year == 2015) %>% 
  pivot_wider(names_from = year, values_from = metric_tons) %>% 
  mutate(diff = `2015` - `1976`) %>% 
  filter(species == "Sockeye" & location == "BeringSea")


bind_rows(gam_salmon_posts_summary, 
          gam_salmon_posts_removestrike_summary) %>% 
  select(species, location, model, year, metric_tons) %>% 
  filter(year <= 1981 | year >= 2010) %>% 
  mutate(year_group = case_when(year <= 1981 ~ "start",
                                TRUE ~ "end")) %>% 
  group_by(species, location, model, year_group) %>% 
  summarize(metric_tons = mean(metric_tons)) %>% 
  pivot_wider(names_from = year_group, values_from = metric_tons) %>% 
  mutate(diff = end - start) %>% 
  filter(species == "Sockeye" & location == "BeringSea")


