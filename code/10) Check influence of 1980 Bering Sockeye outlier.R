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
         model = "With 1980 outlier") %>% 
  separate(species_location, c("species", "location")) %>%  
  select(species, location, year, .draw, kg, metric_tons, model) %>% ungroup() %>% select(-.row)

gam_salmon_posts_removestrike <- gam_salmon2_removestrike$data %>% 
  data_grid(species_location, year) %>% 
  add_epred_draws(gam_salmon2_removestrike, re_formula = NULL, ndraws = 500) %>% 
  mutate(metric_tons = .epred*10000,
         kg = metric_tons*1000,
         model = "Without 1980 outlier") %>% 
  separate(species_location, c("species", "location")) %>%  
  select(species, location, year, .draw, kg, metric_tons, model) %>% ungroup() %>% select(-.row)

gam_both_posts = bind_rows(gam_salmon_posts,
                           gam_salmon_posts_removestrike)

# summarize posteriors

gam_salmon_posts_summary = gam_salmon_posts %>% 
  group_by(species, location, year, model) %>% 
  median_qi(metric_tons) 

gam_salmon_posts_removestrike_summary = gam_salmon_posts_removestrike %>% 
  group_by(species, location, year, model) %>% 
  median_qi(metric_tons) 

# wrangle raw data
data_for_posts = gam_salmon2$data %>% mutate(model = "With 1980 outlier") %>% 
  bind_rows(gam_salmon2_removestrike$data %>% mutate(model = "Without 1980 outlier")) %>% 
  separate(species_location, c("species", "location", sep = " _ "))

# plot
bind_rows(gam_salmon_posts_summary, 
          gam_salmon_posts_removestrike_summary) %>% 
  ggplot(aes(x = year, y = metric_tons, fill = species, group = species)) + 
  geom_line(alpha = 0.2) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, alpha = species)) +
  scale_alpha_manual(values = c(0.1, 0.1, 0.1, 0.1, 0.7)) +
  facet_grid(location ~ model) +
  geom_point(data = data_for_posts, aes(y = y_10000*10000, color = species, alpha = species)) +
  theme_default() + 
  labs(title = "Model comparison with and without 1980 outlier\nfor Bering Sea Sockeye")

# compare
gam_both_posts %>% 
  select(species, location, model, year, metric_tons, .draw) %>% 
  filter(year == 1976 | year == 2015) %>% 
  pivot_wider(names_from = year, values_from = metric_tons) %>% 
  mutate(diff = `2015` - `1976`,
         prop = diff/`1976`) %>% 
  filter(species == "Sockeye" & location == "BeringSea") %>% 
  group_by(species, location, model) %>% 
  median_qi(diff, prop)


gam_both_posts %>% 
  select(species, location, model, year, metric_tons, .draw) %>% 
  filter(year <= 1981 | year >= 2010) %>% 
  mutate(year_group = case_when(year <= 1981 ~ "start",
                                TRUE ~ "end")) %>% 
  group_by(species, location, model, year_group, .draw) %>% 
  summarize(metric_tons = mean(metric_tons)) %>% 
  pivot_wider(names_from = year_group, values_from = metric_tons) %>% 
  mutate(diff = end - start) %>% 
  filter(species == "Sockeye" & location == "BeringSea") %>% 
  group_by(species, location, model) %>% 
  median_qi(diff)


single_compare = gam_both_posts %>% 
  select(species, location, model, year, metric_tons, .draw) %>% 
  filter(year == 1976 | year == 2015) %>% 
  pivot_wider(names_from = year, values_from = metric_tons) %>% 
  mutate(diff = `2015` - `1976`) %>% 
  mutate(comparison = "Original: 1976 vs 2015")


five_year_compare = gam_both_posts %>% 
  select(species, location, model, year, metric_tons, .draw) %>% 
  filter(year <= 1981 | year >= 2010) %>% 
  mutate(year_group = case_when(year <= 1981 ~ "start",
                                TRUE ~ "end")) %>% 
  group_by(species, location, model, year_group, .draw) %>% 
  summarize(metric_tons = mean(metric_tons)) %>% 
  pivot_wider(names_from = year_group, values_from = metric_tons) %>% 
  mutate(diff = end - start) %>% 
  mutate(comparison = "Five year average: 1976-1981 vs 2010-2015")


bind_rows(single_compare, five_year_compare) %>% 
  filter(model == "With 1980 outlier") %>% 
  ggplot(aes(x = reorder(species, diff), y = diff, fill = comparison)) +
  geom_boxplot(position = "dodge", aes(group = interaction(comparison, location, species)),
               outlier.shape = NA, 
               width = 0.5) +
  facet_wrap(~location) +
  geom_hline(yintercept = 0) +
  ylim(-20000, 70000) +
  coord_flip() +
  labs(y = "Change in modeled Salmon Escapement from 1970's to 2015's",
       x = "Species",
       title = "Modeled estimates of change are the same regardless of method")


