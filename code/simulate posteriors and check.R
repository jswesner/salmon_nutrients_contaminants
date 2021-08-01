library(tidyverse)
library(ggridges)

# simulate to confirm that posteriors

hg_posts <- readRDS("posteriors/hg_posts.rds") # Mercury concentrations in ug per kg wet mass

test <- posterior_samples(hg_model) %>% as_tibble() %>% mutate(iter = 1:nrow(.)) %>% 
  select(contains(c("b_species", "iter"))) %>% 
  pivot_longer(cols = -iter) %>% 
  group_by(name) %>% 
  summarize(mean = mean(value),
            sd = sd(value)) %>% 
  rowwise() %>% 
  mutate(sims = list(rnorm(2000, mean, sd)),
         iter = list(1:2000)) %>% 
  unnest(cols = c(sims, iter)) %>% 
  mutate(value = exp(sims)) %>% 
  select(iter, name, value) %>% 
  mutate(model = "posterior simulation")

test_post <- posterior_samples(hg_model) %>% as_tibble() %>% mutate(iter = 1:nrow(.)) %>% 
  select(contains(c("b_species", "iter"))) %>% 
  pivot_longer(cols = -iter) %>% 
  mutate(model = "posterior sample",
         value = exp(value))

test_post %>% 
  bind_rows(test) %>% 
  ggplot(aes(x = value, color = model, y = name)) + 
  geom_density_ridges(fill = NA)
