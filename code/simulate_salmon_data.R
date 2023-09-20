library(tidyverse)
library(janitor)

# bring in original data
fish_escapement_dont_post = read_csv("data/raw_data/fish_escapement_dont_post.csv") 
fish_mass_kgww_of_individual_fish_dont_post = read_csv("data/raw_data/fish_mass_kgww_of_individual_fish_dont_post.csv")

# randomize original data
fish_escapement = fish_escapement_dont_post %>% 
  pivot_longer(cols = c(-Year, -metric, -source)) %>%
  mutate(sd = value/4,
         value = abs(value + rnorm(nrow(.), 0, sd))) %>%   # add random noise to data
  select(-sd) %>%
  pivot_wider(names_from = name, values_from = value) %>% 
  mutate(source = "Simulated data. Contact Dr. Greg Ruggerone for raw data")

fish_mass_kgww_of_individual_fish = fish_mass_kgww_of_individual_fish_dont_post %>% 
  pivot_longer(cols = c(-Year, -units, -Source)) %>%
  mutate(sd = value/4,
         value = abs(value + rnorm(nrow(.), 0, sd))) %>%   # add random noise to data
  select(-sd) %>%
  pivot_wider(names_from = name, values_from = value) %>% 
  mutate(Source = "Simulated data. Contact Dr. Greg Ruggerone for raw data")



# save randomized file for models
write_csv(fish_escapement, file = "data/raw_data/fish_escapement.csv")
write_csv(fish_mass_kgww_of_individual_fish, file = "data/raw_data/fish_mass_kgww_of_individual_fish.csv")
