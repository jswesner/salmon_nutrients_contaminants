library(tidyverse)
library(tidybayes)

# load posteriors
gam_salmon_posts <- readRDS("posteriors/gam_salmon_posts.rds")  # Salmon escapement in kg wet mass
all_chem_posts <- readRDS(file = "posteriors/all_chem_posts.rds") %>% ungroup()

# summarize
gam_salmon_posts %>% 
  group_by(species, location, year) %>% 
  summarize(mean = round(mean(metric_tons), 0),
            sd = round(sd(metric_tons), 0)) %>% 
  mutate(units = "Metric Tons") %>%
  glimpse() %>% 
  write_csv(., file = "tables/modeled_biomass_datarelease.csv")


all_chem_posts %>% 
  group_by(species, chemical, units, type) %>% 
  summarize(mean = round(mean(.epred), 0),
            sd = round(sd(.epred), 0)) %>% 
  glimpse() %>% 
  write_csv(., file = "tables/modeled_subsidies_datarelease.csv")


unique(gam_salmon_posts$location)
