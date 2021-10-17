library(brms)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(cowplot)
library(scales)
library(janitor)
library(ggridges)
library(viridis)

# load posteriors
gam_salmon_posts <- readRDS("posteriors/gam_salmon_posts.rds") # Salmon escapement in kg wet mass
all_chem_posts <- readRDS(file = "data/derived_quantities/all_chem_posts.rds") %>% ungroup()


# Simulate flux from posteriors
n_iter <- 1000

flux_predictions <- gam_salmon_posts %>% filter(.draw <= n_iter) %>% ungroup() %>% 
  select(species, location, year, .draw, kg) %>% 
  left_join(all_chem_posts %>% filter(.draw <= n_iter) %>% rename(mg_kg_chem = .epred) %>% select(-units)) %>% 
  mutate(mg_flux = kg*mg_kg_chem) %>% 
  group_by(location, species, chemical, year) %>% 
  mutate(kg_median = median(kg),
         chem_median = median(mg_kg_chem),
         chem_flux_mg_medianconc = chem_median*kg,
         chem_flux_mg_mediafish = mg_kg_chem*kg_median)


saveRDS(flux_predictions, file = "posteriors/flux_predictions.rds")

