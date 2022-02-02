library(brms)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(cowplot)
library(scales)
library(janitor)
library(ggridges)
library(viridis)

# load regional concentrations



# load posteriors
gam_salmon_posts <- readRDS("posteriors/gam_salmon_posts.rds") # Salmon escapement in kg wet mass

all_chem_posts <- readRDS(file = "data/derived_quantities/all_chem_posts.rds") %>% 
  mutate(chem_mgkg = .epred) %>% 
  group_by(species, chemical) %>% 
  mutate(median_chem_mgkg = median(chem_mgkg))

# Simulate flux from posteriors
n_iter <- 500

# join salmon and chemical posts. Make long.
gam_posts_long <- gam_salmon_posts %>% 
  filter(.draw <= n_iter) %>% 
  ungroup() %>% 
  select(species, location, year, .draw, kg)  %>% 
  group_by(location, species, year) %>% 
  mutate(kg_median = median(kg)) %>% 
  left_join(all_chem_posts %>% select(chemical, species, .draw, chem_mgkg, median_chem_mgkg) %>% 
              filter(.draw <= n_iter)) %>% 
  mutate(mg_flux = kg*chem_mgkg,
         mg_flux_medianconc = median_chem_mgkg*kg,
         mg_flux_medianescape = chem_mgkg*kg_median) %>% 
  pivot_longer(cols = contains("mg_flux"), names_to = "type", values_to = "mg_flux")

# add estimate for total of all species
flux_predictions <- gam_posts_long %>%
  group_by(location, chemical, year, type, .draw) %>%                        
  summarise(mg_flux = sum(mg_flux)) %>%                  
  mutate(species = "All") %>% 
  bind_rows(gam_posts_long, .) 

saveRDS(flux_predictions, file = "posteriors/flux_predictions.rds")

