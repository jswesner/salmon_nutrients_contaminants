library(brms)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(cowplot)
library(scales)
library(janitor)
library(ggridges)
library(viridis)
library(egg)

# load regional concentrations
region_concentrations <- readRDS(file = "data/derived_quantities/region_concentrations.rds") 

# add chum concentrations for fatty acids average of other species concentrations, since no fatty acids available for chum
chum_dha_epa <- region_concentrations %>% filter(chemical == "DHA" |chemical == "EPA") %>% 
  group_by(iter, chemical, units, location) %>% 
  summarize(region_conc = mean(region_conc)) %>% 
  mutate(species = "Chum")

# combine region and chum
region_concentrations_withchum <- bind_rows(region_concentrations, chum_dha_epa)

# load posteriors
gam_salmon_posts <- readRDS("posteriors/gam_salmon_posts.rds") # Salmon escapement in kg wet mass
hg_posts <- readRDS("posteriors/hg_posts.rds") # Mercury concentrations in ug per kg wet mass
pcb_posts <- readRDS("posteriors/pcb_posts.rds") # PCB concentrations in ug per kg wet mass
nit_posts <- readRDS("posteriors/nit_posts.rds") # Nitrogen concentrations in g per kg wet mass
phos_posts <- readRDS("posteriors/phos_posts.rds") # Phosphorous concentrations in g per kg wet mass
epa_posts <- readRDS("posteriors/epa_posts.rds") # EPA concentrations in g per kg wet mass
dha_posts <- readRDS("posteriors/dha_posts.rds") # DHA concentrations in g per kg wet mass
pbde_posts <- readRDS("posteriors/pbde_posts.rds") # DHA concentrations in ug per kg wet mass
ddt_posts <- readRDS("posteriors/ddt_posts.rds") # DHA concentrations in ug per kg wet mass

# Simulate flux from posteriors
n_iter <- 1000

flux_predictions <- gam_salmon_posts %>% filter(iter <= n_iter) %>% 
  group_by(location, species, year) %>% 
  mutate(kg_median = median(kg)) %>% 
  right_join(region_concentrations_withchum %>%  filter(iter <= n_iter) %>% 
               group_by(location, species, chemical) %>% 
               mutate(region_conc_median = median(region_conc))) %>% 
  mutate(g_flux = region_conc*kg,
         g_flux_medianconc = region_conc_median*kg,
         g_flux_medianescape = region_conc*kg_median) %>% 
  select(species, location, year, iter, chemical, units, contains("g_flux")) %>% 
  pivot_longer(cols = contains("g_flux"), names_to = "type", values_to = "g_flux") %>% 
  pivot_wider(names_from = species, values_from = g_flux) %>% 
  mutate(All = Chinook + Chum + Coho + Pink + Sockeye) %>% 
  pivot_longer(cols = c(Chinook, Chum, Coho, Pink, Sockeye, All), names_to = "species", values_to = "g_flux") %>% 
  select(-units) %>% 
  pivot_wider(names_from = type, values_from = g_flux)

saveRDS(flux_predictions, file = "posteriors/flux_predictions.rds")

