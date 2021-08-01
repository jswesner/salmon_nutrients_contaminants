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

theme_set(theme_default())

get_posts <- function(model){
  posterior_samples(model) %>% 
    mutate(iter = 1:nrow(.)) %>% 
    clean_names() %>% 
    as_tibble() %>% 
    mutate(chems_units = model$chems_units)
}

# region data to match salmon biomass with chemical concentrations
regions <- read_csv("data/derived_quantities/regions_match.csv") %>% 
  pivot_longer(cols = -region) %>% filter(!is.na(value)) %>% rename(location = value) %>% 
  select(-name) %>% 
  filter(!is.na(location)) 

# Chunks 1-3 below create this. Load directly or recreate
region_concentrations <- readRDS(file = "data/derived_quantities/region_concentrations.rds")

# 1) Load models -------------------------------------------------------------

gam_salmon2 <- readRDS("models/gam_salmon2.rds")
hg_model <- readRDS("models/hg_model.rds") 
pcb_model <- readRDS("models/pcb_model.rds")
nit_model <- readRDS("models/nit_model.rds") 
phos_model <- readRDS("models/phos_model.rds") 
epa_model <- readRDS("models/epa_model.rds")
dha_model <- readRDS("models/dha_model.rds")
pbde_model <- readRDS("models/pbde_model.rds")
ddt_model <- readRDS("models/ddt_model.rds")

# 2) Load Posteriors -----------------------------------------------------

gam_salmon_posts <- readRDS("posteriors/gam_salmon_posts.rds") # Salmon escapement in kg wet mass
hg_posts <- readRDS("posteriors/hg_posts.rds") # Mercury concentrations in ug per kg wet mass
pcb_posts <- readRDS("posteriors/pcb_posts.rds") # PCB concentrations in ug per kg wet mass
nit_posts <- readRDS("posteriors/nit_posts.rds") # Nitrogen concentrations in g per kg wet mass
phos_posts <- readRDS("posteriors/phos_posts.rds") # Phosphorous concentrations in g per kg wet mass
epa_posts <- readRDS("posteriors/epa_posts.rds") # EPA concentrations in g per kg wet mass
dha_posts <- readRDS("posteriors/dha_posts.rds") # DHA concentrations in g per kg wet mass
pbde_posts <- readRDS("posteriors/pbde_posts.rds") # DHA concentrations in ug per kg wet mass
ddt_posts <- readRDS("posteriors/ddt_posts.rds") # DHA concentrations in ug per kg wet mass


all_posts <- dha_posts %>% bind_rows(ddt_posts,
                                     hg_posts,
                                     pcb_posts,
                                     nit_posts,
                                     phos_posts,
                                     pbde_posts,
                                     epa_posts) %>%
  pivot_longer(cols = c(g_kg_ww, ug_kg_ww, g_kg_N), names_to = "units") %>% 
  filter(!is.na(value)) %>% 
  mutate(units = case_when(units == "g_kg_N" ~ "g_kg_ww", 
                          TRUE ~ units)) %>% 
  select(-b_Intercept, -shape, -lp__)




# 3) Stack Posteriors --------------------------------------------------------

# exclude nitrogen and phosphorous. add later. they don't have region or species groups
mod_list <- list(hg_model, 
                 pcb_model,
                 epa_model,
                 dha_model,
                 pbde_model,
                 ddt_model)


stacked_posts <- lapply(mod_list, get_posts) %>% bind_rows() %>% 
  separate(chems_units, c("chemical", "units"), extra = "merge")

species_mods <- stacked_posts %>% select(contains("b_species")) %>% 
  pivot_longer(cols = everything()) %>% distinct(species = name)

nit_posts_raw <- get_posts(nit_model) %>% select(b_intercept, iter) %>% 
  mutate(chemical = "N", value = exp(b_intercept), units = "g_kg") %>% 
  select(-b_intercept) %>% expand_grid(region = unique(gam_salmon_posts$location)) %>% 
  expand_grid(species = species_mods$species)

phos_posts_raw <- get_posts(phos_model) %>% select(b_intercept, iter) %>% 
  mutate(chemical = "P", value = exp(b_intercept),units = "g_kg") %>% 
  select(-b_intercept) %>% expand_grid(region = unique(gam_salmon_posts$location)) %>% 
  expand_grid(species = species_mods$species)

stacked_posts_region <- stacked_posts %>%
  select(contains(c("b_species", "r_region", "iter", "chemical", "units")))  %>% 
  pivot_longer(cols = contains("region"), names_to = "region", values_to = "offset") %>% 
  filter(!is.na(offset)) %>% 
  pivot_longer(cols = contains("species"), names_to = "species", values_to = "species_value") %>%
  filter(!is.na(offset)) %>%
  filter(!is.na(species_value)) %>% 
  mutate(value = exp(species_value + offset)) %>% 
  separate(region, c(NA, NA, "region", "region2", NA), extra = "merge") %>% 
  mutate(region2 = case_when(region2 != "intercept" ~ region2),
         region = paste0(region, "_", region2)) %>% 
  select(-region2) %>% 
    bind_rows(nit_posts_raw, phos_posts_raw)

saveRDS(stacked_posts_region, file = "data/derived_quantities/stacked_posts_region.rds")

stacked_posts_region <- readRDS("data/derived_quantities/stacked_posts_region.rds")

region_concentrations <- stacked_posts_region %>% 
  mutate(value = case_when(units == "ug_kg" ~ value/1e+06,
                           TRUE ~ value)) %>% 
  mutate(units = "g_kg") %>% 
  left_join(regions)  %>% 
  filter(!is.na(location)) %>% 
  mutate(species = str_sub(species, 11, 20)) %>% 
  group_by(iter, chemical, units, location, species) %>% 
  summarize(region_conc = mean(value)) %>% 
  mutate(species = str_to_sentence(species)) %>% 
  ungroup()

saveRDS(region_concentrations, file = "data/derived_quantities/region_concentrations.rds")
