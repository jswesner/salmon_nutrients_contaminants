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
source("code/functions.R")
theme_set(theme_default())


# 1) Load models -------------------------------------------------------------

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


# 3) Stack Posteriors -----------------------------------------------------
all_chem_posts <- dha_posts %>% bind_rows(ddt_posts,
                                     hg_posts,
                                     pcb_posts,
                                     nit_posts,
                                     phos_posts,
                                     pbde_posts,
                                     epa_posts) %>%
  ungroup() %>% 
  select(species, .draw, .epred, chemical) %>%
  mutate(units = "mg/kg ww") 


saveRDS(all_chem_posts, file = "data/derived_quantities/all_chem_posts.rds")








