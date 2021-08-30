library(brms)
library(ggridges)
source("code/functions.R")

# check models


# 1) bring in models
gam_salmon2 <- readRDS("models/gam_salmon2.rds")
hg_model <- readRDS("models/hg_model.rds") 
pcb_model <- readRDS("models/pcb_model.rds")
nit_model <- readRDS("models/nit_model.rds") 
phos_model <- readRDS("models/phos_model.rds") 
epa_model <- readRDS("models/epa_model.rds")
dha_model <- readRDS("models/dha_model.rds")
pbde_model <- readRDS("models/pbde_model.rds")
ddt_model <- readRDS("models/ddt_model.rds")

# 2) Posterior predictive checks
pp_check(gam_salmon2) + scale_x_log10()
pp_check(hg_model) + scale_x_log10()
pp_check(pcb_model) + scale_x_log10()
pp_check(nit_model) + scale_x_log10()
pp_check(phos_model) + scale_x_log10()
pp_check(dha_model) + scale_x_log10()
pp_check(epa_model) + scale_x_log10()

pp_check(gam_salmon2, type = "boxplot") + scale_y_log10()
pp_check(hg_model, type = "boxplot") + scale_y_log10()
pp_check(pcb_model, type = "boxplot") + scale_y_log10()
pp_check(nit_model, type = "boxplot") + scale_y_log10()
pp_check(phos_model, type = "boxplot") + scale_y_log10()


# 3) Test out any suspect models

nit_posts <- readRDS("posteriors/nit_posts.rds")
nit_posts %>% 
  filter(species == "All") %>% 
  ggplot(aes(x = species, y = g_kg_N)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(data = nit_model$data %>%  as_tibble() %>% mutate(species = "All"),
             aes(x = species, y = concentration))

phos_posts <- readRDS("posteriors/phos_posts.rds")
phos_posts %>% 
  filter(species == "All") %>% 
  ggplot(aes(x = species, y = g_kg_ww)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(data = phos_model$data %>%  as_tibble() %>% mutate(species = "All"),
              aes(x = species, y = concentration))
