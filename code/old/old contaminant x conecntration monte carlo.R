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


region_conc_a <- readRDS(file = "data/derived_quantities/region_concentrations.rds")

chum_dha_epa <- region_conc_a %>% filter(chemical == "DHA" |chemical == "EPA") %>% 
  group_by(iter, chemical, units, location) %>% 
  summarize(mean = mean(mean)) %>% 
  mutate(species = "Chum")

region_concentrations_withchum <- bind_rows(region_conc_a, chum_dha_epa)


gam_salmon_posts <- readRDS("posteriors/gam_salmon_posts.rds") # Salmon escapement in kg wet mass
hg_posts <- readRDS("posteriors/hg_posts.rds") # Mercury concentrations in ug per kg wet mass
pcb_posts <- readRDS("posteriors/pcb_posts.rds") # PCB concentrations in ug per kg wet mass
nit_posts <- readRDS("posteriors/nit_posts.rds") # Nitrogen concentrations in g per kg wet mass
phos_posts <- readRDS("posteriors/phos_posts.rds") # Phosphorous concentrations in g per kg wet mass
epa_posts <- readRDS("posteriors/epa_posts.rds") # EPA concentrations in g per kg wet mass
dha_posts <- readRDS("posteriors/dha_posts.rds") # DHA concentrations in g per kg wet mass
pbde_posts <- readRDS("posteriors/pbde_posts.rds") # DHA concentrations in ug per kg wet mass
ddt_posts <- readRDS("posteriors/ddt_posts.rds") # DHA concentrations in ug per kg wet mass

# Make data
n_iter <- 300

prior_pred <- gam_salmon_posts %>% filter(iter <= n_iter) %>% 
  right_join(region_concentrations_withchum %>%  filter(iter <= n_iter)) %>% 
  mutate(g_flux = region_conc*kg)

prior_pred %>% 
  mutate(ddt_flux = DDT*KG)

prior_pred_sum <- prior_pred %>%
  group_by(year, species, chemical, units, iter) %>% 
  summarize(mean = sum(g_flux/1000)) %>% 
  group_by(year, chemical, species) %>% 
  summarize(median = median(mean),
            low = quantile(mean, probs = 0.125),
            upper = quantile(mean, probs = 1-0.125),
            low50 = quantile(mean, probs = 0.25),
            upper50 = quantile(mean, probs = 1-0.25)) %>% 
  ungroup()

prior_pred_sum %>% 
  ggplot(aes(x = year, fill  = species, group = species)) +
  geom_line(aes(y = median)) +
  geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.2) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.8) +
  facet_wrap(~chemical, scales = "free_y")









chem_mean_sd <- region_concentrations_withchum %>% group_by(species, location, chemical, units) %>% 
  summarize(y_chem = mean(region_conc*1000*1000),
            sd_chem = sd(region_conc*1000*1000)) 

salmon_mean_sd <- gam_salmon_posts %>% group_by(location, year, species) %>% 
  summarize(y_salmon = mean(kg/1000/1000),
            sd_salmon = sd(kg/1000/1000)) %>% 
  arrange(species, location, year)

salmon_chem <- salmon_mean_sd %>% 
  left_join(chem_mean_sd) %>% 
  mutate(y_fake = 1) %>% 
  arrange(chemical, species, location, year) %>% 
  group_by(chemical) %>% 
  mutate(name = paste0("V", row_number())) %>% #for indexing later
  ungroup() 

  
# set priors
prior_list <- get_prior(y_fake ~ 0 + me(y_salmon, sd_salmon):species:location, data = salmon_chem,
                        family = Gamma(link = "log"))

# function to make a brms prior list
brm_priors <- function(chema){
  
get_prior(y_fake ~ 0 + me(y_salmon, sd_salmon):species:location,
                         family = Gamma(link = "log"), data = salmon_chem) %>%            # list of priors for all models
    as_tibble()  %>% 
    # expand_grid(chemical = unique(chem_mean_sd$chemical)) %>% 
    separate(coef, c("a", "b", "c"), sep = ":") %>% 
    # mutate(prior = prior_list$prior) %>%
    left_join(salmon_chem  %>% distinct(species, chemical, location, y_chem, sd_chem) %>%    # merge with priors from models
                mutate(prior_add = paste0("normal(", round(y_chem, 7), ",", round(sd_chem, 7), ")"),
                       b = paste0("species", species),
                       c = paste0("location", location)) %>% 
                select(prior_add, b, c, chemical)) %>%
    mutate(prior = case_when(class == "shape" ~ "exponential(2)", 
                             is.na(prior_add) ~ "normal(0, 1)", 
                             TRUE ~ prior_add)) %>% 
    select(-prior_add) %>% 
    mutate(coef = paste0(a, ":", b, ":", c),
           coef = str_replace(coef, ":NA:NA", "")) %>% 
    filter(chemical == chema)
  
  set_prior(prior = brm_priors$prior,
            class = brm_priors$class,
            coef = brm_priors$coef,
            group = brm_priors$group,
            resp = brm_priors$resp,
            dpar = brm_priors$dpar,
            nlpar = brm_priors$nlpar,
            lb = NA, 
            ub = NA,
            check = T)
}

brm_priors(chema = "Hg")
# Fit Models --------------------------------------------------------------

salmon_ddt <- readRDS(file = "models/salmon_ddt.rds") # first model to fit. Used as template for other models in fit_brms()

fit_brms <- function(chem){
  update(salmon_ddt <- readRDS(file = "models/salmon_ddt.rds"),
         newdata = salmon_chem %>% filter(chemical == chem),
         prior = brm_lognorm_priors(chema = chem), chains = 1, family = gaussian(),
         file = paste0("models/salmon_",chem))
}



# Fit all models
salmon_ddt <- fit_brms(chem = "DDT")
salmon_hg <- fit_brms(chem = "hg")
salmon_dha <- fit_brms(chem = "DHA")
salmon_epa <- fit_brms(chem = "EPA")
salmon_nit <- fit_brms(chem = "N")
salmon_pho <- fit_brms(chem = "P")
salmon_pbd <- fit_brms(chem = "PBDE")
salmon_pcb <- fit_brms(chem = "PCBS")

saveRDS(salmon_hg, file = "models/salmon_hg.rds")
saveRDS(salmon_ddt, file = "models/salmon_ddt.rds")
saveRDS(salmon_dha, file = "models/salmon_dha.rds")
saveRDS(salmon_epa, file = "models/salmon_epa.rds")
saveRDS(salmon_nit, file = "models/salmon_nit.rds")
saveRDS(salmon_pho, file = "models/salmon_pho.rds")
saveRDS(salmon_pbd, file = "models/salmon_pbd.rds")
saveRDS(salmon_pcb, file = "models/salmon_pcb.rds")

salmon_hg <-  readRDS(file = "models/salmon_hg.rds")
salmon_ddt <- readRDS(file = "models/salmon_ddt.rds") 
salmon_dha <- readRDS(file = "models/salmon_dha.rds")
salmon_epa <- readRDS(file = "models/salmon_epa.rds") 
salmon_nit <- readRDS(file = "models/salmon_nit.rds") 
salmon_pho <- readRDS(file = "models/salmon_pho.rds") 
salmon_pbd <- readRDS(file = "models/salmon_pbd.rds") 
salmon_pcb <- readRDS(file = "models/salmon_pcb.rds") 


# Sample Posteriors -------------------------------------------------------

post_summaries_combined <- readRDS(file = "posteriors/post_summaries_combined.rds")

model_list <- list(salmon_hg, salmon_ddt, salmon_dha, salmon_epa,
                   salmon_nit, salmon_pho, salmon_pbd, salmon_pcb)

model_names = c("Hg", "DDT", "DHA", "EPA", "N", "P", "PBDE", "PCBS")

post_summaries_combined <- lapply(
  lapply(model_list, function(x){
  fitted(x, summary = F) %>% 
           as_tibble() %>% 
           mutate(iter = 1:nrow(.)) %>%
           pivot_longer(cols = -iter) 
}) %>% 
  mapply(cbind, ., "chemical" = model_names, SIMPLIFY = F),
function(x){left_join(x, salmon_chem)})

saveRDS(post_summaries_combined, file = "posteriors/post_summaries_combined.rds")

