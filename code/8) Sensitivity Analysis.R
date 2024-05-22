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
library(ggalt)
library(broom)
library(relaimpo)


# load data
fish_escapement <- read_csv("data/raw_data/fish_escapement.csv") %>% 
  pivot_longer(cols = -c(Year, metric, source), values_to = "millions_fish") %>% 
  clean_names() %>% 
  separate(name, c("location", "species"), sep = "_") %>% 
  dplyr::select(-metric, -source) %>% 
  separate(species, c("species", "delete")) %>% 
  dplyr::select(-delete) %>% 
  mutate(location = str_replace(location, " ", ""),
         location = str_replace(location, "/", ""))

fish_mass_kgww <- read_csv("data/raw_data/fish_mass_kgww_of_individual_fish.csv") %>%
  pivot_longer(cols = -c(Year, units, Source), values_to = "kg_ww_individual") %>% 
  clean_names() %>% 
  separate(name, c("location", "species"), sep = "_") %>% 
  dplyr::select(-units, -source) %>% 
  separate(species, c("species", "delete")) %>% 
  dplyr::select(-delete) %>% 
  mutate(location = str_replace(location, " ", ""),
         location = str_replace(location, "/", ""))

# load posteriors
flux_predictions <- readRDS(file = "posteriors/flux_predictions.rds") 


year_summaries <- flux_predictions %>% 
  group_by(species, location, chemical, year) %>% 
  summarize(median = median(mg_flux),
            lower = quantile(mg_flux, probs = 0.05),
            upper = quantile(mg_flux, probs = 0.95))

year_summaries %>% 
  # filter(species == "All") %>% 
  ggplot(aes(x = year, y = median, ymin = lower, ymax = upper,
             fill = location)) +
  geom_ribbon() +
  facet_wrap(~chemical, scales = "free")


# Sensitivity via R2 Dietze 2017 p141 ------------------------------------------------------

sens_data <- flux_predictions %>%
  filter(species != "All")  %>% 
  left_join(fish_escapement) %>% 
  left_join(fish_mass_kgww) %>% 
  filter(.draw <= 5) %>%
  group_by(chemical, species)%>% 
  mutate(conc_c = (mg_kg_chem - mean(mg_kg_chem))/sd(mg_kg_chem),
         kg_c = (kg - mean(kg))/sd(kg), 
         kg_ind_c = (kg_ww_individual - mean(kg_ww_individual))/sd(kg_ww_individual),
         millions_c = (millions_fish - mean(millions_fish))/sd(millions_fish),
         mg_flux_c = (mg_flux - mean(mg_flux))/sd(mg_flux),
         year_c = (year - mean(year))/sd(year)) 
  
# fit linear models with flux estimates as response and mass, escapement abundance, and chem concentration as
# predictors. Replicates for flux are monte carlo replicates.
nested_sens <- sens_data %>% 
  ungroup %>% 
  nest(data = c(-chemical)) %>% 
  mutate(mg_multivariate = purrr::map(data, ~lm(mg_flux_c ~ millions_c*year_c + kg_ind_c*year_c + 
                                           conc_c + location + species, data = .x))) %>% 
  pivot_longer(cols = c(-chemical, -data)) %>% 
  mutate(tidied = purrr::map(value, tidy),
         glanced = purrr::map(value, glance),
         augmented = purrr::map(value, augment),
         rel_imp = purrr::map(value, calc.relimp))

detach("package:relaimpo", unload = TRUE)
detach("package:ggalt", unload = T)

saveRDS(nested_sens, file = "models/nested_sens.rds")


# summarize
library(relaimpo)
nested_sens <- readRDS(file = "models/nested_sens.rds")

rel_imp = nested_sens$rel_imp


nested_sens %>% unnest(tidied) %>% 
  filter(term != "(Intercept)") %>% 
  group_by(chemical) %>% 
  mutate(order = max(estimate),
         estimate = abs(estimate)) %>% 
  ggplot(aes(x = reorder(chemical, order), y = estimate, fill = term, 
             color = term)) +
  geom_pointrange(aes(ymin=0, ymax=estimate, 
                      y = estimate), position=position_dodge(width = 0.4)) + 
  coord_flip() +
  NULL

#lmg values from the calc.relimp function. lmg is similar to partial r2's, but doesn't necessarily sum to 1

lmg_data_chem = distinct(nested_sens, chemical) %>% pull

lmg_list = NULL

for(i in 1:length(lmg_data_chem)){
  lmg_list[[i]] = tibble(estimate = rel_imp[[i]]@lmg) %>% 
    mutate(term = rel_imp[[i]]@lmg.rank %>% names(),
           chemical = lmg_data_chem[[i]])
}

lmg = bind_rows(lmg_list)

saveRDS(lmg, file = "data/lmg.rds")

lmg %>% 
  group_by(chemical) %>% 
  mutate(total_weight = sum(estimate),
         prop_weight = estimate/total_weight) %>% 
  filter(term %in% c("conc_c", "millions_c", "kg_ind_c")) %>% 
  dplyr::select(prop_weight, term) %>% 
  pivot_wider(names_from = term, values_from = prop_weight) %>% 
  mutate(escape_conc = sum(millions_c, conc_c))

