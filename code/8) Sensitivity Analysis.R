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


theme_set(theme_default())

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
  nest(data = c(-chemical)) %>% 
  mutate(mg_multivariate = map(data, ~lm(mg_flux_c ~ millions_c*year_c + kg_ind_c*year_c + 
                                           conc_c + location + species, data = .x))) %>% 
  pivot_longer(cols = c(-chemical, -data)) %>% 
  mutate(tidied = map(value, tidy),
         glanced = map(value, glance),
         augmented = map(value, augment),
         rel_imp = map(value, calc.relimp))

detach("package:relaimpo", unload = TRUE)
detach("package:ggalt", unload = T)

saveRDS(nested_sens, file = "models/nested_sens.rds")

nested_sens <- readRDS(file = "models/nested_sens.rds")

nested_sens$rel_imp


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

nested_sens$rel_imp


lmg_data_millions = c(0.3, 0.49, 0.54, 0.46,
                      0.65, 0.62, 0.14, 0.32)

lmg_data_kg_ind = c(0.001, 0.001, 0.002, 0.002,
                    0.003, 0.002, 0.0004, 0.001)

lmg_data_conc = c(0.36, 0.18, 0.13, 0.21,
                 0.02, 0.05, 0.53, 0.35)



lmg <- tibble(chemical = lmg_data_chem,
              conc_c = lmg_data_conc,
              kg_ind_c = lmg_data_kg_ind,
              millions_c = lmg_data_millions) %>% 
  pivot_longer(cols = -chemical,
               names_to = "term", values_to = "estimate")


rel_importance <- lmg %>% 
  group_by(chemical) %>% 
  mutate(order = max(estimate),
         term = case_when(term == "conc_c" ~ "Chemical Concentration",
                          term == "kg_ind_c" ~ "Mass",
                          TRUE ~ "Escapement")) %>% 
  mutate(term = fct_relevel(term, "Escapement",
                            "Chemical Concentration")) %>% 
  ggplot(aes(x = reorder(chemical, order), y = estimate, 
             color = term)) +
  geom_pointrange(aes(ymin=0, ymax=estimate, 
                      y = estimate,
                      shape = term), position=position_dodge(width = 0.4)) + 
  coord_flip() + 
  ylim(0,1) +
  labs(x = "Chemical",
       y = expression(paste("Explained Variance (", "R"^2,")")),
       color = "Coefficient",
       shape = "Coefficient") +
  scale_color_grey() + 
  guides(colour = guide_legend(reverse=T),
         shape = guide_legend(reverse = T)) +
  # theme(legend.position = "top") +
  NULL
  
saveRDS(rel_importance, file = "plots/ms_plots/rel_importance.rds")
ggsave(rel_importance, file = "plots/ms_plots/rel_importance.jpg", width = 7, height = 3)
