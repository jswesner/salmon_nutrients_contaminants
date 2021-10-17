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

# load posteriors
flux_predictions <- readRDS(file = "posteriors/flux_predictions.rds") 


year_summaries <- flux_predictions %>% 
  group_by(type, species, location, chemical, year) %>% 
  summarize(median = median(mg_flux),
            lower = quantile(mg_flux, probs = 0.05),
            upper = quantile(mg_flux, probs = 0.95))

year_summaries %>% 
  filter(species == "All") %>% 
  ggplot(aes(x = year, y = median, ymin = lower, ymax = upper,
             fill = location)) +
  geom_ribbon() +
  facet_grid(chemical ~ type, scales = "free")


# Sensitivity via R2 Dietze 2017 p141 ------------------------------------------------------


sens_data <- flux_predictions %>%
  filter(type == "mg_flux" &
           species != "All") %>% 
  # filter(.draw <= 10) %>% 
  group_by(chemical, species) %>% 
  mutate(conc_c = (chem_mgkg - mean(chem_mgkg))/sd(chem_mgkg),
         kg_c = (kg - mean(kg))/sd(kg), 
         mg_flux_c = (mg_flux - mean(mg_flux))/sd(mg_flux),
         year_c = (year - mean(year))/sd(year)) 
  

nested_sens <- sens_data %>% 
  nest(data = c(-chemical)) %>% 
  mutate(mg_multivariate = map(data, ~lm(mg_flux_c ~ kg_c + conc_c, data = .x))) %>% 
  pivot_longer(cols = c(-chemical, -data)) %>% 
  mutate(tidied = map(value, tidy),
         glanced = map(value, glance),
         augmented = map(value, augment),
         rel_imp = map(value, calc.relimp))

detach("package:relaimpo", unload = TRUE)
detach("package:ggalt", unload = T)

saveRDS(nested_sens, file = "models/nested_sens.rds")

nested_sens$rel_imp


nested_sens %>% unnest(tidied) %>% 
  filter(term %in% c("kg_c", "conc_c")) %>%
  group_by(chemical) %>% 
  mutate(order = max(estimate)) %>% 
  ggplot(aes(x = reorder(chemical, order), y = estimate, fill = term, 
             color = term)) +
  geom_pointrange(aes(ymin=0, ymax=estimate, 
                      y = estimate), position=position_dodge(width = 0.2)) + 
  coord_flip() +
  NULL

#lmg values from the calc.relimp function. lmg is similar to partial r2's, but doesn't necessarily sum to 1

lmg_data_chem = distinct(nested_sens, chemical) %>% pull
lmg_data_conc = c(0.4, 0.18, 0.2, 0.32,
                 0.08, 0.05, 0.5, 0.35)
lmg_data_kg = c(0.37, 0.73, 0.81, 0.5,
               0.88, 0.92, 0.47, 0.45)

lmg <- tibble(chemical = lmg_data_chem,
              conc_c = lmg_data_conc,
              kg_c = lmg_data_kg) %>% 
  pivot_longer(cols = -chemical,
               names_to = "term", values_to = "estimate")


lmg %>% 
  group_by(chemical) %>% 
  mutate(order = max(estimate),
         term = case_when(term == "conc_c" ~ "Chemical Concentration (mg/kg ww)",
                          TRUE ~ "Salmon Escapement (kg ww)")) %>% 
  ggplot(aes(x = reorder(chemical, order), y = estimate, 
             color = term)) +
  geom_pointrange(aes(ymin=0, ymax=estimate, 
                      y = estimate), position=position_dodge(width = 0.2)) + 
  coord_flip() + 
  ylim(0,1) +
  labs(x = "Chemical",
       y = expression(paste("Explained Variance (", "R"^2,")")),
       color = "Coefficient") +
  scale_color_colorblind() +
  NULL
  

saveRDS(rel_importance, file = "plots/rel_importance.rds")
ggsave(rel_importance, file = "plots/rel_importance.jpg", width = 6, height = 3)
