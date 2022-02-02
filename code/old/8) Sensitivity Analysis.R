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

theme_set(theme_default())

# load regional concentrations
region_concentrations <- readRDS(file = "data/derived_quantities/region_concentrations.rds") 

# add chum concentrations for fatty acids average of other species concentrations, since no fatty acids available for chum
chum_dha_epa <- region_concentrations %>% filter(chemical == "DHA" |chemical == "EPA") %>% 
  group_by(iter, chemical, units, location) %>% 
  summarize(region_conc = mean(region_conc)) %>% 
  mutate(species = "Chum")

# combine region and chum
region_concentrations_withchum <- bind_rows(region_concentrations, chum_dha_epa) %>% 
  group_by(chemical) %>% 
  mutate(region_conc_median = median(region_conc),
         region_conc_low = quantile(region_conc, probs = 0.125),
         region_conc_high = quantile(region_conc, probs = 1-0.125)) 

# load posteriors
gam_salmon_posts <- readRDS("posteriors/gam_salmon_posts.rds") %>% 
  mutate(kg_median = median(kg),
         kg_low = quantile(kg, probs = 0.125),
         kg_high = quantile(kg, probs = 1-0.125))

# calculate flux with different levels of uncertainty --------------------------------------------------------
n_iter = 300

flux_all_uncertainty <- gam_salmon_posts %>% filter(iter <= n_iter) %>% 
  right_join(region_concentrations_withchum %>%  filter(iter <= n_iter)) %>% 
  mutate(g_flux_all = region_conc*kg,
         g_flux_median_conc = region_conc_median*kg,
         g_flux_low75_conc = region_conc_low*kg,
         g_flux_high75_conc = region_conc_high*kg,
         g_flux_median_kg = region_conc*kg_median,
         g_flux_low75_kg = region_conc*kg_low,
         g_flux_high75_kg = region_conc*kg_high) %>% 
  select(species, location, year, iter, chemical, units, contains("g_flux")) %>% 
  pivot_longer(cols = contains("g_flux")) %>% 
  pivot_wider(names_from = species, values_from = value) %>% 
  mutate(All = Chinook + Chum + Coho + Pink + Sockeye) %>% 
  pivot_longer(cols = c(Chinook, Chum, Coho, Pink, Sockeye, All), names_to = "species", values_to = "g_flux") %>% 
  select(-units) 


cumulative_compare <- flux_all_uncertainty %>% 
  group_by(chemical, species, name, iter) %>% 
  summarize(cumulative_flux = sum(g_flux/1000)) %>% 
  ungroup() %>% 
  mutate(uncertainty_source = case_when(grepl("_kg", name) ~ "Chemical Concentrations",
                                        grepl("_conc", name) ~ "Escapement",
                                        TRUE ~ "Escapement + Concentrations")) %>% 
  separate(name, c("units", "measure", "quantile", "source"))

yearly_compare <- flux_all_uncertainty %>% 
  group_by(chemical, species, year, name, iter) %>% 
  summarize(cumulative_flux = sum(g_flux/1000)) %>% 
  ungroup() %>% 
  mutate(uncertainty_source = case_when(grepl("_kg", name) ~ "Chemical Concentrations",
                                        grepl("_conc", name) ~ "Escapement",
                                        TRUE ~ "Escapement + Concentrations")) %>% 
  separate(name, c("units", "measure", "quantile", "source"))

# plot
oat_variance_partitioning <- cumulative_compare %>% 
  filter(species == "All") %>%
  ggplot(aes(y = uncertainty_source, x = cumulative_flux, fill = quantile)) + 
  geom_density_ridges(scale = 1) +
  # geom_boxplot(outlier.shape = NA) +
  facet_wrap(~chemical, scales = "free_x", ncol = 2) +
  # coord_flip() +
  scale_fill_grey() +
  # scale_x_log10() +
  NULL

saveRDS(oat_variance_partitioning, file = "plots/oat_variance_partitioning.rds")
ggsave(oat_variance_partitioning, file = "plots/oat_variance_partitioning.jpg", width = 6, height = 8)


yearly_compare %>% 
  group_by(chemical, species, uncertainty_source, year) %>% 
  summarize(median = median(cumulative_flux),
            low75 = quantile(cumulative_flux, probs = 0.125),
            high75 = quantile(cumulative_flux, probs = 1-0.125)) %>% 
  ggplot(aes(x = year, y = median, fill = species)) + 
  geom_line() +
  geom_ribbon(aes(ymin = low75, ymax = high75), alpha = 0.5) +
  facet_grid(chemical ~ uncertainty_source, scales = "free") +
  scale_fill_viridis_d()


# Sensitivity via R2 Dietze 2017 p141 ------------------------------------------------------
sens_data <- gam_salmon_posts %>% filter(iter <= 100) %>% 
  group_by(location, species, year) %>% 
  right_join(region_concentrations_withchum %>%  filter(iter <= 100)) %>% 
  group_by(chemical) %>% 
  mutate(g_flux = region_conc*kg,
         conc_c = (region_conc - mean(region_conc))/sd(region_conc),
         kg_c = (kg - mean(kg))/sd(kg),
         g_flux_c = (g_flux - mean(g_flux))/sd(g_flux)) 

nested_sens <- sens_data %>% 
  nest(data = -chemical) %>% 
  mutate(kg_univariate = map(data, ~lm(g_flux_c ~ kg_c, data = .x)),
         conc_univariate = map(data, ~lm(g_flux_c ~ conc_c, data = .x))) %>% 
  pivot_longer(cols = c(-chemical, -data)) %>% 
  mutate(tidied = map(value, tidy),
         glanced = map(value, glance),
         augmented = map(value, augment))

nested_sens %>% unnest(glanced) %>% 
  select(chemical, name, r.squared) %>% 
  mutate(rsq_coded = case_when(name == "kg_univariate" ~ r.squared, 
                               TRUE ~ r.squared*-1)) %>% 
  # pivot_wider(names_from = name, values_from = r.squared) %>% 
  # mutate(diff = kg_univariate - conc_univariate) %>% 
  ggplot(aes(x = reorder(chemical, rsq_coded), y = rsq_coded, 
             color = name)) + 
  geom_lollipop() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = c("1.0", "0.5", "0.0", "0.5", '1.0'),
                     breaks = c(-1, -0.5, 0, 0.5, 1),
                     limits = c(-1, 1)) +
  scale_color_colorblind() +
  labs(x = "Chemical",
       y = "Variance explained (R-squared)",
       color = "") +
  coord_flip() 
  
rel_importance <- nested_sens %>% unnest(glanced) %>% 
  select(chemical, name, r.squared) %>% 
  mutate(rsq_coded = case_when(name == "kg_univariate" ~ r.squared, 
                               TRUE ~ r.squared*-1)) %>% 
  # pivot_wider(names_from = name, values_from = r.squared) %>% 
  # mutate(diff = kg_univariate - conc_univariate) %>% 
  ggplot(aes(x = reorder(chemical, rsq_coded), y = rsq_coded, 
             color = name)) + 
  geom_lollipop() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = c("1.0", "0.5", "0.0", "0.5", '1.0'),
                     breaks = c(-1, -0.5, 0, 0.5, 1),
                     limits = c(-1, 1)) +
  scale_color_colorblind() +
  labs(x = "Chemical",
       y = "Relative Importance (R-squared)",
       color = "") +
  guides(color = F) +
  coord_flip(clip = "off") +
  annotate("text", x = 8.5, y = -0.5, label = "Chemical Concentration") +
  annotate("text", x = 8.5, y = 0.5, label = "Salmon Escapement") +
  # coord_cartesian(clip = "off") +
  NULL

saveRDS(rel_importance, file = "plots/rel_importance.rds")
ggsave(rel_importance, file = "plots/rel_importance.jpg", width = 6, height = 3)
