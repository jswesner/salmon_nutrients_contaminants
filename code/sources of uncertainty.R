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

# calculate flux with different levels of uncertainty
n_iter = 300
flux_all_uncertainty <- gam_salmon_posts %>% filter(iter <= n_iter) %>% 
  group_by(location, species, year) %>% 
  right_join(region_concentrations_withchum %>%  filter(iter <= n_iter)) %>% 
  mutate(g_flux = region_conc*kg) %>% 
  select(species, location, year, iter, chemical, units, contains("g_flux")) %>% 
  pivot_wider(names_from = species, values_from = g_flux) %>% 
  mutate(All = Chinook + Chum + Coho + Pink + Sockeye) %>% 
  pivot_longer(cols = c(Chinook, Chum, Coho, Pink, Sockeye, All), names_to = "species", values_to = "g_flux") %>% 
  select(-units) %>% 
  mutate(uncertainty_source = "Escapement + Concentrations")

flux_uncertainty_is_chem <- gam_salmon_posts %>% filter(iter <= n_iter) %>% 
  group_by(location, species, year) %>% 
  summarize(kg_median = median(kg)) %>% 
  right_join(region_concentrations_withchum %>%  filter(iter <= n_iter)) %>% 
  mutate(g_flux = region_conc*kg_median) %>% 
  select(species, location, year, iter, chemical, units, contains("g_flux")) %>% 
  pivot_wider(names_from = species, values_from = g_flux) %>% 
  mutate(All = Chinook + Chum + Coho + Pink + Sockeye) %>% 
  pivot_longer(cols = c(Chinook, Chum, Coho, Pink, Sockeye, All), names_to = "species", values_to = "g_flux") %>% 
  select(-units) %>% 
  mutate(uncertainty_source = "Chemical Concentrations")

flux_uncertainty_is_escape <- gam_salmon_posts %>% filter(iter <= n_iter) %>% 
  group_by(location, species, year) %>% 
  left_join(region_concentrations_withchum %>%  filter(iter <= n_iter) %>% 
              ungroup() %>% 
              group_by(species, location, chemical, units) %>% 
              summarize(region_conc_median = median(region_conc))) %>% 
  mutate(g_flux = region_conc_median*kg) %>% 
  select(species, location, year, iter, chemical, units, contains("g_flux")) %>% 
  pivot_wider(names_from = species, values_from = g_flux) %>% 
  mutate(All = Chinook + Chum + Coho + Pink + Sockeye) %>% 
  pivot_longer(cols = c(Chinook, Chum, Coho, Pink, Sockeye, All), names_to = "species", values_to = "g_flux") %>% 
  select(-units) %>%
  mutate(uncertainty_source = "Salmon Escapement")

cumulative_compare <- bind_rows(flux_all_uncertainty, flux_uncertainty_is_chem, flux_uncertainty_is_escape) %>% 
  # pivot_longer(cols = c(-location, -year, -iter, -chemical, -species)) %>% 
  group_by(chemical, species, uncertainty_source, iter) %>% 
  summarize(cumulative_flux = sum(g_flux/1000)) %>% 
  ungroup() %>% 
  mutate(uncertainty_source = fct_relevel(uncertainty_source, "Escapement + Concentrations", "Chemical Concentrations")) %>% 
  group_by(chemical, species, uncertainty_source) 

yearly_compare <- bind_rows(flux_all_uncertainty, flux_uncertainty_is_chem, flux_uncertainty_is_escape) %>% 
  # pivot_longer(cols = c(-location, -year, -iter, -chemical, -species)) %>% 
  group_by(chemical, species, year, uncertainty_source, iter) %>% 
  summarize(cumulative_flux = sum(g_flux/1000)) %>% 
  ungroup() %>% 
  mutate(uncertainty_source = fct_relevel(uncertainty_source, "Escapement + Concentrations", "Chemical Concentrations")) %>% 
  group_by(chemical, species, uncertainty_source) 

# plot
cumulative_compare %>% 
  # group_by(chemical, species, name) %>% 
  # summarize(cumulative_flux_s = (cumulative_flux - mean(cumulative_flux))/sd(cumulative_flux)) %>% 
  filter(species == "All") %>%
  ggplot(aes(x = uncertainty_source, y = cumulative_flux)) + 
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~chemical, scales = "free_x", ncol = 2) +
  coord_flip() +
  scale_y_log10()


cumulative_compare %>% 
  group_by(chemical, uncertainty_source, species) %>%
  mutate(cumulative_flux = cumulative_flux, 
         mean = mean(cumulative_flux),
         sd = sd(cumulative_flux)) %>% 
  ungroup() %>% 
  mutate(cumulative_flux_s = (cumulative_flux - mean)/sd,
         cumulative_flux_c = cumulative_flux - mean) %>% 
  filter(species == "All") %>%
  ggplot(aes(x = uncertainty_source, y = cumulative_flux_s)) + 
  # geom_boxplot(outlier.shape = NA) +
  geom_violin() +
  facet_wrap(~chemical, scales = "free_x", ncol = 2) +
  facet_wrap(~chemical, scales = "free_x", ncol = 2) +
  coord_flip()

  ggplot(aes(x = chemical, y = cumulative_flux_s, fill = uncertainty_source)) + 
  geom_boxplot()
  geom_boxplot(outlier.shape = NA) 







yearly_compare %>% 
  group_by(chemical, species, uncertainty_source, year) %>% 
  summarize(median = median(cumulative_flux),
            low75 = quantile(cumulative_flux, probs = 0.125),
            high75 = quantile(cumulative_flux, probs = 1-0.125)) %>% 
  ggplot(aes(x = year, y = median, fill = species)) + 
  geom_line() +
  geom_ribbon(aes(ymin = low75, ymax = high75), alpha = 0.5) +
  facet_grid(chemical ~ uncertainty_source, scales = "free") +
  scale_color_colorblind()


