flux_predictions <- readRDS(file = "posteriors/flux_predictions.rds") # posterior chem export
gam_salmon_posts <- readRDS("posteriors/gam_salmon_posts.rds") # Salmon escapement in kg wet mass


salmon_prop_medians <- gam_salmon_posts %>% 
  filter(species != "All") %>% 
  group_by(iter, year, location) %>% 
  mutate(total_value = sum(value),
         proportion = value/total_value) %>% 
  group_by(species, year, location) %>% 
  summarize(median_escape_prop = median(proportion))

chem_prop_medians <- flux_predictions %>% 
  filter(species != "All") %>% 
  group_by(iter, year, location, chemical) %>% 
  mutate(total_value = sum(g_flux),
         proportion = g_flux/total_value) %>% 
  group_by(species, year, location, chemical) %>% 
  summarize(median_flux_prop = median(proportion))

prop_compare <- gam_salmon_posts %>% 
  filter(species != "All") %>% 
  group_by(iter, year, location) %>% 
  mutate(total_value = sum(value),
         proportion_salmon = value/total_value) %>% 
  select(-total_value) %>% 
  left_join(flux_predictions %>% 
              filter(species != "All") %>% 
              group_by(iter, year, location, chemical) %>% 
              mutate(total_value = sum(g_flux),
                     proportion_flux = g_flux/total_value) %>% 
              select(-total_value, -g_flux)) %>% 
  mutate(diff = proportion_salmon - proportion_flux,
         dotcolor = case_when(diff < 0 ~ "More flux",
                              TRUE ~ "More biomass"))


prop_compare_average <- prop_compare %>% 
  group_by(iter, species, chemical, year) %>% 
  summarize(diff = mean(diff),
            prop_flux = mean(proportion_flux),
            prop_salmon = mean(proportion_salmon)) %>% 
  mutate(dotcolor = case_when(diff < 0 ~ "More flux",
                              TRUE ~ "More biomass")) %>% 
  mutate(prop_flux_c = prop_flux - mean(prop_flux),
         prop_salmon_c = prop_salmon - mean(prop_salmon))

prop_compare_average %>% filter(iter <= 300) %>% 
  # filter(chemical == "PCBS") %>% 
  ggplot(aes(x = prop_salmon_c, y = prop_flux_c, group = species, color = species)) + 
  # geom_point(alpha = 0.1, size = 0.1) +
  # geom_line(aes(group = iter), alpha = 0.6) +
  geom_line(aes(group = iter), stat="smooth",method = "lm",
            size = 0.1,
            # linetype ="dashed",
            alpha = 0.3) +
  facet_grid(chemical ~ species) +
  scale_color_viridis_d() +
  geom_abline(intercept = 0, slope = 1) +
  theme_classic()




all_prop_medians <- salmon_prop_medians %>% left_join(chem_prop_medians) %>% 
  mutate(diff = median_flux_prop - median_escape_prop)

all_prop_medians %>% 
  mutate(dotcolor = case_when(diff < 0 ~ "Higher",
                              TRUE ~ "Lower")) %>% 
  ggplot(aes(x = year, y = diff, fill = chemical, alpha = dotcolor)) + 
  geom_point() + 
  facet_grid(. ~ species)



all_prop_medians %>% 
  ggplot(aes(x = median_escape_prop, y = median_flux_prop, color = chemical)) + 
  geom_point() + 
  facet_wrap(location ~ species)
