library(tidyverse)

# Do modeled chemical concentrations approximate raw concentration --------

# 1) load modeled data
biotransport_potential_mg_per_individual = read_csv(file = "tables/ms_tables/tbl8_biotransport_potential_grams_per_individual.csv")
# 2) load raw data
nut_cont <- readRDS("data/nut_cont.rds")

# 3) summarize, bind, and plot
modeled_mgperfish = species_ind_average %>% 
  select(species, DHA, DDTs, Hg, PCBs, N, P, PBDEs, EPA, mean_size_kg) %>% 
  pivot_longer(cols = -c(mean_size_kg, species), names_to = "chemical", values_to = "mg_perkgww") %>% 
  group_by(species, chemical) %>% 
  # summarize(mg_perfish = mean(mg_perfish)) %>% 
  mutate(source = "modeled",
         mg_perfish = mg_perkgww*mean_size_kg)

raw_mgperfish = nut_cont %>%
  group_by(species, chemical, concentration_units_standardized) %>% 
  summarize(mean_mgkg = mean(mean_concentration_standardized)) %>%
  left_join(mean_fish_size) %>% 
  mutate(mg_perfish = mean_size_kg*mean_mgkg) %>% 
  filter(species != "All") %>% 
  select(species, chemical, mg_perfish) %>% 
  mutate(source = "raw mean")


modeled_mgperfish %>% 
  ggplot(aes(y = reorder(interaction(species, chemical), mg_perfish), 
           x = mg_perfish, color = source)) + 
  tidybayes::stat_halfeye() +
  geom_point(data = raw_mgperfish) + 
  scale_x_log10() +
  NULL

