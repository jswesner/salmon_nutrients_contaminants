library(tidyverse)

# Do modeled chemical concentrations approximate raw concentration? --------

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


# Check decline of individual body sizes? --------
kg_ind_fish = read_csv("data/raw_data/fish_mass_kgww_of_individual_fish.csv") %>% 
  clean_names() %>% 
  pivot_longer(cols = -c(year, units, source)) %>% 
  select(year, name, value) %>% 
  rename(kg_ind = value) %>% 
  mutate(location = case_when(grepl("bering", name) ~ "BeringSea",
                              grepl("central", name) ~ "CentralAK",
                              grepl("seak", name) ~ "SEAK",
                              TRUE ~ "BCWC")) %>% 
  mutate(species = case_when(grepl("pink", name) ~ "Pink",
                             grepl("chum", name) ~ "Chum",
                             grepl("sockeye", name) ~ "Sockeye",
                             grepl("coho", name) ~ "Coho",
                             TRUE ~ "Chinook")) %>% 
  filter(year >= 1976)

kg_ind_fish %>% 
  filter(year >= max(year)-0 | year <= min(year)+0) %>% 
  select(-name) %>% 
  # distinct(year) %>% 
  mutate(period = case_when(year >= 2010 ~ "late",
                            TRUE ~ "early")) %>% 
  group_by(period, species, location) %>% 
  reframe(kg_ind = mean(kg_ind)) %>% 
  pivot_wider(names_from = "period", values_from = kg_ind) %>%
  mutate(change = late - early,
         percent_change = change/early) %>% 
  group_by(species, location) %>% 
  reframe(mean_percent_change = mean(percent_change)) %>% 
  pivot_wider(names_from = location, 
              values_from = mean_percent_change)
  

kg_ind_fish %>% 
  group_by(species, year) %>% 
  reframe(mean = mean(kg_ind)) %>% 
  ggplot(aes(x = year, y = mean, color = species)) +
  geom_point()

# Do modeled chemical fluxes approximate raw fluxes? --------

d = read_csv(file = "data/raw_data/salmon_metric_tons.csv")

nut_cont <- readRDS("data/nut_cont.rds")
nut_cont_species = nut_cont %>% 
  group_by(chemical, species) %>% 
  reframe(mean_mgkg = median(mean_concentration_standardized))


mean_escape = d %>% 
  group_by(year, species) %>% 
  reframe(mt_escape = sum(mt_escape)) %>% 
  group_by(species) %>% 
  reframe(mean_escape_kg = median(mt_escape)*1000)


nut_cont_species %>% 
  left_join(mean_escape) %>% 
  mutate(flux_mg = mean_mgkg*mean_escape_kg,
         flux_kg = flux_mg/1e06) %>% 
  group_by(chemical) %>% 
  reframe(mean_flux_kg = mean(flux_kg),
          sd = sd(flux_kg))
