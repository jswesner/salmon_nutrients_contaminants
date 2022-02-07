library(tidyverse)
library(janitor)



# escapement --------------------------------------------------------------

#load escapement

# millions
fish_escapement <- read_csv("data/raw_data/fish_escapement.csv") %>% clean_names() %>% 
  pivot_longer(cols = c(-year, -metric, -source)) %>% 
  mutate(species = case_when(grepl("pink", name) ~ "Pink",
                             grepl("chum", name) ~ "Chum",
                             grepl("sockeye", name) ~ "Sockeye",
                             grepl("chinook", name) ~ "Chinook",
                             TRUE ~ "Coho"),
         location = case_when(grepl("bering", name) ~ "Bering Sea",
                              grepl("central", name) ~ "Central Alaska",
                              grepl("seak", name) ~ "Southeast Alaska",
                              grepl("bc_wc", name) ~ "BCWC"))


# metric tons wet
salmon_mass <- readRDS("posteriors/derived_quantities/salmon_mass.rds")


# mean annual escapement (millions of fish)
mean_annual_escapement_millions <- fish_escapement %>% filter(year >= 1976) %>% # only after 1976
  select(year, value, species, location) %>%
  pivot_wider(names_from = location, values_from = value) %>% # add totals for region and species
  rowwise() %>% 
  mutate(region_total = sum(c_across(-c(1:2)))) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(-year, -species), names_to = "location") %>% 
  pivot_wider(names_from = species, values_from = value) %>% 
  rowwise() %>% 
  mutate(species_total = sum(c_across(-c(1:2)))) %>% 
  pivot_longer(cols = c(-year, -location), names_to = "species") %>% 
  group_by(location, species) %>% 
  summarize(mean = mean(value)) %>% 
  pivot_wider(names_from = species, values_from = mean) 

write_csv(mean_annual_escapement_millions, file = "tables/mean_annual_escape.csv")

# mean annual metric tons escapement
salmon_mass %>% 
  group_by(species, .draw, year) %>%
  summarize(total_mt = sum(metric_tons)) %>% 
  pivot_wider(names_from = species, values_from = total_mt) %>% 
  pivot_longer(cols = c(-.draw, -Total, -year)) %>% 
  mutate(proportion = value/Total) %>% 
  group_by(name, .draw) %>% 
  summarize(mean_mt = mean(value),
            mean_proportion = mean(proportion)) %>% 
  group_by(name) %>% 
  summarize(mean_mt = mean(mean_mt),
            mean_proportion = mean(mean_proportion))




# chemical flux -----------------------------------------------------------
# load posteriors
flux_predictions <- readRDS(file = "posteriors/flux_predictions.rds") # posterior chem export
gam_salmon_posts <- readRDS("posteriors/gam_salmon_posts.rds") # Salmon escapement in kg wet mass
all_chem_posts <- readRDS("posteriors/all_chem_posts.rds") %>% 
  mutate(chemical = case_when(chemical == "DDT" ~ "DDTs",
                              chemical == "PBDE" ~ "PBDEs",
                              TRUE ~ chemical)) %>% 
  mutate(type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "Nutrients",
                          TRUE ~ "Contaminants"),
         chemical = fct_relevel(chemical, "N", 
                                "P", 
                                "DHA", 
                                "EPA",
                                "Hg",
                                "DDTs")) # Salmon chemical concentrations mg kg wet mass

chem_species_prop <- readRDS("posteriors/chem_species_prop.rds")

# biomass_chem_medians
biomass_chem_medians <- all_chem_posts %>%
  pivot_wider(names_from = species, values_from = .epred) %>% 
  mutate(sumfish_meanchem = (Chinook + Chum + Coho + Pink + Sockeye)/5) %>% # mean chem concentrations across species
  pivot_longer(cols = c(-.draw, -chemical, -units, -type),
               names_to = "species", values_to = ".epred") %>% 
  group_by(species, chemical) %>% 
  summarize(median_chem_mgkg = median(.epred)) %>% 
  pivot_wider(names_from = chemical, values_from = median_chem_mgkg) %>% 
  right_join(gam_salmon_posts %>%
               dplyr::select(-kg) %>% 
               pivot_wider(names_from = species, values_from = metric_tons) %>% 
               mutate(sumfish_meanchem = (Chinook + Chum + Coho + Pink + Sockeye)) %>% # mean chem concentrations across species
               pivot_longer(cols = c(-.draw, -location, -year),
                            names_to = "species", values_to = "metric_tons") %>%
               group_by(species, year, .draw) %>% 
               summarize(total_mt = sum(metric_tons)) %>%  # total for species per year - summed across regions
               group_by(species) %>% 
               summarize(median_mt = median(total_mt)) %>% # median averaged across years
               dplyr::select(species, median_mt, everything())) %>% 
  dplyr::select(species, median_mt, N, P, DHA, EPA, Hg, everything()) %>% 
  arrange(median_mt)


write_csv(biomass_chem_medians, file = "tables/biomass_chem_medians.csv")




flux_with_all <- flux_predictions %>% 
  ungroup() %>% 
  group_by(species, year, .draw, chemical) %>% 
  summarize(total_kg = sum(mg_flux/1e6)) %>% 
  pivot_wider(names_from = species, values_from = total_kg) %>% 
  mutate(Total = Chinook + Chum + Coho + Pink + Sockeye) %>% 
  pivot_longer(cols = c(-year, -.draw, -chemical), names_to = "species", values_to = "total_kg") %>%
  mutate(type = case_when(chemical %in% c("N","P","DHA","EPA") ~ "Nutrients",
                          TRUE ~ "Contaminants"))

# annual average and sd flux 
mean_sd_annual_flux <- flux_with_all  %>% 
  group_by(species, chemical, type) %>% 
  mutate(value = case_when(type == "Contaminants" ~ total_kg,
                             TRUE ~ total_kg/1000)) %>% 
  group_by(species, chemical, type, .draw) %>%
  summarize(mean_year = mean(value)) %>%       # annual flux
  ungroup() %>% 
  group_by(species, chemical, type) %>% 
  summarize(mean = mean(mean_year),        # mean of mean per year
         sd = sd(mean_year)) %>%        # sd of mean per year
  mutate(mean = case_when(mean > 2 ~ round(mean, 0),
                          TRUE ~ round(mean, 2)),
         sd = case_when(mean > 2 ~ round(sd, 0),
                        TRUE ~ round(sd, 2))) %>% 
  mutate(mean_sd = paste0(mean," \u00B1 ", sd)) %>% 
  dplyr::select(-mean, -sd) %>%
  pivot_wider(names_from = species, values_from = mean_sd) %>% 
  arrange(desc(type), desc(Total)) %>% 
  mutate(units = case_when(type == "Nutrients" ~ "MT/y", 
                           TRUE ~ "Kg/y"),
         summary = "Average Annual")

cumulative_sd_annual_flux <- flux_with_all  %>% 
  group_by(species, chemical, type) %>% 
  mutate(value = case_when(type == "Contaminants" ~ total_kg,
                           TRUE ~ total_kg/1000)) %>% 
  group_by(species, chemical, type, .draw) %>%
  summarize(total_year = sum(value)) %>%       # cumulative flux
  ungroup() %>% 
  group_by(species, chemical, type) %>% 
  summarize(mean = median(total_year),        # mean of cumulative per year
            sd = sd(total_year)) %>%        # sd of cumulative per year
  mutate(mean = case_when(mean > 2 ~ round(mean, 0),
                          TRUE ~ round(mean, 2)),
         sd = case_when(mean > 2 ~ round(sd, 0),
                        TRUE ~ round(sd, 2))) %>% 
  mutate(mean_sd = paste0(mean," \u00B1 ", sd)) %>% 
  dplyr::select(-mean, -sd) %>%
  pivot_wider(names_from = species, values_from = mean_sd) %>% 
  arrange(desc(type), desc(Total)) %>% 
  mutate(units = case_when(type == "Nutrients" ~ "MT", 
                           TRUE ~ "Kg"),
         summary = "Cumulative")

mean_cumulative_flux <-
  bind_rows(mean_sd_annual_flux, cumulative_sd_annual_flux)

write_csv(mean_cumulative_flux, file = "tables/mean_cumulative_flux.csv")

# export over time
flux_predictions %>% 
  filter(.draw <= 500) %>% 
  ungroup() %>% 
  group_by(year, .draw, chemical) %>% 
  summarize(total_kg = sum(mg_flux/1e6)) %>% 
  group_by(year, chemical) %>% 
  summarize(mean_flux = mean(total_kg)) %>% 
  filter(year %in% c(1976, 2000, 2015)) %>% 
  pivot_wider(names_from = year, values_from = mean_flux) %>% 
  mutate(prop_increase  = (`2015` - `1976`)/`2015`)

  
flux_predictions %>% 
  filter(year %in% c(1976, 2000, 2015)) %>% 
  filter(.draw <= 500) %>% 
  ungroup() %>% 
  group_by(year, species, .draw, chemical) %>% 
  summarize(total_kg = sum(mg_flux/1e6)) %>% 
  group_by(year, chemical, species) %>% 
  summarize(mean_flux = mean(total_kg)) %>% 
  pivot_wider(names_from = species, values_from = mean_flux) %>% 
  mutate(total = Chinook + Chum + Coho + Pink + Sockeye,
         prop = Chinook/total) %>% 
  print(n = Inf)


# proportions
chem_species_prop <- flux_predictions %>% 
  # filter(.draw <= 2) %>% 
  select(species, location, year, .draw, mg_flux, chemical) %>% 
  pivot_wider(names_from = species, values_from = mg_flux) %>% 
  mutate(All = Chinook + Chum + Coho + Pink + Sockeye) %>% 
  pivot_longer(cols = c(Chinook, Chum, Coho, Pink, Sockeye), names_to = "species") %>% 
  mutate(proportion = value/All)


chem_species_prop %>% 
  # filter(chemical == "PCBs") %>% 
  # filter(species == "Pink") %>% 
  group_by(year, species, .draw, chemical) %>% 
  summarize(all = sum(All/1e+9),   #convert mg to metric tons
            species_mt = sum(value/1e+9),
            proportion = species_mt/all) %>% 
  group_by(.draw, chemical, species) %>% 
  summarize(mean_prop = mean(proportion),
            mean_mt = mean(species_mt)) %>% 
  group_by(chemical, species) %>% 
  summarize(mean_prop = mean(mean_prop),
            mean_mt = mean(mean_mt)) %>% 
  # pivot_wider(names_from = species, values_from = mean_prop) %>% 
  print(n = Inf)

# total biotransport
change_1976_2015 <- flux_predictions %>% 
  filter(year == 1976 | year ==2015) %>% 
  mutate(kg_flux = mg_flux/1e6) %>% 
  select(year, chemical, .draw, kg_flux, location) %>% 
  pivot_wider(names_from = chemical, values_from = kg_flux) %>% 
  mutate(All = DHA + DDT + Hg + PCBs + N + P + PBDE + EPA) %>% 
  pivot_longer(cols = c(-location, -species, -year, -.draw)) %>% 
  pivot_wider(values_from = value, names_from = location) %>% 
  mutate(All = BCWC + BeringSea + CentralAK + SEAK) %>% 
  pivot_longer(cols = c(-species, -year, -.draw, -name),
               names_to = "location", values_to = "kg_year") %>% 
  group_by(year, .draw, name, location) %>% 
  summarize(biotransport = sum(kg_year)) %>% 
  pivot_wider(names_from = year, values_from = biotransport) %>% 
  mutate(change = `2015` - `1976`,
         percent_change = `2015`/`1976`) %>% 
  group_by(name, location) %>% 
  summarize(kg_yr = median(change),
            percent_diff = median(percent_change)) %>% 
  pivot_longer(cols = c(kg_yr, percent_diff), names_to = "metric") %>% 
  unite(col = "loc_metric", location:metric, sep = "_") %>% 
  pivot_wider(names_from = loc_metric, values_from = value) %>% 
  arrange(-All_kg_yr)

write_csv(change_1976_2015, file = "tables/change_1976_2015.csv")  
  

         