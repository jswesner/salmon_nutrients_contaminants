library(janitor)
library(tidybayes)
library(tidyverse)

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
         location = case_when(grepl("bering", name) ~ "BeringSea",
                              grepl("central", name) ~ "CentralAK",
                              grepl("seak", name) ~ "SEAK",
                              grepl("bc_wc", name) ~ "BCWC"))


# metric tons wet
salmon_mass <- readRDS("posteriors/derived_quantities/salmon_mass.rds")



# mean annual escapement (millions of fish)
mean_annual_escapement_millions <- fish_escapement %>% 
  filter(year >= 1976) %>% # only after 1976
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

write_csv(mean_annual_escapement_millions, file = "tables/ms_tables/tbl1_mean_annual_escape.csv")

# mean annual metric tons escapement by regions

mean_escape_mass = salmon_mass %>% 
  group_by(location, species) %>% 
  summarize(median = median(metric_tons)) %>% 
  pivot_wider(names_from = species, values_from = median) %>%
  adorn_totals("row") 

write_csv(mean_escape_mass, file = "tables/ms_tables/tbl1_mean_escape_mass.csv")

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


# Change in mean annual metric tons escapement
salmon_mass %>% 
  filter(year <= 1981 | year >= 2010) %>% 
  mutate(year_group = case_when(year <= 1981 ~ "start", 
                   TRUE ~ 'end')) %>% 
  group_by(location, species, year_group, .draw) %>% 
  summarize(metric_tons = mean(metric_tons)) %>% 
  pivot_wider(names_from = year_group, values_from = metric_tons) %>% 
  mutate(diff = end - start,
         prop = diff/start) %>% 
  ungroup() %>% 
  # filter(species == "Total") %>% 
  group_by(.draw, location, species) %>% 
  summarize(diff = sum(diff),
            prop = sum(prop)) %>% 
  group_by(location, species) %>%
  median_qi(diff, prop) %>% 
  print(n = Inf)

# Change in mean annual millions escapement
fish_escapement  %>% 
  filter(year <= 1981 | year >= 2010) %>% 
  mutate(year_group = case_when(year <= 1981 ~ "start", 
                                TRUE ~ 'end')) %>% 
  group_by(location, species, year_group) %>% 
  summarize(value = mean(value)) %>% 
  summarize(total = sum(value, na.rm = T))

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


mean_subsidy_concentration = all_chem_posts %>% 
  group_by(species, chemical, type, units) %>% 
  median_qi(.epred) %>% 
  select(species, chemical, .epred) %>% 
  pivot_wider(names_from = species, values_from = .epred)

write_csv(mean_subsidy_concentration, file = "tables/ms_tables/tbl8_mean_subsidy_concentration.csv")


chem_species_prop %>% 
  filter(year == 1976 | year == 2015) %>%
  select(-All, -proportion) %>% 
  group_by(year, .draw, chemical, species) %>% 
  summarize(value = sum(value)) %>% 
  group_by(year, .draw, chemical) %>% 
  mutate(total = sum(value),
         prop = value/total) %>% 
  select(-value, -total) %>% 
  pivot_wider(names_from = year, values_from = prop) %>% 
  mutate(diff = `2015` - `1976`) %>% 
  group_by(chemical, species) %>% 
  median_qi(diff) %>% 
  ggplot(aes(x = species, y = diff, ymin = .lower, ymax = .upper)) + 
  geom_pointrange() + 
  facet_wrap(~chemical) +
  geom_hline(yintercept = 0)

chem_species_prop %>% 
  filter(year == 1976 | year == 2015) %>%
  select(-All, -proportion) %>% 
  group_by(year, .draw, chemical, species) %>% 
  summarize(value = sum(value)) %>% 
  group_by(year, .draw, chemical) %>% 
  mutate(total = sum(value),
         prop = value/total) %>% 
  select(-value, -total) %>% 
  ungroup() %>% 
  mutate(species = fct_relevel(species, "Pink", "Sockeye", "Chum", "Coho", "Chinook")) %>% 
  ggplot(aes(x = species, y = prop, fill = year)) +
  geom_violin(aes(group = interaction(species, year)),
              position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~chemical)

# proportion biotransport change by species
chem_species_prop %>% 
  filter(year == 1976 | year == 2015) %>%
  select(-All, -proportion) %>% 
  group_by(year, .draw, species) %>% 
  summarize(value = sum(value)) %>% 
  pivot_wider(names_from = year, values_from = value) %>% 
  mutate(species_change = `2015` - `1976`) %>% 
  mutate(species_change = case_when(species_change < 0 ~ 0, TRUE ~ species_change)) %>% 
  group_by(.draw) %>% 
  mutate(total_76 = sum(`1976`),
         total_15 = sum(`2015`),
         total_change = total_15 - total_76) %>% 
  mutate(proportional_species_change = species_change/total_change) %>% 
  group_by(species) %>% 
  median_qi(proportional_species_change)

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

write_csv(mean_sd_annual_flux, file = "tables/ms_tables/tbl2_mean_sd_annual_flux.csv")

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

write_csv(cumulative_sd_annual_flux, file = "tables/ms_tables/tbl3_cumulative_sd_annual_flux.csv")

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

# total biotransport by region
change_1976_2015 <- flux_predictions %>% 
  filter(year == 1976 | year ==2015) %>% 
  mutate(kg_flux = mg_flux/1e6) %>% 
  dplyr::select(year, chemical, .draw, kg_flux, location) %>% 
  pivot_wider(names_from = chemical, values_from = kg_flux) %>% 
  mutate(All = DHA + DDTs + Hg + PCBs + N + P + PBDEs + EPA) %>% 
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
            percent_diff = median(percent_change) - 1) %>% 
  pivot_longer(cols = c(kg_yr, percent_diff), names_to = "metric") %>% 
  unite(col = "loc_metric", location:metric, sep = "_") %>% 
  pivot_wider(names_from = loc_metric, values_from = value) %>% 
  arrange(-All_kg_yr)

write_csv(change_1976_2015, file = "tables/ms_tables/tbl7_change_1976_2015.csv")  
  
# total biotransport by species
change_1976_2015_species <- flux_predictions %>% 
  filter(year == 1976 | year ==2015) %>% 
  mutate(kg_flux = mg_flux/1e6) %>% 
  dplyr::select(year, chemical, .draw, kg_flux, location, species) %>% 
  group_by(year, .draw, location, species) %>% 
  summarize(kg_flux = sum(kg_flux)) %>%  
  pivot_wider(values_from = kg_flux, names_from = location) %>% 
  mutate(All = BCWC + BeringSea + CentralAK + SEAK) %>% 
  pivot_longer(cols = c(-species, -year, -.draw),
               names_to = "location", values_to = "kg_year") %>% 
  pivot_wider(names_from = year, values_from = kg_year) %>% 
  mutate(change = `2015` - `1976`,
         percent_change = `2015`/`1976`) %>% 
  group_by(location, species) %>% 
  summarize(kg_yr = median(change),
            percent_diff = median(percent_change)-1) %>% 
  pivot_longer(cols = c(kg_yr, percent_diff), names_to = "metric") %>% 
  unite(col = "loc_metric", c(location,metric), sep = "_") %>% 
  pivot_wider(names_from = loc_metric, values_from = value)

write_csv(change_1976_2015_species, file = "tables/ms_tables/tbl6_change_1976_2015_species.csv")  



# median annual flux total biotransport (contaminants + nutrients)
mean_total_biotransport = flux_predictions %>% 
  # filter(year == 1976 | year ==2015) %>% 
  mutate(kg_flux = mg_flux/1e6) %>% 
  dplyr::select(year, chemical, .draw, kg_flux, location) %>% 
  group_by(year, .draw, location) %>% 
  summarize(total_kg_flux = sum(kg_flux)) %>% 
  pivot_wider(values_from = total_kg_flux, names_from = location) %>% 
  mutate(All = BCWC + BeringSea + CentralAK + SEAK) %>% 
  pivot_longer(cols = c(-year, -.draw),
               names_to = "location", values_to = "kg_year") %>% 
  group_by(.draw, location) %>% 
  summarize(median = median(kg_year)) %>% 
  group_by(location) %>% 
  summarize(median_annual_flux = median(median))


write_csv(mean_total_biotransport, file = "tables/ms_tables/tbl4_mean_total_biotransport.csv")


# proportional contribution to median annual flux
prop_contribution_annual <- flux_predictions %>% 
  # filter(year == 1976 | year ==2015) %>% 
  mutate(kg_flux = mg_flux/1e6) %>% 
  dplyr::select(year, chemical, .draw, kg_flux, location) %>% 
  group_by(year, species, .draw, location) %>% 
  summarize(region_species_kg_flux = sum(kg_flux)) %>% 
  pivot_wider(values_from = region_species_kg_flux, names_from = location) %>% 
  mutate(All = BCWC + BeringSea + CentralAK + SEAK) %>% 
  pivot_longer(cols = c(-year, -.draw, -species),
               names_to = "location", values_to = "kg_year") %>% 
  group_by(year, location, .draw) %>% 
  mutate(total = sum(kg_year),
         proportion = kg_year/total) %>% 
  group_by(.draw, location, species) %>% 
  summarize(median = median(proportion)) %>% 
  group_by(location, species) %>% 
  summarize(median = median(median))

write_csv(prop_contribution_annual, file = "tables/ms_tables/tbl4_prop_contribution_annual.csv")  

# median total transport
flux_predictions %>% 
  # filter(year == 1976 | year ==2015) %>% 
  mutate(kg_flux = mg_flux/1e6) %>% 
  dplyr::select(year, chemical, .draw, kg_flux, location) %>% 
  group_by(year, species, .draw, location) %>% 
  summarize(region_species_kg_flux = sum(kg_flux)) %>% 
  pivot_wider(values_from = region_species_kg_flux, names_from = location) %>% 
  mutate(All = BCWC + BeringSea + CentralAK + SEAK) %>% 
  pivot_longer(cols = c(-year, -.draw, -species),
               names_to = "location", values_to = "kg_year") %>% 
  group_by(year, location, .draw) %>% 
  mutate(total = sum(kg_year),
         proportion = kg_year/total) %>% 
  group_by(.draw, location, species) %>% 
  summarize(median = median(kg_year)) %>% 
  group_by(location, species) %>% 
  summarize(median = median(median))


# change in size, escapement abundance, and escapement biomass
fish_mass_kgww_of_individual_fish <- read_csv("data/raw_data/fish_mass_kgww_of_individual_fish.csv") %>% 
  clean_names()

escape_change_mill <- fish_escapement %>% 
  filter(year == 1976|year == 2015) %>% 
  select(year, species, location, value) %>% 
  pivot_wider(names_from = location, values_from = value) %>% 
  rowwise() %>% 
  mutate(overall = sum(c_across(`BeringSea`:`BCWC`))) %>% 
  pivot_longer(cols = c(-year, -species)) %>% 
  pivot_wider(names_from = year, values_from = value) %>% 
  mutate(diff = `2015` - `1976`) %>% 
  select(species, name, diff) %>% 
  pivot_wider(names_from = name, values_from = diff) %>% 
  adorn_totals(where = "row") %>% 
  mutate(`40 year change` = "Escapement (millions)")


escape_change_size <- fish_mass_kgww_of_individual_fish %>% 
  pivot_longer(cols = c(-year, -units, -source)) %>% 
  mutate(species = case_when(grepl("pink", name) ~ "Pink",
                             grepl("chum", name) ~ "Chum",
                             grepl("sockeye", name) ~ "Sockeye",
                             grepl("chinook", name) ~ "Chinook",
                             TRUE ~ "Coho"),
         location = case_when(grepl("bering", name) ~ "BeringSea",
                              grepl("central", name) ~ "CentralAK",
                              grepl("seak", name) ~ "SEAK",
                              grepl("bc_wc", name) ~ "BCWC")) %>% 
  filter(year == 1976|year == 2015) %>% 
  select(year, species, location, value) %>% 
  pivot_wider(names_from = location, values_from = value)%>% 
  pivot_longer(cols = c(-year, -species)) %>% 
  pivot_wider(names_from = year, values_from = value) %>% 
  mutate(diff = `2015` - `1976`) %>% 
  select(species, name, diff) %>% 
  pivot_wider(names_from = name, values_from = diff) %>% 
  rowwise() %>% 
  mutate(overall = mean(c_across(`BeringSea`:`BCWC`))) %>% 
  mutate(`40 year change` = "Size (kg wet)")


gam_salmon_posts <- readRDS("posteriors/gam_salmon_posts.rds") # Salmon escapement in kg wet mass

escape_change_metric_tons <- gam_salmon_posts %>% 
  select(-kg) %>% 
  filter(year == 1976 | year == 2015) %>% 
  pivot_wider(names_from = year, values_from = metric_tons) %>% 
  mutate(diff = `2015` - `1976`) %>% 
  select(species, location, .draw, diff) %>% 
  pivot_wider(names_from = location, values_from = diff) %>% 
  rowwise() %>% 
  mutate(overall = sum(c_across("BCWC":"SEAK"))) %>% 
  pivot_longer(cols = c(-species, -.draw)) %>% 
  group_by(species, name) %>% 
  summarize(diff = median(value)) %>% 
  pivot_wider(names_from = name, values_from = diff) %>% 
  adorn_totals(where = "row") %>% 
  mutate(`40 year change` = "Escapement (MT wet)")


all_change <- bind_rows(escape_change_metric_tons, escape_change_mill, escape_change_size) %>% 
  select(species, `40 year change`, overall, everything()) %>% 
  mutate(order = case_when(`40 year change` == "Size (kg wet)" ~ 1,
                           `40 year change` == "Escapement (millions)" ~ 2,
                           TRUE ~ 3)) %>% 
  arrange(species, order) %>% 
  select(species, `40 year change`, overall, BeringSea, CentralAK, SEAK, BCWC) 


write_csv(all_change, file = "tables/ms_tables/tbl5_all_change.csv")



# chem per fish
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


# average fish size
mean_fish_size <- fish_mass_kgww_of_individual_fish %>% 
  pivot_longer(cols = c(-year, -units, -source)) %>% 
  mutate(species = case_when(grepl("pink", name) ~ "Pink",
                             grepl("chum", name) ~ "Chum",
                             grepl("sockeye", name) ~ "Sockeye",
                             grepl("chinook", name) ~ "Chinook",
                             TRUE ~ "Coho"),
         location = case_when(grepl("bering", name) ~ "BeringSea",
                              grepl("central", name) ~ "CentralAK",
                              grepl("seak", name) ~ "SEAK",
                              grepl("bc_wc", name) ~ "BCWC")) %>% 
  group_by(species) %>% 
  filter(year >= 1976) %>% 
  summarize(mean_size_kg = mean(value))

biotransport_potential_grams_per_individual <- all_chem_posts %>% 
  left_join(mean_fish_size) %>% 
  mutate(grams_per_fish = (.epred*mean_size_kg)/1e3) %>% 
  group_by(species, chemical, type) %>% 
  summarize(mean_grams_per_fish = mean(grams_per_fish)) %>% 
  pivot_wider(names_from = species, values_from = mean_grams_per_fish)


write_csv(biotransport_potential_grams_per_individual, file = "tables/ms_tables/tbl8_biotransport_potential_grams_per_individual.csv")



# loading ratios ----------------------------------------------------------

species_ind_average = readRDS("posteriors/derived_quantities/species_ind_average.rds")

ratio_summaries = species_ind_average %>% 
  group_by(species) %>% 
  summarize(median = median(ratio_1e6),
            mean = mean(ratio_1e6),
            low95 = quantile(ratio_1e6, probs = 0.025),
            high95 = quantile(ratio_1e6, probs = 0.975),
            sd = sd(ratio_1e6)) %>% 
  arrange(median)

write_csv(ratio_summaries, file = "tables/ratio_summaries.csv")


mean_kg_per_species = read_csv("data/raw_data/fish_mass_kgww_of_individual_fish.csv") %>% 
  clean_names() %>% 
  pivot_longer(cols = -c(year, units, source)) %>% 
  select(year, name, value) %>% 
  filter(year >= 1976) %>% 
  mutate(location = case_when(grepl("bering", name) ~ "BeringSea",
                              grepl("central", name) ~ "CentralAK",
                              grepl("seak", name) ~ "SEAK",
                              TRUE ~ "BCWC")) %>% 
  mutate(species = case_when(grepl("pink", name) ~ "Pink",
                             grepl("chum", name) ~ "Chum",
                             grepl("sockeye", name) ~ "Sockeye",
                             grepl("coho", name) ~ "Coho",
                             TRUE ~ "Chinook")) %>% 
  select(-name) %>% 
  group_by(location, species) %>% 
  summarize(mean_kg_ind = mean(value))

species_ind_average = readRDS(file = "posteriors/derived_quantities/species_ind_average.rds")

table_si9_data = species_ind_average %>% 
  select(species, cont_total, nut_total, ratio_1e6) %>% 
  mutate(nut_total = nut_total/1e6) %>% 
  pivot_longer(cols = c(cont_total, nut_total, ratio_1e6)) %>% 
  group_by(species, name) %>% 
  summarize(mean = median(value),
            sd = sd(value)) 


table_si9 = table_si9_data %>% 
  mutate(mean_sd = paste0(round(mean, 2), "+/-(", round(sd, 2), ")")) %>% 
  select(-mean, -sd) %>% 
  pivot_wider(names_from = species, values_fro = mean_sd) %>% 
  select(name, Chinook, Coho, Sockeye, Chum, Pink)

write_csv(table_si9, file = "tables/table_si9.csv")

