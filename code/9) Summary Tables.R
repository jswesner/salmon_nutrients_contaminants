library(janitor)
library(tidybayes)
library(tidyverse)

trophic_levels <- tibble(tl = c(4.3, 3.9, 3.9, 3.6, 3.5),
                         species = c("Chinook", "Sockeye", "Coho",
                                     "Chum", "Pink"),
                         source = "Qin, Y., & Kaeriyama, M. (2016). Feeding habits and trophic levels of Pacific salmon (Oncorhynchus spp.) in the North Pacific Ocean. N Pac Anadromous Fish Com Bul, 6, 469-481.")

# escapement --------------------------------------------------------------

#load escapement

# millions
fish_escapement <- read_csv("data/raw_data/fish_escapement_dont_post.csv") %>% clean_names() %>% 
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


# fish size
fish_mass_kgww_of_individual_fish <- read_csv("data/raw_data/fish_mass_kgww_of_individual_fish.csv") %>% 
  clean_names()

mean_individual_size = fish_mass_kgww_of_individual_fish %>% 
  filter(year >= 1976) %>% 
  pivot_longer(cols = c(-year,-units,-source)) %>% 
  mutate(species = case_when(grepl("pink", name) ~ "Pink",
                             grepl("chum", name) ~ "Chum",
                             grepl("sockeye", name) ~ "Sockeye",
                             grepl("chinook", name) ~ "Chinook",
                             TRUE ~ "Coho"),
         location = case_when(grepl("bering", name) ~ "BeringSea",
                              grepl("central", name) ~ "CentralAK",
                              grepl("seak", name) ~ "SEAK",
                              grepl("bc_wc", name) ~ "BCWC")) %>% 
  group_by(species, location) %>% 
  summarize(median = round(median(value, na.rm = T),1)) %>% 
  pivot_wider(names_from = species, values_from = median) %>% 
  rowwise() %>% 
  mutate(All = median(Chinook, Chum, Coho, Pink, Sockeye, na.rm = T)) %>% 
  ungroup %>% 
  reframe(location = c(location, 'mean'),
            across(where(is.numeric), ~ c(., mean(.))))
  
write_csv(mean_individual_size, file = "tables/tbl_ed1_mean_individual_size.csv")

# metric tons wet
salmon_mass <- readRDS("posteriors/derived_quantities/salmon_mass.rds")
gam_salmon_posts = readRDS("posteriors/gam_salmon_posts.rds")

# mean annual escapement (millions of fish)
mean_annual_escapement_millions <- fish_escapement %>% 
  filter(year >= 1976) %>% # only after 1976
  dplyr::select(year, value, species, location) %>%
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
  summarize(median = median(value)) %>% 
  pivot_wider(names_from = species, values_from = median) 

fish_escapement %>% 
  filter(year >= 1976) %>% 
  group_by(year) %>% 
  reframe(total = sum(value)) %>% 
  reframe(mean = median(total))


write_csv(mean_annual_escapement_millions, file = "tables/tbl1_mean_annual_escape.csv")

# mean annual metric tons escapement by regions

mean_escape_mass = salmon_mass %>% 
  group_by(location, species) %>% 
  summarize(mean = median(metric_tons)) %>% 
  pivot_wider(names_from = species, values_from = mean) %>%
  adorn_totals("row") 

write_csv(mean_escape_mass, file = "tables/tbl1_mean_escape_mass.csv")

# mean annual metric tons escapement
salmon_mass %>% 
  group_by(species, .draw, year) %>%
  summarize(total_mt = sum(metric_tons)) %>% 
  pivot_wider(names_from = species, values_from = total_mt) %>% 
  pivot_longer(cols = c(-.draw, -Total, -year)) %>% 
  mutate(proportion = value/Total) %>% 
  group_by(name, .draw) %>% 
  summarize(mean_mt = median(value),
            mean_proportion = median(proportion)) %>% 
  group_by(name) %>% 
  summarize(mean_mt = median(mean_mt),
            mean_proportion = median(mean_proportion))

# mean annual metric tons escapement - total
salmon_mass %>% 
  group_by(species, .draw, year) %>%
  summarize(total_mt = sum(metric_tons)) %>% 
  pivot_wider(names_from = species, values_from = total_mt) %>% 
  pivot_longer(cols = c(-.draw, -Total, -year)) %>% 
  mutate(proportion = value/Total) %>% 
  group_by(name, .draw) %>% 
  summarize(mean_mt = median(value),
            mean_proportion = median(proportion)) %>% 
  group_by(name) %>% 
  summarize(mean_mt = median(mean_mt),
            mean_proportion = median(mean_proportion))

# Change in mean annual metric tons escapement
gam_salmon_posts %>% 
  group_by(.draw, year) %>% 
  reframe(total = sum(metric_tons)) %>% 
  filter(year == 1976 | year == 2015) %>% 
  pivot_wider(names_from = year,
              values_from = total) %>% 
  mutate(diff = `2015` - `1976`,
         percent = diff/`1976`) %>% 
  reframe(mean = median(diff),
          sd = sd(diff),
          percent = median(percent),
          mean_1976 = median(`1976`))

gam_salmon_posts %>% 
  group_by(.draw, year, species) %>% 
  reframe(total = sum(metric_tons)) %>% 
  filter(year == 1976 | year == 2015) %>% 
  pivot_wider(names_from = year,
              values_from = total) %>% 
  mutate(diff = `2015` - `1976`) %>% 
  group_by(species) %>% 
  reframe(mean = median(diff),
          sd = sd(diff))

gam_salmon_posts %>% 
  group_by(.draw, year, species, location) %>% 
  reframe(total = sum(metric_tons)) %>% 
  filter(year == 1976 | year == 2015) %>% 
  pivot_wider(names_from = year,
              values_from = total) %>% 
  mutate(diff = `2015` - `1976`) %>% 
  group_by(species, location) %>% 
  reframe(mean = median(diff),
          sd = sd(diff)) %>% 
  dplyr::select(-sd) %>% 
  pivot_wider(names_from = location, values_from = mean) %>% 
  dplyr::select(species, BeringSea, CentralAK, SEAK, BCWC)

gam_salmon_posts %>% 
  group_by(.draw, year, location) %>% 
  reframe(total = sum(metric_tons)) %>% 
  filter(year == 1976 | year == 2015) %>% 
  pivot_wider(names_from = year,
              values_from = total) %>% 
  mutate(diff = `2015` - `1976`) %>% 
  group_by(location) %>% 
  reframe(mean = median(diff),
          sd = sd(diff)) %>% 
  dplyr::select(-sd) %>% 
  pivot_wider(names_from = location, values_from = mean) %>% 
  dplyr::select(BeringSea, CentralAK, SEAK, BCWC)


# Change in mean annual millions escapement
fish_escapement %>% 
  filter(year >= 1976) %>% 
  group_by(year) %>% 
  reframe(total = sum(value)) %>% 
  filter(year == min(year) | year == max(year))



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
  tidybayes::mean_qi(.epred) %>% 
  dplyr::select(species, chemical, .epred) %>% 
  pivot_wider(names_from = species, values_from = .epred)

write_csv(mean_subsidy_concentration, file = "tables/tbl8_mean_subsidy_concentration.csv")


chem_species_prop %>% 
  filter(year == 1976 | year == 2015) %>%
  dplyr::select(-All, -proportion) %>% 
  group_by(year, .draw, chemical, species) %>% 
  summarize(value = sum(value)) %>% 
  group_by(year, .draw, chemical) %>% 
  mutate(total = sum(value),
         prop = value/total) %>% 
  dplyr::select(-value, -total) %>% 
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
  dplyr::select(-All, -proportion) %>% 
  group_by(year, .draw, chemical, species) %>% 
  summarize(value = sum(value)) %>% 
  group_by(year, .draw, chemical) %>% 
  mutate(total = sum(value),
         prop = value/total) %>% 
  dplyr::select(-value, -total) %>% 
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
  dplyr::select(-All, -proportion) %>% 
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
# mean_sd_annual_flux <- flux_with_all  %>% 
#   group_by(species, chemical, type) %>% 
#   mutate(value = case_when(type == "Contaminants" ~ total_kg,
#                              TRUE ~ total_kg/1000)) %>% 
#   group_by(species, chemical, type, .draw) %>%
#   summarize(mean_year = median(value)) %>%       # annual flux
#   ungroup() %>% 
#   group_by(species, chemical, type) %>% 
#   summarize(mean = median(mean_year),        # mean of mean per year
#          sd = sd(mean_year)) %>%        # sd of mean per year
#   mutate(mean = case_when(mean > 2 ~ round(mean, 0),
#                           TRUE ~ round(mean, 2)),
#          sd = case_when(mean > 2 ~ round(sd, 0),
#                         TRUE ~ round(sd, 2))) %>% 
#   mutate(mean_sd = paste0(mean," \u00B1 ", sd)) %>% 
#   dplyr::select(-mean, -sd) %>%
#   pivot_wider(names_from = species, values_from = mean_sd) %>% 
#   arrange(desc(type), desc(Total)) %>% 
#   mutate(units = case_when(type == "Nutrients" ~ "MT/y", 
#                            TRUE ~ "Kg/y"),
#          summary = "Average Annual")

median_qi_annual_flux = flux_with_all  %>% 
  group_by(species, chemical, type) %>% 
  mutate(value = case_when(type == "Contaminants" ~ total_kg,
                           TRUE ~ total_kg/1000)) %>% 
  group_by(species, chemical, type, .draw) %>%
  summarize(mean_year = median(value)) %>%       # annual flux
  ungroup() %>% 
  group_by(species, chemical, type) %>%
  # summarize(mean = median(mean_year),        # mean of mean per year
  #           sd = sd(mean_year),
  #           lower = quantile()) %>%        # sd of mean per year
  median_qi(mean_year) %>% 
  mutate(mean_year = case_when(mean_year > 2 ~ round(mean_year, 0),
                               TRUE ~ round(mean_year, 2)),
         .lower = case_when(mean_year > 2 ~ round(.lower, 0),
                            TRUE ~ round(.lower, 2)),
         .upper = case_when(mean_year > 2 ~ round(.upper, 0),
                            TRUE ~ round(.upper, 2))) %>% 
  mutate(median_qi = paste0(mean_year, " (", .lower, ",", .upper, ")")) %>% 
  # mutate(mean_sd = paste0(mean_year," \u00B1 ", sd)) 
  dplyr::select(species, chemical, type, median_qi) %>% 
  pivot_wider(names_from = species, values_from = median_qi) %>% 
  arrange(desc(type), desc(Total)) %>% 
  mutate(units = case_when(type == "Nutrients" ~ "MT/y", 
                           TRUE ~ "Kg/y"),
         summary = "Average Annual")

write_csv(median_qi_annual_flux, file = "tables/tbl2_median_qi_annual_flux.csv")

median_iqr_annual_flux <- flux_with_all  %>% 
  group_by(species, chemical, type) %>% 
  mutate(value = case_when(type == "Contaminants" ~ total_kg,
                           TRUE ~ total_kg/1000)) %>% 
  group_by(species, chemical, type, .draw) %>%
  summarize(median_year = median(value)) %>%       # annual flux
  ungroup() %>% 
  group_by(species, chemical, type) %>% 
  summarize(median = median(median_year/1000),        # mean of mean per year
            low25 = quantile(median_year/1000, 0.25),
            high75 = quantile(median_year/1000, 0.75)) %>%        # sd of mean per year
  mutate(median = case_when(median > 2 ~ round(median, 0),
                          TRUE ~ round(median, 2)),
         low25 = case_when(median > 2 ~ round(low25, 0),
                        TRUE ~ round(low25, 2)),
         high75 = case_when(median > 2 ~ round(high75, 0),
                           TRUE ~ round(high75, 2))) %>% 
  mutate(median_sd = paste0(median," (", low25, ",", high75, ")")) %>% 
  dplyr::select(-median, -low25, -high75) %>%
  pivot_wider(names_from = species, values_from = median_sd) %>% 
  arrange(desc(type), desc(Total)) %>% 
  mutate(units = case_when(type == "Nutrients" ~ "MT/y", 
                           TRUE ~ "Kg/y"),
         summary = "Average Annual")

write_csv(median_iqr_annual_flux, file = "tables/tbl2_median_iqr_annual_flux.csv")


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

write_csv(cumulative_sd_annual_flux, file = "tables/tbl3_cumulative_sd_annual_flux.csv")

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
  summarize(mean_flux = median(total_kg)) %>% 
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
  summarize(mean_flux = median(total_kg)) %>% 
  pivot_wider(names_from = species, values_from = mean_flux) %>% 
  mutate(total = Chinook + Chum + Coho + Pink + Sockeye,
         prop = Chinook/total) %>% 
  print(n = Inf)


# proportions
chem_species_prop <- flux_predictions %>% 
  # filter(.draw <= 2) %>% 
  dplyr::select(species, location, year, .draw, mg_flux, chemical) %>% 
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
  summarize(mean_prop = median(proportion),
            mean_mt = median(species_mt)) %>% 
  group_by(chemical, species) %>% 
  summarize(mean_prop = median(mean_prop),
            mean_mt = median(mean_mt)) %>% 
  # pivot_wider(names_from = species, values_from = mean_prop) %>% 
  print(n = Inf)

# total biotransport by region
# Table S3
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
  mutate(conc_yr = case_when(name == "Hg" ~ kg_yr,
                             name == "PCBs" ~ kg_yr,
                             name == "DDTs" ~ kg_yr,
                             name == "PBDEs" ~ kg_yr,
                             TRUE ~ kg_yr/1000),
         concentrations = case_when(name == "Hg" ~ "kg_yr",
                                    name == "PCBs" ~ "kg_yr",
                                    name == "DDTs" ~ "kg_yr",
                                    name == "PBDEs" ~ "kg_yr",
                                    TRUE ~ "mt_yr")) %>% 
  pivot_longer(cols = c(conc_yr, percent_diff), names_to = "metric") %>% 
  unite(col = "loc_metric", location:metric, sep = "_") %>% 
  pivot_wider(names_from = loc_metric, values_from = value) %>% 
  mutate(name = as.factor(name),
         name = fct_relevel(name, "N", "DHA", 'EPA', 'P', 'Hg', 'PCBs', 'DDTs', 'PBDEs', 'All')) %>% 
  arrange(name)

write_csv(change_1976_2015, file = "tables/tbl6_change_per_analyte.csv")  
  
# summarize table s3 by contaminant
flux_predictions %>% 
  filter(year == 1976 | year ==2015) %>% 
  mutate(kg_flux = mg_flux/1e6) %>% 
  dplyr::select(year, chemical, .draw, kg_flux, location, type) %>% 
  group_by(year, .draw, type) %>% 
  reframe(biotransport = sum(kg_flux)) %>% 
  pivot_wider(names_from = year, values_from = biotransport) %>% 
  mutate(change = `2015` - `1976`,
         percent_change = `2015`/`1976`) %>% 
  group_by(type) %>% 
  summarize(kg_yr = median(change),
            percent_diff = median(percent_change) - 1) 

# flux by region per fish
#bcwc 
4700000/32000000
#bering sea
3200000/20000000
#central ak
2800000/26000000
#seak
3400000/35000000

# percent contribution by pink
# nutrients from table ed2
(3448+678+387+480)/(8985 + 2316 + 1582 + 1163)
# contaminants from table ed2
(1.3 + 0.31 + 0.34 + 0.07)/(7 + 1.67 + 1.59 + 0.23)

# percent contribution by chinook
# nutrients from table ed2
(461 + 108 + 105 + 51)/(8985 + 2316 + 1582 + 1163)
# contaminants from table ed2
(0.76 + 0.23 + 0.27 + 0.06)/(7 + 1.67 + 1.59 + 0.23)

# percent contribution by coho
# nutrients from table ed2
(463 + 101 + 92 + 48)/(8985 + 2316 + 1582 + 1163)
# contaminants from table ed2
(0.48 + 0.16 + 0.19 + 0.01)/(7 + 1.67 + 1.59 + 0.23)

# total biotransport by species
tbl5_species = flux_predictions %>% 
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
  summarize(mt_yr = median(change)/1000,
            percent_diff = (median(percent_change)-1)) %>% 
  pivot_longer(cols = c(mt_yr, percent_diff), names_to = "metric") %>% 
  unite(col = "loc_metric", c(location,metric), sep = "_") %>% 
  pivot_wider(names_from = loc_metric, values_from = value)

tbl5_overall = flux_predictions %>% 
  filter(year == 1976 | year ==2015) %>% 
  mutate(kg_flux = mg_flux/1e6) %>% 
  dplyr::select(year, chemical, .draw, kg_flux, location) %>% 
  group_by(year, .draw, location) %>% 
  summarize(kg_flux = sum(kg_flux)) %>%  
  pivot_wider(values_from = kg_flux, names_from = location) %>% 
  mutate(All = BCWC + BeringSea + CentralAK + SEAK) %>% 
  pivot_longer(cols = c(-year, -.draw),
               names_to = "location", values_to = "kg_year") %>% 
  pivot_wider(names_from = year, values_from = kg_year) %>% 
  mutate(change = `2015` - `1976`,
         percent_change = `2015`/`1976`) %>% 
  group_by(location) %>% 
  summarize(mt_yr = median(change)/1000,
            percent_diff = (median(percent_change)-1)) %>% 
  pivot_longer(cols = c(mt_yr, percent_diff), names_to = "metric") %>% 
  unite(col = "loc_metric", c(location,metric), sep = "_") %>% 
  pivot_wider(names_from = loc_metric, values_from = value) %>% 
  mutate(species = "overall")

tbl5 = bind_rows(tbl5_species, tbl5_overall) %>% 
  dplyr::select(species, All_mt_yr, All_percent_diff,
                BeringSea_mt_yr, BeringSea_percent_diff,
                CentralAK_mt_yr, CentralAK_percent_diff,
                SEAK_mt_yr, SEAK_percent_diff, 
                BCWC_mt_yr, BCWC_percent_diff)

write_csv(tbl5, file = "tables/tbl5_change_in_biotransport.csv")

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
  summarize(mean = median(kg_year)) %>% 
  group_by(location) %>% 
  summarize(mean_annual_flux = median(mean))


write_csv(mean_total_biotransport, file = "tables/tbl3_mean_total_biotransport.csv")

# abstract increases 

flux_predictions %>% 
  filter(year == 1976 | year ==2015) %>% 
  group_by(type, .draw, year) %>% 
  reframe(total = sum(mg_flux)) %>% 
  pivot_wider(names_from = year, values_from = total) %>% 
  mutate(diff = `2015` - `1976`,
         percent = 100*(diff/`1976`)) %>% 
  group_by(type) %>% 
  median_qi(percent)

flux_predictions %>% 
  # filter(year == 1976 | year ==2015) %>% 
  mutate(kg_flux = mg_flux/1e6) %>% 
  dplyr::select(year, chemical, .draw, kg_flux, location) %>% 
  group_by(year, .draw, location, chemical) %>% 
  summarize(total_kg_flux = sum(kg_flux)) %>% 
  pivot_wider(values_from = total_kg_flux, names_from = location) %>% 
  mutate(All = BCWC + BeringSea + CentralAK + SEAK) %>% 
  dplyr::select(-BCWC, -BeringSea, -CentralAK, -SEAK) %>% 
  group_by(.draw, chemical) %>% 
  summarize(mean = median(All)/1000) %>% 
  group_by(chemical) %>% 
  reframe(mean_all = median(mean),
          sd_all = sd(mean))


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

write_csv(prop_contribution_annual, file = "tables/tbl3_prop_contribution_annual.csv")  

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
  dplyr::select(year, species, location, value) %>% 
  pivot_wider(names_from = location, values_from = value) %>% 
  rowwise() %>% 
  mutate(overall = sum(c_across(`BeringSea`:`BCWC`))) %>% 
  pivot_longer(cols = c(-year, -species)) %>% 
  pivot_wider(names_from = year, values_from = value) %>% 
  mutate(diff = `2015` - `1976`) %>% 
  dplyr::select(species, name, diff) %>% 
  pivot_wider(names_from = name, values_from = diff) %>% 
  adorn_totals(where = "row") %>% 
  mutate(`40 year change` = "Escapement (millions)")


temp_tbl4 = fish_mass_kgww_of_individual_fish %>% 
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
  dplyr::select(year, species, location, value) %>% 
  pivot_wider(names_from = location, values_from = value) %>% 
  pivot_longer(cols = c(-year, -species)) %>% 
  pivot_wider(names_from = year, values_from = value) %>% 
  mutate(diff = round(`2015` - `1976`, 2),
         percent = 100*round(diff/`1976`, 2))

temp_tbl4_all = temp_tbl4 %>% 
  group_by(species) %>% 
  reframe(`1976` = median(`1976`),
          `2015` = median(`2015`)) %>% 
  mutate(diff = round(`2015` - `1976`, 2),
         percent = 100*round(diff/`1976`, 2)) %>% 
  mutate(name = 'overall')

tbl4_mass = bind_rows(temp_tbl4, 
                      temp_tbl4_all) %>% 
  rename(location = name,
         mass = diff,
         mass_percent = percent) %>% 
  dplyr::select(species, location, mass, mass_percent) %>% 
  pivot_longer(cols = starts_with("mass")) %>% 
  mutate(value = as.character(value))

tbl4_escape = gam_salmon_posts %>% 
  dplyr::select(-kg) %>% 
  filter(year == 1976 | year == 2015) %>% 
  pivot_wider(names_from = year, values_from = metric_tons) %>% 
  mutate(diff = `2015` - `1976`) %>% 
  dplyr::select(species, location, .draw, diff) %>% 
  pivot_wider(names_from = location, values_from = diff) %>% 
  rowwise() %>% 
  mutate(overall = sum(c_across("BCWC":"SEAK"))) %>% 
  pivot_longer(cols = c(-species, -.draw)) %>% 
  group_by(species, name) %>%
  median_qi() %>% 
  dplyr::select(species, name, value, .lower, .upper) %>% 
  mutate(diff_median = as.character(round(value, )),
         diff_cri = paste0("(", round(.lower, 0), ", ", round(.upper, 0), ")")) %>% 
  dplyr::select(species, name, diff_median, diff_cri) %>% 
  rename(location = name) %>% 
  pivot_longer(cols = starts_with("diff"))

tbl4_escape_all = gam_salmon_posts %>% 
  dplyr::select(-kg) %>% 
  filter(year == 1976 | year == 2015) %>% 
  pivot_wider(names_from = year, values_from = metric_tons) %>% 
  mutate(diff = `2015` - `1976`) %>% 
  dplyr::select(species, location, .draw, diff) %>% 
  pivot_wider(names_from = location, values_from = diff) %>% 
  rowwise() %>% 
  mutate(overall = sum(c_across("BCWC":"SEAK"))) %>% 
  pivot_longer(cols = c(-species, -.draw)) %>% 
  group_by(name, .draw) %>%
  reframe(value = sum(value)) %>% 
  group_by(name) %>% 
  median_qi(value) %>% 
  dplyr::select(name, value, .lower, .upper) %>% 
  mutate(diff_median = as.character(round(value, )),
         diff_cri = paste0("(", round(.lower, 0), ", ", round(.upper, 0), ")")) %>% 
  dplyr::select(name, diff_median, diff_cri) %>% 
  rename(location = name) %>% 
  pivot_longer(cols = starts_with("diff")) %>% 
  mutate(species = "overall")


tbl4_all_change = bind_rows(tbl4_mass, tbl4_escape, tbl4_escape_all) %>% 
  pivot_wider(names_from = location, values_from = value) %>% 
  mutate(name = as.factor(name),
         name = fct_relevel(name, "mass", "mass_percent", "diff_median")) %>% 
  arrange(species) %>% 
  dplyr::select(species, name, overall, BeringSea, CentralAK, everything())



write_csv(tbl4_all_change, file = "tables/tbl4_all_change.csv")

# overall return percentages
gam_salmon_posts %>% 
  filter(year == 1976| year == 2015) %>% 
  group_by(.draw, year) %>% 
  reframe(mt = sum(metric_tons)) %>% 
  pivot_wider(names_from = year, values_from = mt) %>% 
  mutate(diff = `2015`-`1976`,
         percent = 100*diff/`1976`) %>% 
  median_qi(percent)


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
                                "DDTs",
                                "PCBs")) # Salmon chemical concentrations mg kg wet mass


# average fish size
mean_fish_size <- fish_mass_kgww_of_individual_fish %>% 
  clean_names() %>% 
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
  summarize(mean_size_kg = median(value))

biotransport_potential_perfish_perkg <- all_chem_posts %>% 
  left_join(mean_fish_size) %>% 
  mutate(milligrams_per_kg = .epred,
         milligrams_per_fish = (.epred)*mean_size_kg) %>% 
  group_by(species, chemical, type) %>% 
  summarize(mean_milligrams_per_kg = mean(milligrams_per_kg),
            mean_milligrams_per_fish = median(milligrams_per_fish)) %>% 
  mutate(mean_conc_per_fish = case_when(type == "Nutrients" ~ mean_milligrams_per_fish/1e6,
                                 TRUE ~ mean_milligrams_per_fish),
         per_fish_units = case_when(type == "Nutrients" ~ "kg/fish",
                                    TRUE ~ "mg/fish")) %>% 
  select(-mean_milligrams_per_fish) %>% 
  pivot_longer(cols = starts_with('mean')) %>% 
  mutate(species = paste0(species, "_", name)) %>% 
  dplyr::select(-name) %>% 
  pivot_wider(names_from = species, values_from = value) %>% 
  glimpse() %>% 
  dplyr::select(chemical, type, 
                Chinook_mean_milligrams_per_kg, Chinook_mean_conc_per_fish,
                Coho_mean_milligrams_per_kg, Coho_mean_conc_per_fish,
                Sockeye_mean_milligrams_per_kg, Sockeye_mean_conc_per_fish,
                Chum_mean_milligrams_per_kg, Chum_mean_conc_per_fish,
                Pink_mean_milligrams_per_kg, Pink_mean_conc_per_fish,
                per_fish_units)

write_csv(biotransport_potential_perfish_perkg, file = "tables/tbl7_biotransport_potential_perfish_perkg.csv")

# increased of X-XX percentage points....
read_csv(file = "plots/fig2_data.csv") %>%
  filter(panel_letter %in% letters[5:8]) %>% 
  filter(year == 1976|year == 2015) %>% 
  dplyr::select(year, chemical, species, proportion) %>% 
  pivot_wider(names_from = year, values_from = proportion) %>% 
  mutate(diff = `2015`-`1976`)

# loading ratios ----------------------------------------------------------

species_ind_average = readRDS("posteriors/derived_quantities/species_ind_average.rds")

ratio_summaries = species_ind_average %>% 
  group_by(species) %>% 
  summarize(median = median(ratio_kgmgperfish),
            mean = median(ratio_kgmgperfish),
            low95 = quantile(ratio_kgmgperfish, probs = 0.025),
            high95 = quantile(ratio_kgmgperfish, probs = 0.975),
            sd = sd(ratio_kgmgperfish)) %>% 
  arrange(median)

write_csv(ratio_summaries, file = "tables/ratio_summaries.csv")


mean_kg_per_species = read_csv("data/raw_data/fish_mass_kgww_of_individual_fish.csv") %>% 
  clean_names() %>% 
  pivot_longer(cols = -c(year, units, source)) %>% 
  dplyr::select(year, name, value) %>% 
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
  dplyr::select(-name) %>% 
  group_by(location, species) %>% 
  summarize(mean_kg_ind = median(value))

species_ind_average = readRDS(file = "posteriors/derived_quantities/species_ind_average.rds")

table_si9_data = species_ind_average %>% 
  dplyr::select(species, cont_total_mgperfish, nut_total_kgperfish, ratio_kgmgperfish) %>% 
  pivot_longer(cols = -species) %>% 
  group_by(species, name) %>% 
  median_qi(value)

table_si9 = table_si9_data %>% 
  mutate(median_cri = paste0(round(value, 2), " (", round(.lower, 2), " to ", round(.upper,2), ")")) %>%
  dplyr::select(species, name, median_cri) %>%
  pivot_wider(names_from = species, values_from = median_cri) %>% 
  dplyr::select(name, Chinook, Coho, Sockeye, Chum, Pink)

write_csv(table_si9, file = "tables/tbl9.csv")

# hazard ratios ------------------
all_chem_posts <- readRDS("posteriors/all_chem_posts.rds") 

risk_posts = all_chem_posts %>% 
  dplyr::select(-type) %>%
  mutate(.epred = 4*(.epred/1000)) %>% # convert to mg/g dry weight
  pivot_wider(names_from = chemical, values_from = .epred) %>%
  mutate(rfdm_hg = 0.000186,
         rfdm_pbde = 0.0001,
         rfdm_pcb = 0.00002,
         rfdm_ddt = 0.0005,
         bw = 81, 
         rsefa = 250,
         csefa = EPA + DHA,
         crlim = bw/(Hg/rfdm_hg + PBDEs/rfdm_pbde + PCBs/rfdm_pcb + DDTs/rfdm_ddt),
         crsefa = rsefa/csefa,
         risk_quotient = crsefa/crlim) %>% 
  left_join(trophic_levels) %>% 
  mutate(species_tl = paste0(species, "\n(TL: ", round(tl,1), ")"))

risk_posts %>% 
  group_by(species) %>%
  summarize(mean = median(risk_quotient),
            sd = sd(risk_quotient))

# raw hazard
fig_ed1_data = readRDS("plots/fig_ed1_data.rds")
species_size = readRDS(file = "posteriors/derived_quantities/species_ind_average.rds") %>% distinct(species, mean_size_kg)

raw_ratios = fig_ed1_data[[3]] %>%
  filter(species != "All") %>% 
  filter(type == "dots") %>% 
  left_join(species_size) %>% 
  group_by(species, chemical, chem_type, mean_size_kg) %>% 
  reframe(mean = median(mean_concentration_standardized*mean_size_kg)) %>% 
  group_by(species, chem_type) %>% 
  reframe(total = sum(mean)) %>% 
  pivot_wider(names_from = chem_type, values_from = total) %>% 
  mutate(nutrients_kg_perfish = Nutrients/1e6) %>% 
  mutate(ratio = nutrients_kg_perfish/Contaminants) %>% 
  rename(contaminants_kg_perfish = Contaminants) %>% 
  dplyr::select(-Nutrients) %>% 
  arrange(ratio) %>% 
  mutate_if(is.numeric, round, 2)

write_csv(raw_ratios, file = "tables/raw_ratios_referee_response.csv")

