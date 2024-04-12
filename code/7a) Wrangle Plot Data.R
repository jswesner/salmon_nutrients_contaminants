library(tidyverse)
library(cowplot)
library(janitor)
library(brms)

# This script wrangles the posteriors to make the figures in the manuscript. It should all run well, but some code takes a 
# few minutes. In those cases, we comment out that code and load a previously saved version instead.

# Figure 1 ----------------------------------------------------------------
# load posteriors that underlie the figures
gam_salmon_posts <- readRDS("posteriors/gam_salmon_posts.rds") # Salmon escapement in kg wet mass
flux_predictions <- readRDS(file = "posteriors/flux_predictions.rds") # posterior chem export

# wrangle salmon escapement total and per species

salmon_mass_temp <- gam_salmon_posts %>% 
  ungroup() %>% 
  dplyr::select(metric_tons, .draw, year, species, location) %>% 
  #calculate total kg 
  pivot_wider(names_from = species, values_from = metric_tons) %>% 
  mutate(Total = Chinook + Chum + Coho + Pink + Sockeye) %>% 
  pivot_longer(cols = c(-location, -year, -.draw), names_to = "species", values_to = "metric_tons") 

saveRDS(salmon_mass_temp, file = "posteriors/derived_quantities/salmon_mass.rds")

salmon_mass_posts <- salmon_mass_temp %>%
  group_by(species, year, .draw, location) %>% 
  summarize(metric_tons = sum(metric_tons)) %>% 
  pivot_wider(names_from = location, values_from = metric_tons) %>% 
  mutate(`All Regions` = BCWC + BeringSea + CentralAK + SEAK) %>% 
  pivot_longer(cols = c(-.draw, -species, -year), names_to = "location", values_to = "metric_tons") %>% 
  group_by(species, year, location) %>% 
  summarize(low75 = quantile(metric_tons, probs = 0.125, na.rm = T),
            median = median(metric_tons, na.rm = T),
            high75 = quantile(metric_tons, probs = 0.875, na.rm = T),
            low50 = quantile(metric_tons, probs = 0.25, na.rm = T),
            high50 = quantile(metric_tons, probs = 0.75, na.rm = T)) %>% 
  mutate(species = as.factor(str_trim(species)),
         location = fct_relevel(location, "All Regions", "SEAK", "BeringSea"),
         species = fct_relevel(species, "Pink", "Sockeye", "Chum"),
         group = case_when(species == "Total" ~ "All", TRUE ~ "Species")) %>% 
  mutate(panel = case_when(species == "Total" ~ "b) Total returns",
                           TRUE ~ "c) Species returns")) %>% 
  filter(location == "All Regions")


# wrangle nutrient and contaminant flux total and per species
total_nut_cont_flux = flux_predictions %>% 
  ungroup() %>%
  group_by(year, .draw, type) %>%
  summarize(total = sum(mg_flux/1000000)) %>%
  group_by(year, type) %>%
  summarize(median = median(total),
            low75 = quantile(total, probs = 0.125),
            high75 = quantile(total, probs = 1-0.125),
            low50 = quantile(total, probs = 0.25),
            high50 = quantile(total, probs = 1-0.25)) %>%
  ungroup() %>% 
  mutate(type = fct_relevel(type, "Nutrients"),
         species = "All")

species_nut_cont_flux = flux_predictions %>%
  ungroup() %>%
  group_by(year, .draw, type, species) %>%
  summarize(total = sum(mg_flux/1000000)) %>% # sum total transport across all regions
  mutate(total = case_when(type == "Nutrients" ~ total/1000,
                           TRUE ~ total),
         units = case_when(type == "Nutrients" ~ "MT_yr", 
                           TRUE ~ "kg_yr")) %>% 
  group_by(year, type, species) %>%  # summarize transport by year
  summarize(median = median(total),
            low75 = quantile(total, probs = 0.125),
            high75 = quantile(total, probs = 1-0.125),
            low50 = quantile(total, probs = 0.25),
            high50 = quantile(total, probs = 1-0.25)) %>%
  ungroup() %>% 
  mutate(type = fct_relevel(type, "Nutrients")) 


all_nut_cont_flux = bind_rows(total_nut_cont_flux, species_nut_cont_flux) %>% 
  mutate(panel = case_when(species == "All" & type == "Nutrients" ~ "d) Total nutrients",
                           species == "All" & type == "Contaminants" ~ "f) Total contaminants",
                           species != "All" & type == "Nutrients" ~ "e) Species nutrients",
                           TRUE ~ "g) Species contaminants")) %>% 
  select(species, year, low75, median, high75, low50, high50, panel)


# combine escapement and analyte flux

fig1_data = bind_rows(all_nut_cont_flux, 
                      salmon_mass_posts)

write_csv(fig1_data, file = "plots/fig1_data.csv")


# Figure 2 ----------------------------------------------------------------
# flux_predictions <- readRDS(file = "posteriors/flux_predictions.rds") # posterior chem export

# species_chem_nut_cont_flux = flux_predictions %>%
#   filter(.draw <= 500) %>% 
#   ungroup() %>%
#   group_by(year, .draw, type, species, chemical) %>%
#   summarize(total = sum(mg_flux/1000000)) %>% # sum total transport across all regions
#   mutate(total = case_when(type == "Nutrients" ~ total/1000,
#                            TRUE ~ total),
#          units = case_when(type == "Nutrients" ~ "MT_yr", 
#                            TRUE ~ "kg_yr")) 
# 
# saveRDS(species_chem_nut_cont_flux, file = "posteriors/species_chem_nut_cont_flux.rds")

species_chem_nut_cont_flux = readRDS(file = "posteriors/species_chem_nut_cont_flux.rds")

total_flux_species = species_chem_nut_cont_flux %>% 
  group_by(year, type, species, chemical) %>%  # summarize transport by year
  summarize(median = median(total),
            low75 = quantile(total, probs = 0.125),
            high75 = quantile(total, probs = 1-0.125),
            low50 = quantile(total, probs = 0.25),
            high50 = quantile(total, probs = 1-0.25)) %>%
  mutate(panel = case_when(chemical == "N" ~ "a) N",
                           chemical == "P" ~ "b) P",
                           chemical == "DHA" ~ "c) DHA",
                           chemical == "EPA" ~ "d) EPA",
                           chemical == "Hg" ~ "i) Hg",
                           chemical == "PCBs" ~ "j) PCBs",
                           chemical == "DDTs" ~ "k) DDTs",
                           chemical == "PBDEs" ~ "l) PBDEs")) %>% 
  mutate(species = fct_relevel(species, "Coho", "Chinook", "Chum", "Sockeye", "Pink")) %>% 
  mutate(panel_letter = str_sub(panel, 1, 1),
         measure = "total")

proportion_flux_species = total_flux_species %>% 
  group_by(year, type, chemical) %>% 
  mutate(total = sum(median)) %>% 
  mutate(proportion = median/total) %>%
  mutate(panel = case_when(chemical == "N" ~ "e) N",
                                 chemical == "P" ~ "f) P",
                                 chemical == "DHA" ~ "g) DHA",
                                 chemical == "EPA" ~ "h) EPA",
                                 chemical == "Hg" ~ "m) Hg",
                                 chemical == "PCBs" ~ "n) PCBs",
                                 chemical == "DDTs" ~ "o) DDTs",
                                 chemical == "PBDEs" ~ "p) PBDEs")) %>% 
  mutate(species = fct_relevel(species, "Coho", "Chinook", "Chum", "Sockeye", "Pink")) %>% 
  mutate(panel_letter = str_sub(panel, 1, 1),
         measure = "proportion")

fig2_data = bind_rows(total_flux_species, 
                      proportion_flux_species)

write_csv(fig2_data, file = "plots/fig2_data.csv")

# Figure 3 ----------------------------------------------------------------
# flux_predictions <- readRDS(file = "posteriors/flux_predictions.rds") # posterior chem export

# location_chem_nut_cont_flux = flux_predictions %>%
#   filter(.draw <= 500) %>%
#   ungroup() %>%
#   group_by(year, .draw, type, location, chemical) %>%
#   summarize(total = sum(mg_flux/1000000)) %>% # sum total transport across all regions
#   mutate(total = case_when(type == "Nutrients" ~ total/1000,
#                            TRUE ~ total),
#          units = case_when(type == "Nutrients" ~ "MT_yr",
#                            TRUE ~ "kg_yr")) 

location_chem_nut_cont_flux = readRDS(file = "posteriors/location_chem_nut_cont_flux.rds")

add_fig3_labels = location_chem_nut_cont_flux %>% ungroup %>% 
  distinct(chemical, location) %>%
  mutate(panel = case_when(chemical == "N" ~ "a) N",
                           chemical == "P" ~ "b) P",
                           chemical == "DHA" ~ "c) DHA",
                           chemical == "EPA" ~ "d) EPA",
                           chemical == "Hg" ~ "e) Hg",
                           chemical == "PCBs" ~ "f) PCBs",
                           chemical == "DDTs" ~ "g) DDTs",
                           chemical == "PBDEs" ~ "h) PBDEs"),
         location_fixed = case_when(location == "BeringSea" ~ "BS",
                              location == "CentralAK" ~ "CAK",
                              TRUE ~ location)) %>% 
  mutate(location_fixed = fct_relevel(location_fixed, "BCWC", "SEAK", "CAK", "BS"))

fig3_data = location_chem_nut_cont_flux %>% 
  ungroup %>% 
  left_join(add_fig3_labels) %>% 
  select(-location) %>% 
  rename(location = location_fixed) %>%
  group_by(year, chemical, type, units, panel, location) %>% 
  summarize(median = median(total),
            low75 = quantile(total, probs = 0.125),
            high75 = quantile(total, probs = 1-0.125),
            low50 = quantile(total, probs = 0.25),
            high50 = quantile(total, probs = 1-0.25)) %>%
  ungroup() %>% 
  mutate(location = as.factor(location),
         location = fct_relevel(location, "BCWC", "SEAK", "CAK", "BS"))
  
write_csv(fig3_data, file = "plots/fig3_data.csv")
# saveRDS(location_chem_nut_cont_flux_labeled, file = "posteriors/location_chem_nut_cont_flux_labeled.rds")

location_chem_nut_cont_flux_labeled = readRDS(file = "posteriors/location_chem_nut_cont_flux_labeled.rds")


# Figure 4 ----------------------------------------------------------------
# flux_predictions <- readRDS(file = "posteriors/flux_predictions.rds") # posterior chem export
all_chem_posts = readRDS(file = "posteriors/all_chem_posts.rds")

# calculate proportions of 
# species_props <- flux_predictions %>%
#   mutate(type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "Nutrients",
#                           TRUE ~ "Contaminants")) %>%
#   group_by(species, year, type, .draw) %>%
#   summarize(species_flux = sum(mg_flux)) %>%
#   group_by(year, .draw, type) %>%
#   mutate(total_flux = sum(species_flux),
#          species_prop = species_flux/total_flux) %>% 
#   mutate(panel = "b)")

# saveRDS(species_props, file = "posteriors/derived_quantities/species_props.rds")

species_props <- readRDS("posteriors/derived_quantities/species_props.rds") %>% 
  mutate(panel = "b)")

# check that it adds to ~ 1 (approximately because it is a sum of medians)
species_props %>% 
  group_by(year, type, .draw, panel) %>% 
  summarize(sum = sum(median_prop))

# add trophic levels
trophic_levels <- tibble(tl = c(4.3, 3.9, 3.901, 3.6, 3.5),
                         species = c("Chinook", "Sockeye", "Coho",
                                     "Chum", "Pink"),
                         source = "Qin, Y., & Kaeriyama, M. (2016). Feeding habits and trophic levels of Pacific salmon (Oncorhynchus spp.) in the North Pacific Ocean. N Pac Anadromous Fish Com Bul, 6, 469-481.")

diff_props <- species_props %>% 
  left_join(trophic_levels) %>%  
  # select(-species_flux, -total_flux) %>% 
  pivot_wider(names_from = type, values_from = median_prop) %>% 
  ungroup() %>% 
  mutate(cont_minus_nut = (Nutrients-Contaminants),
         species = fct_relevel(species, "Chinook", "Coho", "Sockeye", "Chum")) %>% 
  group_by(species, year) %>% 
  mutate(mean_difference = median(cont_minus_nut),
         species_tl = paste0(species, "\n(TL: ", round(tl,1), ")"))

species_props_mean <- diff_props %>% 
  group_by(species, .draw, panel) %>% 
  summarize(mean_prop = mean(cont_minus_nut)) %>% 
  left_join(trophic_levels) %>% 
  mutate(species_tl = paste0(species, "\n(TL: ", round(tl,1), ")"))

# wrangle data for Fig 4a:
fish_mass_kgww_of_individual_fish <- read_csv("data/raw_data/fish_mass_kgww_of_individual_fish.csv")
mean_kg_per_species = fish_mass_kgww_of_individual_fish %>% pivot_longer(cols = c(-Year, -units, -Source)) %>% 
  filter(Year >= 1975) %>%
  mutate(name = str_remove(name, " salmon")) %>% 
  separate(name, c("location", "species"), sep = "_") %>% 
  group_by(species) %>% 
  summarize(mean_kg_ind = mean(value, na.rm = T))


all_chem_posts <- readRDS("posteriors/all_chem_posts.rds") 

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
  summarize(mean_size_kg = mean(value))


species_ind_average = all_chem_posts %>%
  left_join(mean_fish_size) %>%
  select(species, .draw, .epred, chemical, mean_size_kg) %>%
  pivot_wider(names_from = chemical, values_from = .epred) %>%
  mutate(cont_total_mgperkg = DDTs + Hg + PCBs + PBDEs,
         nut_total_mgperkg = DHA + N + P + EPA,
         cont_total_mgperfish = (DDTs + Hg + PCBs + PBDEs)*mean_size_kg,
         nut_total_mgperfish = (DHA + N + P + EPA)*mean_size_kg,
         cont_total_mgperfish = cont_total_mgperfish,
         nut_total_kgperfish = nut_total_mgperfish/1e6,
         ratio_mgperfish = nut_total_mgperfish/cont_total_mgperfish,
         ratio_kgmgperfish = nut_total_kgperfish/cont_total_mgperfish) %>%
  left_join(trophic_levels) %>%
  mutate(species_tl = paste0(species, "\n(TL: ", round(tl,1), ")")) %>% 
  mutate(panel = "a)")
# 
saveRDS(species_ind_average, file = "posteriors/derived_quantities/species_ind_average.rds")

species_ind_average = readRDS(file = "posteriors/derived_quantities/species_ind_average.rds") 

# combine
fig4_data = bind_rows(diff_props, species_ind_average)

write_csv(fig4_data, file = "plots/fig4_data.csv")


# Figure 5 ----------------------------------------------------------------

fig5_data = all_chem_posts %>% 
  select(-type) %>%
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
  mutate(species_tl = paste0(species, "\n(TL: ", round(tl,1), ")")) %>%  
  mutate(tl = case_when(species == "Sockeye" ~ tl - 0.1,
                        TRUE ~ tl))

write_csv(fig5_data, file = "plots/fig5_data.csv")

# Figure ED1 --------------------------------------------------------------
# data for panel b (Do b first becase a can be added from b)
gam_salmon_posts <- readRDS(file = "posteriors/gam_salmon_posts.rds")
all_chem_posts <- readRDS("posteriors/all_chem_posts.rds") 

species_order = gam_salmon_posts %>% ungroup %>% distinct(species) %>% 
  mutate(species_order = as.factor(species),
         species_order = fct_relevel(species_order, "Pink", "Sockeye", "Chum", "Chinook", "Coho"))

chem_order = all_chem_posts %>% ungroup %>% distinct(chemical) %>% 
  mutate(chem_order = as.factor(chemical),
         chem_order = fct_relevel(chem_order, "N", "P", "DHA", "EPA", "Hg", "PCBs", "DDTs"))

fig_ed1b_lines <- gam_salmon_posts %>% 
  select(-metric_tons) %>% 
  #calculate total Hg and kg 
  pivot_wider(names_from = location, values_from = kg) %>%
  mutate(Total = BCWC + BeringSea + CentralAK + SEAK) %>% 
  pivot_longer(cols = c(-species, -year, -.draw), names_to = "location", values_to = "kg") %>%
  mutate(metric_tons = kg*0.001) %>% 
  left_join(species_order) %>% 
  mutate(panel = "b) Species returns",
         type = "lines")

d_short = d_short %>% mutate(metric_tons = mt_escape)

d_short_totals = d_short %>% group_by(species, year) %>% reframe(metric_tons = sum(metric_tons)) %>% 
  mutate(location = "Total")

fig_ed1b_dots = bind_rows(d_short, d_short_totals) %>% 
  left_join(species_order) %>% 
  mutate(panel = "b) Species returns",
         type = "dots")


# data for panel a

fig_ed1a_lines = fig_ed1b_lines %>% 
  group_by(location, year, .draw) %>% 
  reframe(metric_tons = sum(metric_tons),
          kg = sum(kg)) %>% 
  mutate(panel = "a) Total returns",
         type = "lines")

fig_ed1a_dots = fig_ed1b_dots %>% 
  group_by(year, location) %>% 
  reframe(metric_tons = sum(metric_tons)) %>% 
  mutate(panel = "a) Total returns",
         type = "dots")

# make all data for a and b
       
location_fix = fig_ed1a_dots %>% distinct(location) %>%
  mutate(fixed_location = case_when(location == "Total" ~ "All Regions",
                              location == "BeringSea" ~ "Bering Sea",
                              TRUE ~ location)) %>% 
  mutate(fixed_location = as.factor(fixed_location),
         fixed_location = fct_relevel(fixed_location, "All Regions", "SEAK", "Bering Sea", "BCWC"))

fig_ed1a_data = bind_rows(fig_ed1a_lines, fig_ed1a_dots) %>% left_join(location_fix) %>% select(-location) %>% rename(location = fixed_location)
fig_ed1b_data = bind_rows(fig_ed1b_lines, fig_ed1b_dots) %>% left_join(location_fix) %>% select(-location) %>% rename(location = fixed_location)
fig_ed1cd_lines = all_chem_posts %>% left_join(species_order) %>% mutate(type = "lines") %>% 
  mutate(chem_type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "Nutrients",
                          TRUE ~ "Contaminants")) %>% 
  left_join(chem_order) %>% 
  left_join(species_order)
fig_ed1cd_dots = read_csv("data/nut_cont.csv") %>% select(species, mean_concentration_standardized, chemical) %>% 
  mutate(chem_type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "Nutrients",
                          TRUE ~ "Contaminants")) %>% 
  left_join(species_order) %>% 
  left_join(chem_order) %>% 
  mutate(type = "dots")

fig_ed1cd_data = bind_rows(fig_ed1cd_lines, fig_ed1cd_dots) %>% 
  mutate(panel = case_when(chem_type =="Contaminants" ~ "d) Contaminants",
                           TRUE ~ "c) Nutrients"))

fig_ed1_data = list(fig_ed1a_data, fig_ed1b_data, fig_ed1cd_data)
saveRDS(fig_ed1_data, file = "plots/fig_ed1_data.rds")



# Figure ED2 --------------------------------------
flux_predictions <- readRDS(file = "posteriors/flux_predictions.rds") # posterior chem export

chem_species_prop <- flux_predictions %>% 
  # filter(.draw <= 2) %>% 
  select(species, location, year, .draw, mg_flux, chemical) %>% 
  pivot_wider(names_from = species, values_from = mg_flux) %>% 
  mutate(All = Chinook + Chum + Coho + Pink + Sockeye) %>% 
  pivot_longer(cols = c(Chinook, Chum, Coho, Pink, Sockeye), names_to = "species") %>% 
  mutate(proportion = value/All)

saveRDS(chem_species_prop, file = "posteriors/chem_species_prop.rds")

chem_species_location_prop_summary <- chem_species_prop %>% 
  ungroup() %>%
  group_by(chemical, year, species, location) %>% 
  mutate(total = proportion) %>% 
  summarize(median = median(total),
            low75 = quantile(total, probs = 0.125),
            high75 = quantile(total, probs = 1-0.125),
            low50 = quantile(total, probs = 0.25),
            high50 = quantile(total, probs = 1-0.25)) %>% 
  ungroup() %>% 
  mutate(type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "Nutrients",
                          TRUE ~ "Contaminants"),
         chemical = fct_relevel(chemical, "N", 
                                "P", 
                                "DHA", 
                                "EPA",
                                "Hg",
                                "DDT"),
         group = case_when(species == "All" ~ "All",
                           TRUE ~ "Species"),
         units = "kg") %>%
  mutate(species = fct_relevel(species, "Coho", "Chinook", "Chum", "Sockeye", "Pink"),
         type = fct_relevel(type, "Contaminants"))


chem_species_location_total_summary <- chem_species_prop %>% 
  ungroup() %>%
  group_by(chemical, year, species, location) %>% 
  mutate(total = value) %>% 
  summarize(median = median(total),
            low75 = quantile(total, probs = 0.125),
            high75 = quantile(total, probs = 1-0.125),
            low50 = quantile(total, probs = 0.25),
            high50 = quantile(total, probs = 1-0.25)) %>% 
  ungroup() %>% 
  mutate(type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "Nutrients",
                          TRUE ~ "Contaminants"),
         chemical = case_when(chemical == "PCBS" ~ "PCBs",
                              TRUE ~ chemical),
         group = case_when(species == "All" ~ "All",
                           TRUE ~ "Species"),
         units = "kg") %>%
  mutate(species = as.factor(species))

write_csv(chem_species_location_total_summary, file = "plots/fig_ed2_data.csv")
