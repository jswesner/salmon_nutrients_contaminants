library(tidyverse)



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
                                "DDT")) # Salmon chemical concentrations mg kg wet mass


# biomass_chem_medians

biomass_chem_medians <- all_chem_posts %>%
  pivot_wider(names_from = species, values_from = .epred) %>% 
  mutate(sumfish_meanchem = (Chinook + Chum + Coho + Pink + Sockeye)/5) %>% # mean chem concentrations across species
  pivot_longer(cols = c(-.draw, -chemical, -units),
               names_to = "species", values_to = ".epred") %>% 
  group_by(species, chemical) %>% 
  summarize(median_chem_mgkg = median(.epred)) %>% 
  pivot_wider(names_from = chemical, values_from = median_chem_mgkg) %>% 
  right_join(gam_salmon_posts %>%
               select(-kg) %>% 
               pivot_wider(names_from = species, values_from = metric_tons) %>% 
               mutate(sumfish_meanchem = (Chinook + Chum + Coho + Pink + Sockeye)) %>% # mean chem concentrations across species
               pivot_longer(cols = c(-.draw, -location, -year),
                            names_to = "species", values_to = "metric_tons") %>%
               group_by(species, year, .draw) %>% 
               summarize(total_mt = sum(metric_tons)) %>%  # total for species per year - summed across regions
               group_by(species) %>% 
               summarize(median_mt = median(total_mt)) %>% # median averaged across years
               select(species, median_mt, everything())) %>% 
  select(species, median_mt, N, P, DHA, EPA, Hg, everything()) %>% 
  arrange(median_mt)


write_csv(biomass_chem_medians, file = "tables/biomass_chem_medians.csv")


# chemical flux -----------------------------------------------------------

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
  select(-mean, -sd) %>%
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
  select(-mean, -sd) %>%
  pivot_wider(names_from = species, values_from = mean_sd) %>% 
  arrange(desc(type), desc(Total)) %>% 
  mutate(units = case_when(type == "Nutrients" ~ "MT", 
                           TRUE ~ "Kg"),
         summary = "Cumulative")

mean_cumulative_flux <-
  bind_rows(mean_sd_annual_flux, cumulative_sd_annual_flux)

write_csv(mean_cumulative_flux, file = "tables/mean_cumulative_flux.csv")

