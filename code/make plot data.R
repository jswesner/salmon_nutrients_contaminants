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
library(patchwork)

#raw data
nut_cont <- readRDS("data/nut_cont.rds")   # nutrient and contaminant concentrations mg/kg
d_short <- readRDS("data/d_short.rds") %>% 
  separate(species, c("species", "family")) # salmon escapement after 1975
trophic_levels <- tibble(tl = c(4.3, 3.9, 3.9, 3.6, 3.5),
                         species = c("Chinook", "Sockeye", "Coho",
                                     "Chum", "Pink"),
                         source = "Qin, Y., & Kaeriyama, M. (2016). Feeding habits and trophic levels of Pacific salmon (Oncorhynchus spp.) in the North Pacific Ocean. N Pac Anadromous Fish Com Bul, 6, 469-481.")

#posteriors
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



# 1 -----------------------------------------------------------------------


nut_cont_plot_data <- flux_predictions %>% 
  mutate(type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "Nutrients",
                          TRUE ~ "Contaminants")) %>%
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
  mutate(type = fct_relevel(type, "Nutrients"))


nut_cont_twopanel <- nut_cont_plot_data %>% 
  ggplot(aes(x = year, y = median)) + 
  geom_line() +
  geom_ribbon(aes(ymin = low75, ymax = high75), alpha = 0.25) + 
  geom_ribbon(aes(ymin = low50, ymax = high50), alpha = 0.35) +
  facet_wrap(~type, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  labs(y = "Total Biotransport (kg/y)", 
       x = "Year")

nut_twopanel <- nut_cont_plot_data %>% 
  filter(type == "Nutrients") %>% 
  ggplot(aes(x = year, y = median)) + 
  geom_line() +
  geom_ribbon(aes(ymin = low75, ymax = high75), alpha = 0.25) + 
  geom_ribbon(aes(ymin = low50, ymax = high50), alpha = 0.35) +
  # facet_wrap(~type, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  labs(y = "Total Biotransport (kg/y)", 
       x = "Year") +
  scale_x_continuous(breaks = c(1975, 1995, 2015))


cont_twopanel <- nut_cont_plot_data %>% 
  filter(type != "Nutrients") %>%   
  ggplot(aes(x = year, y = median)) + 
  geom_line() +
  geom_ribbon(aes(ymin = low75, ymax = high75), alpha = 0.25) + 
  geom_ribbon(aes(ymin = low50, ymax = high50), alpha = 0.35) +
  # facet_wrap(~type, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  labs(y = "Total Biotransport (kg/y)", 
       x = "Year") +
  scale_x_continuous(breaks = c(1975, 1995, 2015))

nut_cont_species_plot_data <- flux_predictions %>% 
  mutate(type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "Nutrients",
                          TRUE ~ "Contaminants")) %>%
  ungroup() %>%
  group_by(year, .draw, type, species) %>%
  summarize(total = sum(mg_flux/1000000)) %>%
  group_by(year, type, species) %>%
  summarize(median = median(total),
            low75 = quantile(total, probs = 0.125),
            high75 = quantile(total, probs = 1-0.125),
            low50 = quantile(total, probs = 0.25),
            high50 = quantile(total, probs = 1-0.25)) %>%
  ungroup() %>% 
  mutate(type = fct_relevel(type, "Nutrients")) 

nut_twopanel_species <- nut_cont_species_plot_data %>% 
  filter(type == "Nutrients") %>% 
  ggplot(aes(x = year, y = median))  +
  # geom_ribbon(aes(ymin = low75, ymax = high75, fill = species), alpha = 0.25) +
  geom_ribbon(aes(ymin = low50, ymax = high50, fill = species), alpha = 0.65) + 
  geom_line(aes(group = species)) +
  # geom_ribbon(data = nut_cont_plot_data %>% mutate(species = "Total"),
  #             aes(ymin = low75, ymax = high75), fill = "black", alpha = 0.2) +
  # geom_line(data = nut_cont_plot_data %>% mutate(species = "Total"),
  #           aes(y = median), fill = "black", alpha = 0.5) +
  # facet_wrap(~type, scales = "free_y") +
  # scale_y_continuous(labels = comma) +
  labs(y = "Total Biotransport (kg/y)", 
       x = "Year") +
  scale_color_viridis_d(direction = 1, option = 'A', end = 0.7) +
  scale_fill_viridis_d(direction = 1, option = 'A', alpha = 0.5) +
  guides(color = "none",
         fill = "none") +
  # scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015), 
                     limits = c(1975, 2025)) +
  scale_y_continuous(labels = comma) +
  geom_text_repel(data = . %>% filter(year == 2015) %>% 
                    distinct(year, median, species, type), 
                  aes(x = 2015,
                      y = median, 
                      # color = species,
                      label = species),
                  # color = "#FCFDBFFF",
                  nudge_x = 7,
                  size = 3,
                  hjust = 0,
                  segment.color = "black",
                  segment.linetype = "dashed"
  )


cont_twopanel_species <- nut_cont_species_plot_data %>% 
  filter(type != "Nutrients") %>% 
  ggplot(aes(x = year, y = median))  +
  # geom_ribbon(aes(ymin = low75, ymax = high75, fill = species), alpha = 0.25) +
  geom_ribbon(aes(ymin = low50, ymax = high50, fill = species), alpha = 0.65) + 
  geom_line(aes(group = species)) +
  # geom_ribbon(data = nut_cont_plot_data %>% mutate(species = "Total"),
  #             aes(ymin = low75, ymax = high75), fill = "black", alpha = 0.2) +
  # geom_line(data = nut_cont_plot_data %>% mutate(species = "Total"),
  #           aes(y = median), fill = "black", alpha = 0.5) +
  # facet_wrap(~type, scales = "free_y") +
  # scale_y_continuous(labels = comma) +
  labs(y = "Total Biotransport (kg/y)", 
       x = "Year") +
  scale_color_viridis_d(direction = 1, option = 'A', end = 0.7) +
  scale_fill_viridis_d(direction = 1, option = 'A', alpha = 0.5) +
  guides(color = "none",
         fill = "none") +
  # scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015), limits = c(1975, 2025)) +
  scale_y_continuous(labels = comma) +
  geom_text_repel(data = . %>% filter(year == 2015) %>% 
                    distinct(year, median, species, type), 
                  aes(x = 2015,
                      y = median, 
                      # color = species,
                      label = species),
                  # color = "#FCFDBFFF",
                  nudge_x = 7,
                  size = 3,
                  hjust = 0,
                  segment.color = "black",
                  segment.linetype = "dashed"
  )




d_region_toplot <- d_short %>% dplyr::select(year, species, location, y) %>% 
  pivot_wider(names_from = location, values_from = y) %>% 
  mutate(`All Regions` = BCWC + BeringSea + CentralAK + SEAK) %>% 
  pivot_longer(cols = c(-species, -year), names_to = "location") %>% 
  group_by(species, year, location) %>% 
  summarize(value = sum(value)) %>% 
  pivot_wider(names_from = species, values_from = value) %>% 
  mutate(Total = Chinook + Chum + Sockeye + Pink + Coho) %>% 
  pivot_longer(cols = c(Total, Chinook, Chum, Sockeye, Pink, Coho), names_to = "species") %>% 
  mutate(species = as.factor(str_trim(species)),
         location = fct_relevel(location, "All Regions", "SEAK", "BeringSea"),
         species = fct_relevel(species, "Pink", "Sockeye", "Chum"),
         group = case_when(species == "Total" ~ "All", TRUE ~ "Species"))

saveRDS(d_region_toplot, file = "plots/ms_plots/plot_data/d_region_toplot.rds")

salmon_mass <- gam_salmon_posts %>% 
  ungroup() %>% 
  dplyr::select(metric_tons, .draw, year, species, location) %>% 
  #calculate total Hg and kg 
  pivot_wider(names_from = species, values_from = metric_tons) %>% 
  mutate(Total = Chinook + Chum + Coho + Pink + Sockeye) %>% 
  pivot_longer(cols = c(-location, -year, -.draw), names_to = "species", values_to = "metric_tons") 


salmon_mass_region_toplot <- salmon_mass %>%
  group_by(species, year, .draw, location) %>% 
  summarize(metric_tons = sum(metric_tons)) %>% 
  pivot_wider(names_from = location, values_from = metric_tons) %>% 
  mutate(`All Regions` = BCWC + BeringSea + CentralAK + SEAK) %>% 
  pivot_longer(cols = c(-.draw, -species, -year), names_to = "location", values_to = "metric_tons") %>% 
  group_by(species, year, location) %>% 
  summarize(low = quantile(metric_tons, probs = 0.025, na.rm = T),
            med = median(metric_tons, na.rm = T),
            upper = quantile(metric_tons, probs = 0.975, na.rm = T),
            low50 = quantile(metric_tons, probs = 0.25, na.rm = T),
            upper50 = quantile(metric_tons, probs = 0.75, na.rm = T)) %>% 
  mutate(species = as.factor(str_trim(species)),
         location = fct_relevel(location, "All Regions", "SEAK", "BeringSea"),
         species = fct_relevel(species, "Pink", "Sockeye", "Chum"),
         group = case_when(species == "Total" ~ "All", TRUE ~ "Species"))


all_series_data = salmon_mass_region_toplot %>% 
  filter(species == "Total" & location == "All Regions") %>% 
  mutate(type = "Escapement",
         panel = "a) Total Escapement") %>% 
  rename(low75 = low,
         high75 = upper,
         median = med,
         high50 = upper50) %>% 
  bind_rows(nut_twopanel$data %>%  mutate(panel = "c) Total Nutrients", species = "Total"),
            cont_twopanel$data %>%  mutate(panel = "e) Total Contaminants", species = "Total"),
            nut_twopanel_species$data %>%  mutate(panel = "d) Species Nutrients"),
            cont_twopanel_species$data %>%  mutate(panel = "f) Species Contaminants"),
            salmon_mass_region_toplot %>% 
              filter(species != "Total" & location == "All Regions") %>% 
              mutate(type = "Escapement",
                     panel = "b) Species Escapement")%>% 
              rename(low75 = low,
                     high75 = upper,
                     median = med,
                     high50 = upper50)) 

saveRDS(all_series_data, file = "plots/ms_plots/plot_data/all_series_data.rds")


# 2 -----------------------------------------------------------------------
summary_all <- readRDS(file = "posteriors/derived_quantities/summary_all.rds")
summary_species_combined <- readRDS(file = "posteriors/derived_quantities/summary_species_combined.rds")
summary_location_combined <- readRDS(file = "posteriors/derived_quantities/summary_location_combined.rds")


total_series <- summary_all %>% 
  mutate(panel = "All")

species_series <- summary_species_combined %>% 
  rename(panel = species) %>% 
  bind_rows(total_series) %>% 
  mutate(group = "Species")

location_series <- summary_location_combined %>% 
  rename(panel = location) %>% 
  bind_rows(total_series) %>% 
  mutate(group = "Location") 


species_location_series <- species_series %>% bind_rows(location_series) %>% 
  mutate(group = fct_relevel(group, "Species"))
