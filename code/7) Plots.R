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

theme_set(theme_default()) 


# load plots --------------------------------------------------------------

proportion_contributions_species_locations <- readRDS( file = "plots/proportion_contributions_species_locations.rds")  
escapement_plot <- readRDS( file = "plots/escapement_plot.rds")
flux_fig_time <- readRDS( file = "plots/flux_fig_time.rds")
total_chem_plot <- readRDS( file = "plots/total_chem_plot.rds")
mean_chem_plot <- readRDS( file = "plots/mean_chem_plot.rds")
proportion_contributions_species_nutcont <- readRDS( file = "plots/proportion_contributions_species_nutcont.rds")
proportion_contributions_species <- readRDS( file = "plots/proportion_contributions_species.rds")  
proportion_contributions_region <- readRDS( file = "plots/proportion_contributions_region.rds")  
escapement_plus_concentrations_plot<- readRDS(file = "plots/escapement_plus_concentrations_plot.rds")
new_fig3 = readRDS(file = "plots/ms_plots/new_fig3.rds")
new_fig2 = readRDS(file = "plots/ms_plots/new_fig2.rds")

# re-create plots ---------------------------------------------------------


# load posteriors
flux_predictions <- readRDS(file = "posteriors/flux_predictions.rds") # posterior chem export
gam_salmon_posts <- readRDS("posteriors/gam_salmon_posts.rds") # Salmon escapement in kg wet mass
all_chem_posts <- readRDS("posteriors/all_chem_posts.rds") 


# load raw data
nut_cont <- readRDS("data/nut_cont.rds")   # nutrient and contaminant concentrations mg/kg
d_short <- readRDS("data/d_short.rds") %>% 
  separate(species, c("species", "family")) # salmon escapement after 1975
trophic_levels <- tibble(tl = c(4.3, 3.9, 3.9, 3.6, 3.5),
                         species = c("Chinook", "Sockeye", "Coho",
                                     "Chum", "Pink"),
                         source = "Qin, Y., & Kaeriyama, M. (2016). Feeding habits and trophic levels of Pacific salmon (Oncorhynchus spp.) in the North Pacific Ocean. N Pac Anadromous Fish Com Bul, 6, 469-481.")


# Salmon escapement time series -------------------------------------------
#raw data

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

#posterior predictions - estimate total salmon mass for species. Sum over locations
salmon_mass <- gam_salmon_posts %>% 
  ungroup() %>% 
  dplyr::select(metric_tons, .draw, year, species, location) %>% 
  #calculate total Hg and kg 
  pivot_wider(names_from = species, values_from = metric_tons) %>% 
  mutate(Total = Chinook + Chum + Coho + Pink + Sockeye) %>% 
  pivot_longer(cols = c(-location, -year, -.draw), names_to = "species", values_to = "metric_tons") 

saveRDS(salmon_mass, file = "posteriors/derived_quantities/salmon_mass.rds")

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

# total escapement
total_escapement <- salmon_mass_region_toplot %>% 
  filter(species == "Total") %>%
  ggplot(aes(x = year)) + 
  geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.4) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.8) +
  geom_line(aes(y = med)) +
  facet_grid(location ~ group, scales = "free_y") +
  labs(y = "Escapement: Metric tons per year",
       x = "Year") +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  scale_y_continuous(labels = comma) +
  # theme(text = element_text(size = 12),
  #       axis.title = element_text(size = 14)) +
  geom_point(data = d_region_toplot %>% 
               filter(species == "Total"),
             aes(y = value),
             size = 0.3) +
  scale_fill_colorblind() + 
  scale_color_colorblind() + 
  labs(y = "Escapement: Metric tons per year",
       subtitle = "a) Total Escapement") +
  theme(strip.text = element_blank(),
        text = element_text(size = 11),
        plot.subtitle = element_text(size = 11))

# species escapement
species_escapement <- salmon_mass_region_toplot %>% 
  filter(species != "Total") %>%
  ggplot(aes(x = year, fill = species)) + 
  geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.4) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.4) +
  geom_line(aes(y = med)) +
  facet_grid(location ~ ., scales = "free_y") +
  labs(y = "Escapement: Metric tons per year",
       x = "Year") +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  scale_y_continuous(labels = comma) +
  # theme(text = element_text(size = 12),
  #       axis.title = element_text(size = 14)) +
  geom_point(data = d_region_toplot %>% 
               filter(species != "Total"),
             aes(y = value, 
                 color = species),
             size = 0.3) + 
  labs(fill = "",
       color = "",
       y = "",
       subtitle = "b) Species Escapement") + 
  theme(legend.text=element_text(size=10),
        text = element_text(size = 11),
        plot.subtitle = element_text(size = 11)) +
  scale_color_viridis_d(option = "A",
                        direction = 1) +
  scale_fill_viridis_d(option = "A",
                       direction = 1) +
  guides(fill = guide_legend(override.aes = 
                                list(alpha = .8)))



legend_escapement <- get_legend(species_escapement)

escapement_plot <- plot_grid(total_escapement,
          species_escapement + guides(color = "none", fill = "none"),
          legend_escapement,
          ncol = 3,
          rel_widths = c(0.36, 0.43, 0.2))


saveRDS(escapement_plot, file = "plots/escapement_plot.rds")
ggsave(escapement_plot, file = "plots/escapement_plot.jpg", dpi = 400, width = 6, height = 8)

saveRDS(species_escapement, file = "plots/species_escapement.rds")

# Chemical Concentrations -------------------------------------------------
species_escapement = readRDS(file = "plots/species_escapement.rds")

chem_concentrations <- all_chem_posts%>% 
  mutate(chemical = as.character(chemical),
         chemical = case_when(chemical == "DDT" ~ "DDTs",
                              chemical == "PBDE" ~ "PBDEs",
                              TRUE ~ chemical)) %>% 
  mutate(type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "c) Nutrients",
                          TRUE ~ "d) Contaminants"),
         chemical = fct_relevel(chemical, "N",
                                "DHA", 
                                "EPA",
                                "P",
                                "Hg",
                                "DDTs"),
         species = fct_relevel(species, "Pink", "Sockeye", "Chum", "Chinook")) %>% 
  ggplot(aes(x = chemical, y = .epred)) +
  geom_boxplot(aes(fill = species),
              alpha = 0.7,
              outlier.shape = NA) +
  scale_y_log10() +
  facet_wrap(~type, ncol = 1, scales = "free") +
  geom_point(data = nut_cont %>% 
               filter(species != "All") %>% 
               mutate(chemical = fct_relevel(chemical, "N",
                                             "DHA", 
                                             "EPA",
                                             "P",
                                             "Hg",
                                             "DDTs"),
                      species = fct_relevel(species, "Pink", "Sockeye", "Chum", "Chinook"),
                      type = case_when(type == "nutrient" ~ "c) Nutrients",
                                       TRUE ~ "d) Contaminants")),
             position = position_dodge(width = 0.75), 
             aes(y = mean_concentration_standardized, group = species),
             size = 0.4) +
  # guides(fill = "none") +
  labs(y = "Whole body concentrations (mg/kg ww)",
       x = "Chemical") +
  theme(text = element_text(size = 11),
        plot.subtitle = element_text(size = 11),
        legend.title = element_blank()) +
  scale_color_viridis_d(direction = 1, option = 'A', alpha = 1,
                        begin = 0.15, end = 0.7) +
  scale_fill_viridis_d(direction = 1, option = 'A', alpha = 1,
                       begin = 0.15, end = 0.7)

legend_escapement_conc <- get_legend(chem_concentrations + theme(legend.position = "top"))

(escapement_plus_a <- plot_grid(total_escapement,
                                species_escapement +
                                  scale_color_viridis_d(direction = 1, option = 'A', alpha = 1,
                                                        begin = 0.15, end = 0.7) +
                                  scale_fill_viridis_d(direction = 1, option = 'A', alpha = 1,
                                                       begin = 0.15, end = 0.7) + 
                                  guides(color = "none", 
                                         fill = "none"),
                                chem_concentrations + 
                                  guides(color = "none", 
                                         fill = "none"),
                                ncol = 3,
                                rel_widths = c(0.36, 0.43, 0.55)))

(escapement_plus_concentrations_plot <- 
    plot_grid(legend_escapement_conc,
          escapement_plus_a,
          ncol = 1,
          rel_heights = c(0.1, 1)))

saveRDS(escapement_plus_concentrations_plot, file = "plots/ms_plots/fig_ed1.rds")
ggview::ggview(escapement_plus_concentrations_plot,
       dpi = 500, width = 6.5, height = 6, units = "in")
ggsave(escapement_plus_concentrations_plot, file = "plots/ms_plots/fig_ed1.pdf",
       dpi = 500, width = 6.5, height = 6, units = "in")
ggsave(escapement_plus_concentrations_plot, file = "plots/ms_plots/fig_ed1.jpg",
       dpi = 500, width = 6.5, height = 6, units = "in")


# Flux Time Series --------------------------------------------------------

# summarize and wrangle posteriors

summary_species_combined <- readRDS(file = "posteriors/derived_quantities/summary_species_combined.rds")
summary_location_combined <- readRDS(file = "posteriors/derived_quantities/summary_location_combined.rds")
summary_all <- readRDS(file = "posteriors/derived_quantities/summary_all.rds")

# total nutrients and contaminants

nut_cont_plot_data <- flux_predictions %>% 
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

# total nutrients and contaminants by species

nut_cont_species_plot_data <- flux_predictions %>%
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


nut_cont_twopanel_species <- nut_cont_species_plot_data %>% 
  ggplot(aes(x = year, y = median))  +
  # geom_ribbon(aes(ymin = low75, ymax = high75, fill = species), alpha = 0.25) +
  geom_ribbon(aes(ymin = low50, ymax = high50, fill = species), alpha = 0.65) + 
  geom_line(aes(group = species)) +
  # geom_ribbon(data = nut_cont_plot_data %>% mutate(species = "Total"),
  #             aes(ymin = low75, ymax = high75), fill = "black", alpha = 0.2) +
  # geom_line(data = nut_cont_plot_data %>% mutate(species = "Total"),
  #           aes(y = median), fill = "black", alpha = 0.5) +
  facet_wrap(~type, scales = "free_y") +
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
                  nudge_x = 8,
                  size = 3,
                  hjust = 0,
                  segment.color = "black",
                  segment.linetype = "dashed"
  )



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



# total escapement
total_escapement_only <- salmon_mass_region_toplot %>% 
  filter(species == "Total" & location == "All Regions") %>%
  ggplot(aes(x = year)) + 
  geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.25) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.25) +
  geom_line(aes(y = med)) +
  facet_grid(location ~ group, scales = "free_y") +
  labs(y = "Escapement: Metric tons per year",
       x = "Year") +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  scale_y_continuous(labels = comma) +
  # theme(text = element_text(size = 12),
  #       axis.title = element_text(size = 14)) +
  geom_point(data = d_region_toplot %>% 
               filter(species == "Total" & location == "All Regions"),
             aes(y = value),
             size = 0.3) +
  scale_fill_colorblind() + 
  scale_color_colorblind() + 
  labs(y = "Escapement: Metric tons per year") +
  theme(strip.text = element_blank(),
        text = element_text(size = 11),
        plot.subtitle = element_text(size = 11))

ggsave(total_escapement_only, file = "plots/total_escapement_only.jpg", width = 3.5, height = 2.7, dpi = 500)
ggsave(nut_cont_twopanel, file = "plots/nut_cont_twopanel.jpg", width = 6, height = 2.7, dpi = 500)

# 6-panel plot ------------------------------------------------------------

all_series_data = total_escapement_only$data %>% 
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
                     high50 = upper50) ) 


six_panel_totals = all_series_data %>% 
  filter(species == "Total") %>% 
  ggplot(aes(x = year, y = median))  + 
  geom_ribbon(aes(ymin = low75, ymax = high75, fill = species), alpha = 0.3) + 
  geom_line(aes(group = species), color = "black") +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = scientific) +
  scale_x_continuous(breaks = c(1975, 1995, 2015), limits = c(1975, 2015)) + 
  guides(fill = "none") +
  scale_fill_viridis_d(direction = 1, option = 'A', alpha = 0.5,
                       guide = guide_legend(reverse = TRUE) ) + 
  labs(y = "kg/y",
       x = "Year") +
  geom_point(data = d_region_toplot %>% 
               filter(species == "Total" & location == "All Regions") %>% 
               mutate(panel = "a) Total Escapement"),
             aes(y = value),
             size = 0.3) +
  geom_point(data = d_region_toplot %>% 
               filter(species == "Total" & location == "All Regions") %>% 
               mutate(panel = "a) Total Escapement"),
             aes(y = value),
             size = 0.3) +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2)) 



six_panel_species = all_series_data %>%
  filter(species != "Total") %>% 
  mutate(species = as_factor(species)) %>% 
  mutate(species = fct_relevel(species, "Coho", "Chinook","Chum", "Sockeye", "Pink" )) %>% 
  ggplot(aes(x = year, y = median))  + 
  geom_ribbon(aes(ymin = low75, ymax = high75, fill = species), alpha = 0.8) + 
  geom_line(aes(group = species), color = "black") +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) + 
  scale_fill_viridis_d(direction = 1, option = 'A', alpha = 1,
                       begin = 0.7, end = 0.15) +
  labs(y = "kg/y",
       x = "Year",
       fill = "Species") +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2)) + 
  guides(fill = guide_legend(override.aes = list(alpha = 1),
                             reverse = T)) +
  NULL


six_panel_cow = six_panel_totals + six_panel_species

# ggsave(six_panel_cow, file = "plots/ms_plots/new_fig1.pdf", dpi = 600, width = 9, height = 9)
# ggsave(six_panel_cow, file = "plots/ms_plots/new_fig1.jpg", dpi = 600, width = 9, height = 9)
# saveRDS(six_panel_cow, file = "plots/ms_plots/new_fig1.rds")

# re-do with MT for nutrients and kg for contaminants
a = six_panel_totals$data %>% 
  filter(grepl("a)", panel)) %>% 
  ggplot(aes(x = year, y = median))  + 
  geom_ribbon(aes(ymin = low75, ymax = high75), alpha = 0.5) + 
  geom_line(color = "black") +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) + 
  scale_fill_viridis_d(direction = 1, option = 'A', alpha = 1,
                       begin = 0.7, end = 0.15) +
  labs(y = "MT/y",
       x = "Year",
       fill = "Species") +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2)) + 
  guides(fill = "none") +
  NULL


b = six_panel_species$data %>% 
  filter(panel == "b) Species Escapement") %>% 
  filter(species != "Total") %>% 
  mutate(species = as_factor(species)) %>% 
  mutate(species = fct_relevel(species, "Coho", "Chinook","Chum", "Sockeye", "Pink" )) %>% 
  ggplot(aes(x = year, y = median))  + 
  geom_ribbon(aes(ymin = low75, ymax = high75, fill = species), alpha = 0.8) + 
  geom_line(aes(group = species), color = "black") +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) + 
  scale_fill_viridis_d(direction = 1, option = 'A', alpha = 1,
                       begin = 0.7, end = 0.15) +
  labs(y = "MT/y",
       x = "Year",
       fill = "Species") +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2)) + 
  guides(fill = "none") +
  NULL

c = six_panel_totals$data %>% 
  filter(grepl("c)", panel)) %>% 
  ggplot(aes(x = year, y = median/1000))  + 
  geom_ribbon(aes(ymin = low75/1000, 
                  ymax = high75/1000), alpha = 0.5) + 
  geom_line(color = "black") +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) + 
  scale_fill_viridis_d(direction = 1, option = 'A', alpha = 1,
                       begin = 0.7, end = 0.15) +
  labs(y = "MT/y",
       x = "Year",
       fill = "Species") +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2)) + 
  guides(fill = "none") +
  NULL

d = six_panel_cow$data %>% 
  filter(type == "Nutrients") %>%
  filter(species != "Total") %>% 
  mutate(species = as_factor(species)) %>% 
  mutate(species = fct_relevel(species, "Coho", "Chinook","Chum", "Sockeye", "Pink" )) %>% 
  ggplot(aes(x = year, y = median/1000))  + 
  geom_ribbon(aes(ymin = low75/1000, ymax = high75/1000, fill = species), alpha = 0.8) + 
  geom_line(aes(group = species), color = "black") +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) + 
  scale_fill_viridis_d(direction = 1, option = 'A', alpha = 1,
                       begin = 0.7, end = 0.15) +
  labs(y = "MT/y",
       x = "Year",
       fill = "Species") +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2)) + 
  guides(fill = guide_legend(override.aes = list(alpha = 1),
                             reverse = T)) +
  NULL

e = six_panel_totals$data %>% 
  filter(grepl("e)", panel)) %>% 
  ggplot(aes(x = year, y = median))  + 
  geom_ribbon(aes(ymin = low75, 
                  ymax = high75), alpha = 0.5) + 
  geom_line(color = "black") +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) + 
  scale_fill_viridis_d(direction = 1, option = 'A', alpha = 1,
                       begin = 0.7, end = 0.15) +
  labs(y = "kg/y",
       x = "Year",
       fill = "Species") +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2)) + 
  guides(fill = "none") +
  NULL

f = six_panel_cow$data %>% 
  filter(type == "Contaminants") %>%
  filter(species != "Total") %>% 
  mutate(species = as_factor(species)) %>% 
  mutate(species = fct_relevel(species, "Coho", "Chinook","Chum", "Sockeye", "Pink" )) %>% 
  ggplot(aes(x = year, y = median))  + 
  geom_ribbon(aes(ymin = low75, ymax = high75, fill = species), alpha = 0.8) + 
  geom_line(aes(group = species), color = "black") +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) + 
  scale_fill_viridis_d(direction = 1, option = 'A', alpha = 1,
                       begin = 0.7, end = 0.15) +
  labs(y = "kg/y",
       x = "Year",
       fill = "Species") +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2)) + 
  guides(fill = "none") +
  ylim(0, 5) +
  NULL

six_panel_cow_new = a/c/e | b/d/f

ggsave(six_panel_cow_new, file = "plots/ms_plots/new_fig1.pdf", dpi = 600, width = 9, height = 9)
ggsave(six_panel_cow_new, file = "plots/ms_plots/new_fig1.jpg", dpi = 600, width = 9, height = 9)
saveRDS(six_panel_cow_new, file = "plots/ms_plots/new_fig1.rds")




# make plots

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





# Totals by region --------------------------------------------------------


summary_location_combined <- flux_predictions %>% 
  ungroup() %>% 
  filter(species != "All") %>% 
  group_by(location, .draw, chemical) %>% 
  summarize(total_flux = sum(mg_flux/1000000)) %>% 
  mutate(type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "Nutrients",
                          TRUE ~ "Contaminants"),
         chemical = fct_relevel(chemical, "N", "Hg", "P", "DDT", "DHA", "PBDE", "EPA", "PCBs"),
         # group = case_when(species == "All" ~ "All",
         #                   TRUE ~ "Species"),
         units = "kg")

labels_6 <- tibble(letters = paste0(letters[1:8], ")")) %>% 
  mutate(chemical = c("N", "Hg", "P", "DDT", "DHA", "PBDE", "EPA", "PCBs"),
         label = paste0(letters, " ", chemical),
         x = "SEAK") %>% 
  mutate(chemical = fct_relevel(chemical, "N", "Hg", "P", "DDT", "DHA", "PBDE", "EPA", "PCBs"))

total_chem_plot <- summary_location_combined %>% 
  ggplot() +
  geom_boxplot(aes(x = location, y = total_flux/1000, fill = location),
               outlier.shape = NA) +
  # scale_y_log10(labels = scientific, expand = c(0.2, 0)) +
  # scale_x_discrete(expand = c(0.5,0)) +
  facet_wrap( ~ chemical, scales = "free_x", ncol = 2) + 
  scale_fill_grey(start = 0.4, end = 1) +
  guides(fill = F) +
  labs(y = "Cumulative Metric Tons (1976-2015)") +
  theme(axis.text.x = element_text(size = 11),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        text = element_text(size = 11),
        strip.text = element_text(size = 8),
        axis.title = element_text(size = 11)) +
  scale_y_log10() +
  coord_flip() +
  labs(subtitle = "Nutrients                                   Contaminants") +
  geom_text(data = labels_6, aes(x = x, y = 0, label = letters), check_overlap = TRUE,
            nudge_x = 0.5,
            nudge_y = 0,
            size = 3) 

saveRDS(total_chem_plot, file = "plots/total_chem_plot.rds")
ggsave(total_chem_plot, file = "plots/total_chem_plot.jpg", dpi = 400, width = 4, height = 8)

# Average by region --------------------------------------------------------

summary_location_combined_ave <- flux_predictions %>% 
  ungroup() %>% 
  filter(species != "All") %>% 
  group_by(location, .draw, chemical) %>% 
  summarize(mean_flux = mean(mg_flux/1000000)) %>% 
  mutate(type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "Nutrients",
                          TRUE ~ "Contaminants"),
         # chemical = fct_relevel(chemical, "N", "Hg", "P", "DDT", "DHA", "PBDE", "EPA", "PCBs"),
         # group = case_when(species == "All" ~ "All",
         #                   TRUE ~ "Species"),
         units = "kg",
         labels = case_when(chemical == "Hg" ~ "b) Hg",
                            chemical == "DDT" ~ "f) DDT",
                            chemical == "PBDE" ~ "h) PBDE",
                            chemical == "PCBs" ~ "d) PCBs",
                            chemical == "N" ~ "a) N",
                            chemical == "P" ~ "g) P",
                            chemical == "DHA" ~ "c) DHA",
                            TRUE ~ "e) EPA")) %>% 
  group_by(location, chemical) %>% 
  mutate(order = max(mean_flux))

mean_chem_plot_nutrients <- summary_location_combined_ave %>% 
  filter(type == "Nutrients") %>%
  ggplot() +
  geom_boxplot(aes(y = reorder(location, order), x = mean_flux/1000, 
                   fill = reorder(location, order)),
               outlier.shape = NA) +
  # scale_y_log10(labels = scientific, expand = c(0.2, 0)) +
  # scale_x_discrete(expand = c(0.5,0)) +
  facet_wrap(~labels, 
             # scales = "free_x", 
             ncol = 1) + 
  scale_fill_grey(start = 1, end = 0.4) +
  guides(fill = F) +
  labs(x = "kg in thousands (1976-2015)",
       subtitle = "Nutrients",
       y = "") +
  # scale_x_log10() +
  theme_classic() +
  xlim(0, 800) +
  NULL

mean_chem_plot_conts <- summary_location_combined_ave %>% 
  filter(type != "Nutrients") %>% 
  ggplot() +
  geom_boxplot(aes(y = reorder(location, order), x = mean_flux, 
                   fill = reorder(location, order)),
               outlier.shape = NA) +
  # scale_y_log10(labels = scientific, expand = c(0.2, 0)) +
  # scale_x_discrete(expand = c(0.5,0)) +
  facet_wrap(~labels, 
             # scales = "free_x", 
             ncol = 1) + 
  scale_fill_grey(start = 1, end = 0.4) +
  guides(fill = F) +
  labs(x = "kg (1976-2015)",
       subtitle = "Contaminants") +
  # scale_x_log10() +
  xlim(0, 1) +
  theme_classic() +
  NULL

(mean_chem_plot <- plot_grid(mean_chem_plot_nutrients,
          mean_chem_plot_conts + theme(axis.text.y = element_blank(),
                                         axis.title.y = element_blank()),
          rel_widths = c(1, 0.75)))


saveRDS(mean_chem_plot, file = "plots/mean_chem_plot.rds")
ggsave(mean_chem_plot, file = "plots/mean_chem_plot.jpg", dpi = 400, width = 6, height = 8)


# Species proportional contaminant/nutrient --------------------------------------

chem_species_prop_nutcont <- flux_predictions %>%
  filter(.draw <= 500) %>%
  select(species, location, year, .draw, mg_flux, chemical, type) %>%
  group_by(species, year, .draw, type) %>%
  summarize(mg_flux = sum(mg_flux)) %>%
  pivot_wider(names_from = species, values_from = mg_flux) %>%
  mutate(All = Chinook + Chum + Coho + Pink + Sockeye) %>%
  pivot_longer(cols = c(Chinook, Chum, Coho, Pink, Sockeye), names_to = "species") %>%
  mutate(proportion = value/All)




chem_species_total_summary_nutcont <- chem_species_prop_nutcont %>% 
  ungroup() %>%
  group_by(species, year, type) %>% 
  mutate(total = value) %>% 
  summarize(median = median(total),
            low75 = quantile(total, probs = 0.125),
            high75 = quantile(total, probs = 1-0.125),
            low50 = quantile(total, probs = 0.25),
            high50 = quantile(total, probs = 1-0.25)) %>% 
  ungroup() %>% 
  mutate(group = case_when(species == "All" ~ "All",
                           TRUE ~ "Species"),
         units = "kg") %>%
  mutate(species = fct_relevel(species, "Coho", "Chinook", "Chum", "Sockeye", "Pink"),
         type = fct_relevel(type, "Contaminants"))


# Species proportional contributions --------------------------------------
chem_species_prop <- readRDS("posteriors/derived_quantities/chem_species_prop.rds")



chem_species_prop_summary <- chem_species_prop %>% 
  ungroup() %>%
  group_by(chemical, year, species) %>% 
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
         units = "kg")


chem_species_total_summary <- chem_species_prop %>% 
  ungroup() %>%
  group_by(chemical, year, species) %>% 
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



# New Fig 2 ---------------------------------------------------------------

a_new = species_location_series %>%
  filter(type != "Contaminants" & group != "Location") %>% 
  filter(panel != "All") %>%
  mutate(facet_label = case_when(chemical == "N" ~ "a) N",
                                 chemical == "P" ~ "b) P",
                                 chemical == "DHA" ~ "c) DHA",
                                 TRUE ~ "d) EPA")) %>% 
  mutate(panel = fct_relevel(panel, "Pink", "Sockeye", "Chum", "Chinook")) %>% 
  ggplot(aes(x = year, y = median)) +
  geom_linerange(aes(ymin = low75, ymax = high75, color = panel), alpha = 0.5,
                 size = .6) +
  geom_point(size = 0.2, aes(color = panel)) +
  scale_color_viridis_d(direction = -1, option = 'A', alpha = 1,
                        begin = 0.7, end = 0.15) +
  scale_fill_viridis_d(direction = -1, option = 'A', alpha = 1,
                       begin = 0.7, end = 0.15) +
  guides(color = "none",
         fill = "none") +
  labs(y = "Total Biotransport \n(kg/y)",
       subtitle = "Nutrients") +
  facet_wrap(~ facet_label, ncol = 4) +
  scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  theme_default() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2)) +
  NULL

nutrient_props = chem_species_total_summary %>%
  mutate(facet_label = case_when(chemical == "N" ~ "e) N",
                                 chemical == "P" ~ "f) P",
                                 chemical == "DHA" ~ "g) DHA",
                                 TRUE ~ "h) EPA")) %>% 
  filter(type == "Nutrients") %>% 
  ggplot(aes(x = year, y = median)) + 
  geom_area(position = "fill", aes(fill = species)) +
  facet_wrap( ~ facet_label, nrow = 1) +
  scale_fill_viridis_d(direction = -1, option = 'A', alpha = 1,
                       begin = 0.15, end = 0.7) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  coord_cartesian(ylim = c(0, 1.1)) +
  labs(y = "Proportional \nBiotransport",
       fill = "Species",
       x = "Year") +
  theme_default()  + 
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())


# contaminants
b_new = species_location_series %>%
  filter(type == "Contaminants" & group != "Location") %>% 
  filter(panel != "All") %>%
  mutate(facet_label = case_when(chemical == "Hg" ~ "i) Hg",
                                 chemical == "PCBs" ~ "j) PCBs",
                                 chemical == "DDT" ~ "k) DDTs",
                                 TRUE ~ "l) PBDEs")) %>% 
  mutate(panel = fct_relevel(panel, "Pink", "Sockeye", "Chum", "Chinook")) %>% 
  ggplot(aes(x = year, y = median)) +
  geom_linerange(aes(ymin = low75, ymax = high75, color = panel), alpha = 0.5,
                 size = .6) +
  geom_point(size = 0.2, aes(color = panel)) +
  scale_color_viridis_d(direction = -1, option = 'A', alpha = 1,
                        begin = 0.7, end = 0.15) +
  scale_fill_viridis_d(direction = -1, option = 'A', alpha = 1,
                       begin = 0.7, end = 0.15) +
  guides(color = "none",
         fill = "none") +
  labs(y = "Total Biotransport \n(kg/y)",
       subtitle = "Contaminants") +
  facet_wrap(~ facet_label, ncol = 4) +
  scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  theme_default() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2)) +
  NULL

cont_props = chem_species_total_summary %>%
  mutate(facet_label = case_when(chemical == "Hg" ~ "m) Hg",
                                 chemical == "PCBs" ~ "n) PCBs",
                                 chemical == "DDT" ~ "o) DDTs",
                                 TRUE ~ "p) PBDEs"))  %>% 
  filter(type != "Nutrients") %>% 
  ggplot(aes(x = year, y = median)) + 
  geom_area(position = "fill", aes(fill = species)) +
  facet_wrap( ~ facet_label, nrow = 1) +
  scale_fill_viridis_d(direction = -1, option = 'A', alpha = 1,
                       begin = 0.15, end = 0.7) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  coord_cartesian(ylim = c(0, 1.1)) +
  labs(y = "Proportional \nBiotransport",
       fill = "Species",
       x = "Year") +
  theme_default()  + 
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2))

# nutrients + contaminants

new_fig2 = a_new/nutrient_props/b_new/cont_props + plot_layout(guides = "collect")


# fix MT and kg
a_new = a_new$data %>% 
  ggplot(aes(x = year, y = median/1000)) +
  geom_linerange(aes(ymin = low75/1000, 
                     ymax = high75/1000, color = panel), alpha = 0.5,
                 size = .6) +
  geom_point(size = 0.2, aes(color = panel)) +
  scale_color_viridis_d(direction = -1, option = 'A', alpha = 1,
                        begin = 0.7, end = 0.15) +
  scale_fill_viridis_d(direction = -1, option = 'A', alpha = 1,
                       begin = 0.7, end = 0.15) +
  guides(color = "none",
         fill = "none") +
  labs(y = "Total Biotransport \n(MT/y)",
       subtitle = "Nutrients") +
  facet_wrap(~ facet_label, ncol = 4) +
  scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  theme_default() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2)) +
  NULL

new_fig2 = a_new/nutrient_props/b_new/cont_props + plot_layout(guides = "collect")

ggsave(new_fig2, file = "plots/ms_plots/new_fig2.pdf", dpi = 600, width = 8, height = 8)
ggsave(new_fig2, file = "plots/ms_plots/new_fig2.jpg", dpi = 600, width = 8, height = 8)
saveRDS(new_fig2, file = "plots/ms_plots/new_fig2.rds")



# New Fig 3 ---------------------------------------------------------------

nut_location = species_location_series %>% 
  filter(group == "Location" & panel != "All") %>%
  mutate(facet_label = case_when(chemical == "N" ~ "a) N",
                                 chemical == "P" ~ "b) P",
                                 chemical == "DHA" ~ "c) DHA",
                                 chemical == "EPA" ~ "d) EPA",
                                 chemical == "Hg" ~ "e) Hg",
                                 chemical == "PCBs" ~ "f) PCBs",
                                 chemical == "DDT" ~ "g) DDTs",
                                 TRUE ~ "h) PBDEs")) %>% 
  filter(type == "Nutrients") %>% 
  mutate(panel = case_when(panel == "BeringSea" ~ "BS",
                           panel == "CentralAK" ~ "CAK",
                           TRUE ~ panel),
         panel = fct_relevel(panel, "BCWC", "SEAK", "CAK", "BS")) %>% 
  ggplot(aes(x = year, y = median/1000)) +
  # geom_linerange(aes(ymin = low75, ymax = high75), alpha = 1,
  #                size = .5) +
  geom_ribbon(aes(ymin = low50/1000, ymax = high50/1000, fill = panel), alpha = 0.25) +
  geom_line(aes(color = panel), linewidth = 1.2) +
  # scale_color_colorblind() +
  # scale_color_brewer(type = "qual", palette = 3) +
  scale_color_viridis_d(direction = 1, option = 'E') +
  scale_fill_viridis_d(direction = 1, option = 'E') +
  # guides(color = "none",
  #        fill = guide_legend(override.aes = list(size = 7))) +
  labs(y = "Total Biotransport (MT/y)",
       subtitle = "Nutrients",
       color = "Region",
       fill = "Region") +
  facet_wrap(~ facet_label, ncol = 4) +
  scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015),
                     limits = c(1975, 2030)) +
  theme_default() +
  theme(axis.title.x = element_blank(),
        strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2)) +
  NULL

cont_location = species_location_series %>% 
  filter(group == "Location" & panel != "All") %>%
  mutate(facet_label = case_when(chemical == "N" ~ "a) N",
                                 chemical == "P" ~ "b) P",
                                 chemical == "DHA" ~ "c) DHA",
                                 chemical == "EPA" ~ "d) EPA",
                                 chemical == "Hg" ~ "e) Hg",
                                 chemical == "PCBs" ~ "f) PCBs",
                                 chemical == "DDT" ~ "g) DDTs",
                                 TRUE ~ "h) PBDEs")) %>% 
  filter(type != "Nutrients") %>% 
  mutate(panel = case_when(panel == "BeringSea" ~ "BS",
                           panel == "CentralAK" ~ "CAK",
                           TRUE ~ panel),
         panel = fct_relevel(panel, "BCWC", "SEAK", "CAK", "BS")) %>% 
  ggplot(aes(x = year, y = median)) +
  geom_ribbon(aes(ymin = low50, ymax = high50, fill = panel), alpha = 0.25) +
  geom_line(aes(color = panel), size = 1.2) +
  scale_color_viridis_d(direction = 1, option = "E") +
  scale_fill_viridis_d(direction = 1, option = "E") +
  # scale_color_brewer(type = "qual", palette = 3) +
  # guides(fill = guide_legend(override.aes = list(size = 7))) +
  labs(y = "Total Biotransport (kg/y)",
       subtitle = "Contaminants",
       color = "Region",
       fill = "Region") +
  facet_wrap(~ facet_label, ncol = 4) +
  scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015),
                     limits = c(1975, 2030)) +
  theme_default() +
  theme(axis.title.x = element_blank(),
        strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2)) +
  NULL


new_fig3 = nut_location / cont_location + plot_layout(guides = "collect") 

ggsave(new_fig3, file = "plots/ms_plots/new_fig3.jpg", dpi = 600, width = 8, height = 4.5)
ggsave(new_fig3, file = "plots/ms_plots/new_fig3.pdf", dpi = 600, width = 8, height = 4.5)
saveRDS(new_fig3, file = "plots/ms_plots/new_fig3.rds")

# Regional proportional contributions --------------------------------------

chem_region_prop <- readRDS("posteriors/derived_quantities/chem_region_prop.rds")

# chem_region_prop <- flux_predictions %>% 
#   filter(.draw <= 500) %>%
#   select(species, location, year, .draw, mg_flux, chemical) %>% 
#   pivot_wider(names_from = location, values_from = mg_flux) %>% 
#   mutate(All = BeringSea + BCWC + CentralAK + SEAK) %>% 
#   pivot_longer(cols = c(BeringSea, BCWC, CentralAK, SEAK), names_to = "region") %>% 
#   mutate(proportion = value/All)

chem_region_prop_summary <- chem_region_prop %>% 
  ungroup() %>%
  group_by(chemical, year, region) %>% 
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
         group = case_when(region == "All" ~ "All",
                           TRUE ~ "Region"),
         units = "kg")


chem_region_total_summary <- chem_region_prop %>% 
  ungroup() %>%
  group_by(chemical, year, region) %>% 
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
         chemical = fct_relevel(chemical, "N", 
                                "P", 
                                "DHA", 
                                "EPA",
                                "Hg",
                                "DDT"),
         group = case_when(region == "All" ~ "All",
                           TRUE ~ "Region"),
         units = "kg") %>%
  mutate(region = fct_relevel(region, "BeringSea", "BCWC", "CentralAK", "SEAK"),
         type = fct_relevel(type, "Contaminants"))

(proportion_contributions_region <- tag_facet(chem_region_total_summary %>% 
                                                 ggplot(aes(x = year, y = median)) + 
                                                 geom_area(position = "fill", aes(fill = region)) +
                                                 facet_wrap( ~chemical, nrow = 2) +
                                                 scale_fill_grey(start = 0.9, end = 0.2) +
                                                 scale_x_continuous(breaks = c(1980, 1995, 2010)) +
                                                 scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
                                                 coord_cartesian(ylim = c(0, 1.1)) +
                                                 labs(y = "Median proportional contributions to\nnutrient and contaminant flux",
                                                      fill = "Region",
                                                      x = "Year") +
                                                 geom_text(data = chem_species_total_summary %>% distinct(chemical, type, region = NA),
                                                           aes(x = 1975 + 12, y = 1.08, label = chemical),
                                                           size = 2.5) +
                                                 theme(text = element_text(size = 11),
                                                       axis.title = element_text(size = 11),
                                                       legend.text = element_text(size = 11),
                                                       strip.text = element_blank(),
                                                       strip.background = element_blank(),
                                                       panel.spacing = unit(0.2, "lines")),
                                               size = 3))


saveRDS(proportion_contributions_region, file = "plots/proportion_contributions_region.rds")  
ggsave(proportion_contributions_region, file = "plots/proportion_contributions_region.jpg",
       dpi = 400, width = 7, height = 3)

# Species proportional contributions by region --------------------------------------

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
                                "DDTs"),
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
         type = fct_relevel(type, "Contaminants"),
         location = fct_relevel(location, "BeringSea"))

write_csv(chem_species_location_total_summary, file = "data/derived_quantities/chem_species_location_total_summary.csv")

(proportion_contributions_species_locations <- 
  chem_species_location_total_summary %>% 
    ggplot(aes(x = year, y = median)) + 
    geom_area(position = "fill", aes(fill = species)) +
    facet_grid(chemical ~ location) +
    scale_fill_viridis_d(direction = -1, option = 'A', alpha = 1,
                         begin = 0.15, end = 0.7) +
    scale_x_continuous(breaks = c(1980, 1995, 2010)) +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    coord_cartesian(ylim = c(0, 1.1)) +
    labs(y = "Median proportional contributions to\nnutrient and contaminant flux",
         fill = "Species",
         x = "Year") +
    theme(text = element_text(size = 11),
          axis.title = element_text(size = 11),
          legend.text = element_text(size = 11),
          # strip.text = element_blank(),
          # strip.background = element_blank(),
          panel.spacing = unit(0.2, "lines")) 
  )


saveRDS(proportion_contributions_species_locations, file = "plots/ms_plots/proportion_contributions_species_locations.rds")  
ggsave(proportion_contributions_species_locations, file = "plots/ms_plots/proportion_contributions_species_locations.jpg",
       dpi = 400, width = 7, height = 9)



# Proportional differences ------------------------------------------------

# proportions
species_props <- flux_predictions %>%
  mutate(type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "Nutrients",
                          TRUE ~ "Contaminants")) %>%
  group_by(species, year, type, .draw) %>%
  summarize(species_flux = sum(mg_flux)) %>%
  group_by(year, .draw, type) %>%
  mutate(total_flux = sum(species_flux),
         species_prop = species_flux/total_flux)

# saveRDS(species_props, file = "posteriors/derived_quantities/species_props.rds")

species_props <- readRDS("posteriors/derived_quantities/species_props.rds")


# check that it adds to ~ 1 (approximately because it is a sum of medians)
species_props %>% 
  group_by(year, type, .draw) %>% 
  summarize(sum = sum(species_prop))

# plot
trophic_levels <- tibble(tl = c(4.3, 3.9, 3.901, 3.6, 3.5),
                         species = c("Chinook", "Sockeye", "Coho",
                                     "Chum", "Pink"),
                         source = "Qin, Y., & Kaeriyama, M. (2016). Feeding habits and trophic levels of Pacific salmon (Oncorhynchus spp.) in the North Pacific Ocean. N Pac Anadromous Fish Com Bul, 6, 469-481.")

diff_props <- species_props %>% 
  left_join(trophic_levels) %>%  
  select(-species_flux, -total_flux) %>% 
  pivot_wider(names_from = type, values_from = species_prop) %>% 
  ungroup() %>% 
  mutate(cont_minus_nut = (Nutrients-Contaminants),
         species = fct_relevel(species, "Chinook", "Coho", "Sockeye", "Chum")) %>% 
  group_by(species, year) %>% 
  mutate(mean_difference = median(cont_minus_nut),
         species_tl = paste0(species, "\n(TL: ", round(tl,1), ")"))

species_props_mean <- diff_props %>% 
  group_by(species, .draw) %>% 
  summarize(mean_prop = mean(cont_minus_nut)) %>% 
  left_join(trophic_levels) %>% 
  mutate(species_tl = paste0(species, "\n(TL: ", round(tl,1), ")"))

prop_differences <- diff_props %>% 
  ungroup %>% 
  mutate(decade = case_when(year <1980 ~ "1970's",
                            year <= 1990 ~ "1980's",
                            year <= 2000 ~ "1990's", 
                            year <= 2010 ~ "2000's",
                            year <= 2020 ~ "2010's")) %>%
  ggplot(aes(x = cont_minus_nut, y = reorder(species_tl,tl))) +
  geom_density_ridges(fill = NA, scale = 0.9,
                      size = 0.1,
                      aes(group = interaction(species, year),
                          color = year)) +
  geom_boxplot(data = species_props_mean, aes(group = species, x = mean_prop), outlier.shape = NA,
               width = 0.1,
               size = 0.25) +
  scale_color_viridis(option = "E",
                      direction = -1) +
  geom_vline(xintercept = 0) + 
  labs(y = "", 
       color = "Year",
       x = "Relative contribution to continental biotransport") +
  annotate("text", label = "Relatively more contaminants", x = -0.14, y = 0.6,
           size = 2.0) +
  annotate("text", label = "Relatively more nutrients", x = 0.14, y = 0.6,
           size = 2.0) +
  # annotate("text", label = "More contaminants per nutrient", x = 2.3, y = 0.74, size = 2) +
  # annotate("segment", x = 2.5, y = 0.6, xend = 4, yend = 0.6,
  #          arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  # coord_cartesian(xlim = c(NA, 4)) +
  theme(legend.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 9),
        plot.subtitle = element_text(size = 10)) +
  NULL

prop_differences

saveRDS(prop_differences, file = "plots/prop_differences.rds")
ggsave(prop_differences, file = "plots/prop_differences.jpg", dpi = 400, width = 6, height = 5)
ggsave(prop_differences, file = "plots/prop_differences.pdf", dpi = 400, width = 6, height = 5)



# grams per nanogram ------------------------------------------------------

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
  mutate(species_tl = paste0(species, "\n(TL: ", round(tl,1), ")"))

saveRDS(species_ind_average, file = "posteriors/derived_quantities/species_ind_average.rds")

library(tidybayes)

species_ind_average = readRDS(file = "posteriors/derived_quantities/species_ind_average.rds")

ratio_per_kg = species_ind_average %>% 
  ggplot(aes(x = ratio_kgmgperfish, y = reorder(species_tl,tl))) +
  geom_density_ridges(scale = 1, size = 0.01) +
  geom_boxplot(outlier.shape = NA, width = 0.1, size = 0.25) +
  scale_color_viridis(option = "E",
                      direction = -1) +
  # geom_vline(xintercept = 1) + 
  labs(color = "Year",
       x = "Ratio of nutrients (kg/fish) to\ncontaminants (mg/fish) for individual fish") +
  annotate("text", label = "More nutrients per contaminant", x = 2.3, y = 0.74,
           size = 2) +
  annotate("segment", x = 2.5, y = 0.6, xend = 4, yend = 0.6,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  # coord_cartesian(xlim = c(NA, 4)) +
  labs(subtitle = "a) Nutrients to contaminants per fish") +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9),
        plot.subtitle = element_text(size = 10)) +
  NULL

saveRDS(ratio_per_kg, file = 'plots/ratio_per_kg.rds')

prop_differences = readRDS(file = "plots/prop_differences.rds") + 
  theme(plot.subtitle = element_text(size = 10),
        legend.position = c(0.8, 0.8))


fig_4new = ratio_per_kg +
  labs(subtitle = "a)") + 
  theme(plot.subtitle = element_text(size = 11),
        axis.title.x = element_text(size = 10)) +
  prop_differences + 
  labs(subtitle = "b)") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.subtitle = element_text(size = 11),
        legend.title=element_text(size=9),
        legend.text=element_text(size=9),
        legend.key.size = unit(0.3, 'cm'),
        axis.title.x = element_text(size = 10)) +
  NULL

saveRDS(fig_4new, file = "plots/ms_plots/fig_4new.rds")
ggsave(fig_4new, file = "plots/ms_plots/fig_4new.jpg", dpi = 600, width = 6.5, height = 3.5)
ggsave(fig_4new, file = "plots/ms_plots/fig_4new.pdf", dpi = 600, width = 6.5, height = 3.5)


# revised fig 4 -----------------------------------------------------------

mean_diff_across_species = species_props %>% 
  left_join(trophic_levels)  %>% 
  select(-source, -species_prop) %>%  
  group_by(year, type, .draw) %>% 
  reframe(total_flux = sum(species_flux)) %>% 
  pivot_wider(names_from = type, values_from = total_flux) %>% 
  ungroup() %>% 
  mutate(cont_minus_nut = (Nutrients/1e6/Contaminants)) %>% 
  group_by(year) %>% 
  mutate(mean_difference = median(cont_minus_nut))



ratio_nut_cont_time = mean_diff_across_species %>% 
  ggplot(aes(x = year, y = cont_minus_nut)) +
  stat_pointinterval(.width = c(0.95), size = 0.4) +
  labs(y = "Ratio of nutrient flux (kg/year)\nto contaminant flux (mg/year)",
       x = "Year",
       subtitle = "c)") +
  theme_default() +
  ylim(0, 2) +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

ggview::ggview(ratio_nut_cont_time, width = 6.5, height = 5)
ggsave(ratio_nut_cont_time, width = 6.5, height = 5, dpi = 500, file = "plots/revision_plots/ratio_nut_cont_time.jpg")
saveRDS(ratio_nut_cont_time, file = "plots/revision_plots/ratio_nut_cont_time.rds")

fig_4new = readRDS(file = "plots/ms_plots/fig_4new.rds")


fig_4revised = fig_4new + ratio_nut_cont_time + plot_layout(ncol = 2) 
ggview::ggview(fig_4revised, width = 6.5, height = 7)
ggsave(fig_4revised, width = 6.5, height = 7, dpi = 500, file = "plots/revision_plots/fig_4revised.jpg")
saveRDS(fig_4revised, file = "plots/revision_plots/fig_4revised.rds")




# hazard ratios -----------------------------------------------------------

all_chem_posts <- readRDS("posteriors/all_chem_posts.rds") 

risk_posts = all_chem_posts %>% 
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
  mutate(species_tl = paste0(species, "\n(TL: ", round(tl,1), ")"))


risk_plot = risk_posts  %>%  
  mutate(tl = case_when(species == "Sockeye" ~ tl - 0.1,
                        TRUE ~ tl)) %>%
  ggplot(aes(x = risk_quotient, y = reorder(species_tl,tl))) +
  geom_density_ridges(scale = 1) +
  geom_boxplot(outlier.shape = NA, width = 0.1, size = 1) +
  scale_color_viridis(option = "E",
                      direction = -1) +
  # geom_vline(xintercept = 1, linetype = "dashed") + 
  labs(x = "Risk-Benefit Quotient") +
  annotate("text", label = "More risk to consumers", x = 0.4, y = 0.8,
           size = 3) +
  annotate("segment", x = 0.1, y = 0.6, xend = 0.5, yend = 0.6,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  coord_cartesian(xlim = c(NA, 0.8)) +
  # labs(subtitle = "c) Risk-Benefit Quotient") +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9)) +
  # scale_x_log10() +
  NULL


ggview::ggview(risk_plot, width = 5, height = 5, units = "in")
saveRDS(risk_plot, file = "plots/ms_plots/fig_5.rds")
ggsave(risk_plot, file = "plots/ms_plots/fig_5.jpg", width = 5, height = 5, units = "in", dpi = 500 )
ggsave(risk_plot, file = "plots/ms_plots/fig_5.pdf", width = 5, height = 5, units = "in", dpi = 500 )

risk_table = risk_posts %>% 
  group_by(species, tl) %>% 
  summarize(mean = mean(risk_quotient),
            sd = sd(risk_quotient),
            low95 = quantile(risk_quotient, probs = 0.025),
            upper95 = quantile(risk_quotient, probs = 0.975)) %>%
  mutate(across(where(is.numeric), round, 2)) %>% 
  arrange(-tl) %>% 
  select(-tl)




library(patchwork)
all_three_ratios = ratio_per_kg + prop_differences + guides(color = "none") +
  labs(subtitle = "b) Contribution to biotransport") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  risk_plot + 
  plot_layout(ncol = 2) +
  NULL


ggview::ggview(all_three_ratios, units = "in", width = 6.5, height = 7)
ggsave(all_three_ratios, units = "in", width = 6.5, height = 7,
       file = "plots/all_three_ratios.jpg")
ggsave(all_three_ratios, units = "in", width = 6.5, height = 7,
       file = "plots/ms_plots/fig4_all_three_ratios.jpg")

