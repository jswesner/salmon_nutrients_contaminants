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

theme_set(theme_default()) 

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

# load raw data
nut_cont <- readRDS("data/nut_cont.rds")   # nutrient and contaminant concentrations mg/kg
d_short <- readRDS("data/d_short.rds") %>% 
  separate(species, c("species", "family")) # salmon escapement after 1975


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
  select(metric_tons, .draw, year, species, location) %>% 
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
       subtitle = "Total Escapement") +
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
       subtitle = "Species Escapement") + 
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
          species_escapement + guides(color = F, fill = F),
          legend_escapement,
          ncol = 3,
          rel_widths = c(0.36, 0.43, 0.2))


saveRDS(escapement_plot, file = "plots/escapement_plot.rds")
ggsave(escapement_plot, file = "plots/escapement_plot.jpg", dpi = 400, width = 6, height = 8)



# Chemical Concentrations -------------------------------------------------

chem_concentrations <- all_chem_posts%>% 
  mutate(chemical = as.character(chemical),
         chemical = case_when(chemical == "DDT" ~ "DDTs",
                              chemical == "PBDE" ~ "PBDEs",
                              TRUE ~ chemical)) %>% 
  mutate(type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "Nutrients",
                          TRUE ~ "Contaminants"),
         chemical = fct_relevel(chemical, "N",
                                "DHA", 
                                "EPA",
                                "P",
                                "Hg",
                                "DDTs"),
         type = fct_relevel(type, "Nutrients"),
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
                      type = case_when(type == "nutrient" ~ "Nutrients",
                                       TRUE ~ "Contaminants"),
                      type = fct_relevel(type, "Nutrients")),
             position = position_dodge(width = 0.75), 
             aes(y = mean_concentration_standardized, group = species),
             size = 0.4) +
  guides(fill = "none") +
  labs(y = "Whole body concentrations (mg/kg ww)",
       x = "Chemical") +
  theme(text = element_text(size = 11),
        plot.subtitle = element_text(size = 11)) +
  scale_color_viridis_d(option = "A",
                        direction = 1) +
  scale_fill_viridis_d(option = "A",
                       direction = 1)

legend_escapement_conc <- get_legend(species_escapement + theme(legend.position = "top"))

(escapement_plus_a <- plot_grid(total_escapement,
                                species_escapement + guides(color = F, fill = F),
                                chem_concentrations,
                                ncol = 3,
                                rel_widths = c(0.36, 0.43, 0.55)))


(escapement_plus_concentrations_plot <- plot_grid(legend_escapement_conc,
          escapement_plus_a,
          ncol = 1,
          rel_heights = c(0.1, 1)))


ggsave(escapement_plus_concentrations_plot, file = "plots/escapement_plus_concentrations_plot.jpg",
       dpi = 400, width = 6.5, height = 6, units = "in")


# Flux Time Series --------------------------------------------------------

# summarize and wrangle posteriors

summary_species_combined <- readRDS(file = "posteriors/derived_quantities/summary_species_combined.rds")
summary_location_combined <- readRDS(file = "posteriors/derived_quantities/summary_location_combined.rds")
summary_all <- readRDS(file = "posteriors/derived_quantities/summary_all.rds")

# summary_species_combined <- flux_predictions %>%
#   ungroup() %>%
#   group_by(year, species, chemical, .draw) %>% 
#   summarize(total = sum(mg_flux/1000000)) %>% 
#   group_by(year, chemical, species) %>% 
#   summarize(median = median(total),
#             low75 = quantile(total, probs = 0.125),
#             high75 = quantile(total, probs = 1-0.125),
#             low50 = quantile(total, probs = 0.25),
#             high50 = quantile(total, probs = 1-0.25)) %>% 
#   ungroup() %>% 
#   mutate(type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "Nutrients",
#                           TRUE ~ "Contaminants"),
#          chemical = fct_relevel(chemical, "N", 
#                                 "P", 
#                                 "DHA", 
#                                 "EPA",
#                                 "Hg",
#                                 "DDT"),
#          group = case_when(species == "All" ~ "All",
#                            TRUE ~ "Species"),
#          units = "Kg")
# 
# summary_location_combined <- flux_predictions %>%
#   ungroup() %>%
#   group_by(year, location, chemical, .draw) %>% 
#   summarize(total = sum(mg_flux/1000000)) %>% 
#   group_by(year, chemical, location) %>% 
#   summarize(median = median(total),
#             low75 = quantile(total, probs = 0.125),
#             high75 = quantile(total, probs = 1-0.125),
#             low50 = quantile(total, probs = 0.25),
#             high50 = quantile(total, probs = 1-0.25)) %>% 
#   ungroup() %>% 
#   mutate(type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "Nutrients",
#                           TRUE ~ "Contaminants"),
#          chemical = fct_relevel(chemical, "N", 
#                                 "P", 
#                                 "DHA", 
#                                 "EPA",
#                                 "Hg",
#                                 "DDT"),
#          units = "Kg")
# 
# 
# summary_all <- flux_predictions %>% 
#   group_by(.draw, year, chemical) %>% 
#   summarize(total = sum(mg_flux/1000000)) %>% 
#   group_by(year, chemical) %>% 
#   summarize(median = median(total),
#             low75 = quantile(total, probs = 0.125),
#             high75 = quantile(total, probs = 1-0.125),
#             low50 = quantile(total, probs = 0.25),
#             high50 = quantile(total, probs = 1-0.25)) %>% 
#   ungroup() %>% 
#   mutate(type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "Nutrients",
#                           TRUE ~ "Contaminants"),
#          chemical = fct_relevel(chemical, "N", 
#                                 "P", 
#                                 "DHA", 
#                                 "EPA",
#                                 "Hg",
#                                 "DDT"),
#          group = "All",
#          units = "Kg")


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




library(ggrepel)

a <- species_location_series %>% 
  glimpse() %>% 
  filter(type != "Contaminants" & group != "Location") %>% 
  mutate(chemical = fct_relevel(chemical, "N", "DHA", "EPA"),
         panel = fct_relevel(panel, "All", "Pink", "Sockeye", "Chum", "Chinook")) %>% 
  ggplot(aes(x = year, y = median, color = panel)) +
  geom_linerange(aes(ymin = low75, ymax = high75), alpha = 0.5,
                 size = .6) +
  geom_point(size = 0.2) +
  # geom_ribbon(aes(ymin = low50, ymax = high50), alpha = 0.8) +
  # geom_line() + 
  scale_color_viridis_d(direction = 1, option = 'A', end = 0.7) +
  scale_fill_viridis_d(direction = 1, option = 'A', alpha = 0.5) +
  guides(color = "none",
         fill = "none") +
  labs(y = "Total Export (kg/y)",
       subtitle = "Nutrients") +
  facet_wrap(~ chemical, ncol = 4) +
  scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  geom_text_repel(data = . %>% filter(year == max(year)) %>% 
                    distinct(year, chemical, median, panel), 
                  aes(x = 2015,
                      y = median, 
                      color = panel,
                      label = panel),
                  # color = "#FCFDBFFF",
                  size = 2,
                  nudge_x = 8,
                  direction = "y",
                  hjust = 0
  ) +         
  coord_cartesian(xlim = c(1975, 2030)) +
  theme_default() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  NULL



b <- species_location_series %>% 
  glimpse() %>% 
  filter(type == "Contaminants" & group == "Species") %>% 
  mutate(chemical = fct_relevel(chemical, "PCBs", "Hg"),
         panel = fct_relevel(panel, "All", "Pink", "Sockeye", "Chum", "Chinook")) %>%  
  ggplot(aes(x = year, y = median, color = panel)) +
  geom_linerange(aes(ymin = low75, ymax = high75), alpha = 0.5,
                 size = .5) +
  geom_point(size = 0.2) +
  # geom_ribbon(aes(ymin = low50, ymax = high50), alpha = 0.8) +
  # geom_line() + 
  scale_color_viridis_d(direction = 1, option = 'A', end = 0.7) +
  scale_fill_viridis_d(direction = 1, option = 'A', alpha = 0.5) +
  guides(color = "none",
         fill = "none") +
  labs(y = "Total Export (kg/y)",
       subtitle = "Contaminants") +
  facet_wrap(~ chemical, ncol = 4) +
  scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  geom_text_repel(data = . %>% filter(year == max(year)) %>% 
                    distinct(year, chemical, median, panel), 
                  aes(x = 2015,
                      y = median, 
                      color = panel,
                      label = panel),
                  size = 2,
                  nudge_x = 8,
                  direction = "y",
                  hjust = 1
  ) +         
  coord_cartesian(xlim = c(1975, 2030)) +
  theme_default() +
  theme(axis.title.x = element_blank()) +
  NULL


c <- species_location_series %>% 
  glimpse() %>% 
  filter(type != "Contaminants" & group == "Location") %>% 
  mutate(chemical = fct_relevel(chemical, "N", "DHA", "EPA")) %>% 
  mutate(panel = case_when(panel == "BeringSea" ~ "BS",
                           panel == "CentralAK" ~ "CAK",
                           TRUE ~ panel),
         panel = fct_relevel(panel, "All", "BCWC", "SEAK", "CAK", "BS")) %>% 
  ggplot(aes(x = year, y = median, color = panel)) +
  geom_linerange(aes(ymin = low75, ymax = high75), alpha = 0.5,
                 size = .5) +
  geom_point(size = 0.2) +
  # geom_ribbon(aes(ymin = low50, ymax = high50), alpha = 0.8) +
  # geom_line() + 
  scale_color_viridis_d(direction = 1, option = 'D', end = 0.9) +
  scale_fill_viridis_d(direction = 1, option = 'D', alpha = 0.5) +
  guides(color = "none",
         fill = "none") +
  labs(y = "Total Export (kg/y)",
       subtitle = "") +
  facet_wrap(~ chemical, ncol = 4) +
  scale_y_log10(breaks = c(1e5, 1e6, 1e7), labels = comma,
                limits = c(1e5, 1.5e7)) +
  scale_x_continuous(breaks = c(1975, 1995, 2015),
                     limits = c(1975, 2030)) +
  geom_text_repel(data = . %>% filter(year == max(year))  %>% 
                    distinct(year, chemical, median, panel), 
                  aes(x = 2015,
                      y = median, 
                      color = panel,
                      label = panel),
                  size = 2,
                  nudge_x = 8,
                  direction = "y",
                  hjust = 1
  ) +         
  theme_default() +
  theme(axis.title.x = element_blank()) +
  NULL


d <- species_location_series %>% 
  glimpse() %>% 
  filter(type == "Contaminants" & group == "Location") %>% 
  mutate(chemical = fct_relevel(chemical, "PCBs", "Hg")) %>%
  mutate(panel = case_when(panel == "BeringSea" ~ "BS",
                           panel == "CentralAK" ~ "CAK",
                           TRUE ~ panel),
         panel = fct_relevel(panel, "All", "BCWC", "SEAK", "CAK", "BS")) %>% 
  ggplot(aes(x = year, y = median, color = panel)) +
  geom_linerange(aes(ymin = low75, ymax = high75), alpha = 0.5,
                 size = .5) +
  geom_point(size = 0.2) +
  # geom_ribbon(aes(ymin = low50, ymax = high50), alpha = 0.8) +
  # geom_line() + 
  scale_color_viridis_d(direction = 1, option = 'D', end = 0.9) +
  scale_fill_viridis_d(direction = 1, option = 'D', alpha = 0.5) +
  guides(color = "none",
         fill = "none") +
  labs(y = "Total Export (kg/y)",
       subtitle = "") +
  facet_wrap(~ chemical, ncol = 4) +
  scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  geom_text_repel(data = . %>% filter(year == max(year)) %>% 
                    distinct(year, chemical, median, panel), 
                  aes(x = 2015,
                      y = median,
                      color = panel,
                      label = panel),
                  size = 2,
                  nudge_x = 8,
                  direction = "y",
                  hjust = 1
  ) +         
  coord_cartesian(xlim = c(1975, 2030)) +
  theme_default() +
  theme(axis.title.x = element_blank()) +
  NULL


flux_fig_time <- plot_grid(a,
          c,
          b,
          d, 
          ncol = 1,
          align = "v")



saveRDS(flux_fig_time, file = "plots/flux_fig_time.rds")
ggsave(flux_fig_time, file = "plots/flux_fig_time.jpg", dpi = 500, width = 8, height = 9)



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
         units = "Kg")

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
         units = "Kg",
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
  labs(x = "Kg in thousands (1976-2015)",
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
  labs(x = "Kg (1976-2015)",
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
chem_species_prop_nutcon <- readRDS("posteriors/derived_quantities/chem_species_prop_nutcon.rds")

# chem_species_prop_nutcont <- flux_predictions %>% 
#   filter(.draw <= 500) %>%
#   select(species, location, year, .draw, mg_flux, chemical) %>% 
#   mutate(type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "Nutrients",
#                           TRUE ~ "Contaminants")) %>% 
#   group_by(species, year, .draw, type) %>% 
#   summarize(mg_flux = sum(mg_flux)) %>% 
#   pivot_wider(names_from = species, values_from = mg_flux) %>% 
#   mutate(All = Chinook + Chum + Coho + Pink + Sockeye) %>% 
#   pivot_longer(cols = c(Chinook, Chum, Coho, Pink, Sockeye), names_to = "species") %>% 
#   mutate(proportion = value/All)
# 
# chem_species_prop_summary_nutcont <- chem_species_prop_nutcont %>% 
#   ungroup() %>%
#   group_by(type, year, species) %>% 
#   mutate(total = proportion) %>% 
#   summarize(median = median(total),
#             low75 = quantile(total, probs = 0.125),
#             high75 = quantile(total, probs = 1-0.125),
#             low50 = quantile(total, probs = 0.25),
#             high50 = quantile(total, probs = 1-0.25)) %>% 
#   ungroup() 


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
         units = "Kg") %>%
  mutate(species = fct_relevel(species, "Coho", "Chinook", "Chum", "Sockeye", "Pink"),
         type = fct_relevel(type, "Contaminants"))

(proportion_contributions_species_nutcont <- tag_facet(chem_species_total_summary_nutcont %>% 
                                                 ggplot(aes(x = year, y = median)) + 
                                                 geom_area(position = "fill", aes(fill = species)) +
                                                 facet_wrap( ~type, nrow = 1) +
                                                   scale_fill_viridis_d(direction = -1, option = 'A', alpha = 1,
                                                                        begin = 0.15, end = 0.7)  +
                                                 scale_x_continuous(breaks = c(1980, 1995, 2010)) +
                                                 scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
                                                 coord_cartesian(ylim = c(0, 1.1)) +
                                                 labs(y = "Median proportional contributions to\nnutrient and contaminant flux",
                                                      fill = "Species",
                                                      x = "Year") +
                                                 geom_text(data = chem_species_total_summary_nutcont %>% distinct(type, type, species = NA),
                                                           aes(x = 1975 + 12, y = 1.08, label = type),
                                                           size = 2.5) +
                                                 theme(text = element_text(size = 12),
                                                       axis.title = element_text(size = 11),
                                                       legend.text = element_text(size = 11),
                                                       strip.text = element_blank(),
                                                       strip.background = element_blank(),
                                                       panel.spacing = unit(0.2, "lines")),
                                               size = 3))


saveRDS(proportion_contributions_species_nutcont, file = "plots/proportion_contributions_species_nutcont.rds")  
ggsave(proportion_contributions_species_nutcont, file = "plots/proportion_contributions_species_nutcont.jpg",
       dpi = 400, width = 7, height = 3)


# Species proportional contributions --------------------------------------
chem_species_prop <- readRDS("posteriors/derived_quantities/chem_species_prop.rds")

# chem_species_prop <- flux_predictions %>% 
#   filter(.draw <= 500) %>%
#   select(species, location, year, .draw, mg_flux, chemical) %>% 
#   pivot_wider(names_from = species, values_from = mg_flux) %>% 
#   mutate(All = Chinook + Chum + Coho + Pink + Sockeye) %>% 
#   pivot_longer(cols = c(Chinook, Chum, Coho, Pink, Sockeye), names_to = "species") %>% 
#   mutate(proportion = value/All)

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
         units = "Kg")


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
         units = "Kg") %>%
  mutate(species = fct_relevel(species, "Coho", "Chinook", "Chum", "Sockeye", "Pink"),
         type = fct_relevel(type, "Contaminants"))

(proportion_contributions_species <- tag_facet(chem_species_total_summary %>% 
  ggplot(aes(x = year, y = median)) + 
  geom_area(position = "fill", aes(fill = species)) +
  facet_wrap( ~chemical, nrow = 2) +
    scale_fill_viridis_d(direction = -1, option = 'A', alpha = 1,
                         begin = 0.15, end = 0.7) +
  scale_x_continuous(breaks = c(1980, 1995, 2010)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  coord_cartesian(ylim = c(0, 1.1)) +
  labs(y = "Median proportional contributions to\nnutrient and contaminant flux",
       fill = "Species",
       x = "Year") +
  geom_text(data = chem_species_total_summary %>% distinct(chemical, type, species = NA),
            aes(x = 1975 + 12, y = 1.08, label = chemical),
            size = 2.5) +
  theme(text = element_text(size = 11),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(0.2, "lines")),
  size = 3))
            

saveRDS(proportion_contributions_species, file = "plots/proportion_contributions_species.rds")  
ggsave(proportion_contributions_species, file = "plots/proportion_contributions_species.jpg",
       dpi = 400, width = 7, height = 3)


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
         units = "Kg")


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
         units = "Kg") %>%
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
                                "DDT"),
         group = case_when(species == "All" ~ "All",
                           TRUE ~ "Species"),
         units = "Kg") %>%
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
         units = "Kg") %>%
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


saveRDS(proportion_contributions_species_locations, file = "plots/proportion_contributions_species_locations.rds")  
ggsave(proportion_contributions_species_locations, file = "plots/proportion_contributions_species_locations.jpg",
       dpi = 400, width = 7, height = 9)



# Proportional differences ------------------------------------------------

# proportions
# species_props <- flux_predictions %>% 
#   mutate(type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "Nutrients",
#                           TRUE ~ "Contaminants")) %>% 
#   group_by(species, year, type, .draw) %>% 
#   summarize(species_flux = sum(mg_flux)) %>% 
#   group_by(year, .draw, type) %>% 
#   mutate(total_flux = sum(species_flux),
#          species_prop = species_flux/total_flux) %>% 
#   group_by(species, year, type, .draw) %>% 
#   summarize(median_prop = median(species_prop))
# saveRDS(species_props, file = "posteriors/derived_quantities/species_props.rds")

species_props <- readRDS("posteriors/derived_quantities/species_props.rds")


# check that it adds to ~ 1 (approximately because it is a sum of medians)
species_props %>% 
  group_by(year, type, .draw) %>% 
  summarize(sum = sum(median_prop))

# plot
text_to_add <- tibble(x = -0.02, y = 0.5, 
                      label = "<-- More contaminants         More nutrients -->")

(prop_differences <- species_props %>% 
  pivot_wider(names_from = type, values_from = median_prop) %>% 
  ungroup() %>% 
  mutate(cont_minus_nut = Nutrients - Contaminants,
         species = fct_relevel(species, "Chinook", "Coho", "Sockeye", "Chum")) %>% 
  group_by(species, year) %>% 
  mutate(mean_difference = median(cont_minus_nut)) %>% 
  ggplot(aes(x = cont_minus_nut, y = species)) + 
  geom_density_ridges(fill = NA, scale = 1, size = 0.2,
                      aes(group = interaction(species, year), 
                          color = year)) +
    # scale_color_brewer(type = "div") +
  scale_color_viridis(option = "E",
                      direction = -1) +
  geom_vline(xintercept = 0) + 
  xlim(-0.3, 0.3) +
  labs(y = "", 
       color = "Year",
       x = "Proportion of nutrients minus proportion of contaminants") +
    geom_text(data = text_to_add, aes(x = x, y = y, label = label),
              size = 3) +
  # guides(color = "none") +
  NULL)


ggsave(prop_differences, file = "plots/prop_differences.jpg", dpi = 400, width = 6, height = 5)



