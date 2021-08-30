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

# bring in posteriors
flux_predictions <- readRDS(file = "posteriors/flux_predictions.rds") # posterior chem export
region_concentrations <- readRDS(file = "data/derived_quantities/region_concentrations.rds") 
gam_salmon_posts <- readRDS("posteriors/gam_salmon_posts.rds") # Salmon escapement in kg wet mass

# Salmon escapement time series -------------------------------------------
#raw data
d_short <- readRDS("data/d_short.rds") %>% 
  separate(species, c("species", "family"))

d_region_toplot <- d_short %>% select(year, species, location, y) %>% 
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
  select(-metric_tons, -value) %>% 
  #calculate total Hg and kg 
  pivot_wider(names_from = species, values_from = kg) %>% 
  mutate(Total = Chinook + Chum + Coho + Pink + Sockeye) %>% 
  pivot_longer(cols = c(-location, -year, -iter), names_to = "species", values_to = "kg") %>%
  mutate(metric_tons = kg*0.001)

salmon_mass_region_toplot <- salmon_mass %>%
  group_by(species, year, iter, location) %>% 
  summarize(metric_tons = sum(metric_tons)) %>% 
  pivot_wider(names_from = location, values_from = metric_tons) %>% 
  mutate(`All Regions` = BCWC + BeringSea + CentralAK + SEAK) %>% 
  pivot_longer(cols = c(-iter, -species, -year), names_to = "location", values_to = "metric_tons") %>% 
  group_by(species, year, location) %>% 
  summarize(low = quantile(metric_tons, probs = 0.025),
            med = median(metric_tons),
            upper = quantile(metric_tons, probs = 0.975),
            low50 = quantile(metric_tons, probs = 0.25),
            upper50 = quantile(metric_tons, probs = 0.75)) %>% 
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
  geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.2) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.2) +
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
  scale_fill_colorblind() + 
  scale_color_colorblind() + 
  labs(fill = "",
       color = "",
       y = "",
       subtitle = "Species Escapement") + 
  theme(legend.text=element_text(size=10),
        text = element_text(size = 11),
        plot.subtitle = element_text(size = 11))

legend_escapement <- get_legend(species_escapement)

escapement_plot <- plot_grid(total_escapement,
          species_escapement + guides(color = F, fill = F),
          legend_escapement,
          ncol = 3,
          rel_widths = c(0.36, 0.43, 0.2))


saveRDS(escapement_plot, file = "plots/escapement_plot.rds")
ggsave(escapement_plot, file = "plots/escapement_plot.jpg", dpi = 400, width = 6, height = 8)

# Flux Time Series --------------------------------------------------------

# summarize and wrangle posteriors
summary_species_combined <- flux_predictions %>%
  ungroup() %>%
  group_by(year, species, chemical, iter) %>% 
  summarize(total = sum(g_flux/1000)) %>% 
  group_by(year, chemical, species) %>% 
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


# make plots

nutrients_total <- summary_species_combined %>% ungroup() %>%
  filter(group == "All" & type == "Nutrients") %>%
  ggplot(aes(x = year, y = median)) + 
  geom_line() +
  geom_ribbon(alpha = 0.2, aes(ymin = low75, ymax = high75)) +
  geom_ribbon(alpha = 0.6, aes(ymin = low50, ymax = high50)) +
  facet_wrap( ~ chemical, scales = "free_y", ncol = 1) +
  labs(y = "Kg per year exported via salmon escapement",
       # fill = "Species",
       x = "Year",
       subtitle = "Total Export (kg/y)",
       title = "Nutrients") +
  theme(text = element_text(size = 11),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 11)) +
  theme(axis.title.x = element_blank(),
        plot.title = element_text(size = 11)) +
  scale_y_continuous(labels = scientific) +
  NULL 

nutrients_species <- summary_species_combined %>% ungroup() %>%
  filter(group != "All" & type == "Nutrients") %>%
  mutate(species = fct_relevel(species, "Coho", "Chinook", "Chum", "Sockeye", "Pink")) %>% 
  ggplot(aes(x = year, y = median, fill = species)) + 
  geom_line() +
  geom_ribbon(alpha = 0.2, aes(ymin = low75, ymax = high75)) +
  geom_ribbon(alpha = 0.6, aes(ymin = low50, ymax = high50)) +
  facet_wrap( ~ chemical, scales = "free_y", ncol = 1, strip.position = "right") +
  labs(y = "Kg per year exported via salmon escapement",
       fill = "Species",
       x = "Year",
       subtitle = "Export by Species (kg/y)",
       title = "") +
  scale_fill_viridis_d(direction = -1, option = 'A',
                       guide = guide_legend(reverse = TRUE)) +
  theme(text = element_text(size = 11),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 11)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 11)) +
  scale_y_continuous(labels = scientific) +
  NULL 

contaminants_total <- summary_species_combined %>% ungroup() %>%
  filter(group == "All" & type != "Nutrients") %>%
  ggplot(aes(x = year, y = median)) + 
  geom_line() +
  geom_ribbon(alpha = 0.2, aes(ymin = low75, ymax = high75)) +
  geom_ribbon(alpha = 0.6, aes(ymin = low50, ymax = high50)) +
  facet_wrap( ~ chemical, scales = "free_y", ncol = 1) +
  labs(y = "Kg per year exported via salmon escapement",
       # fill = "Species",
       x = "Year",
       # subtitle = "Total Export (kg/y)",
       title = "Contaminants") +
  theme(text = element_text(size = 11),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        plot.title = element_text(size = 11)) +
  scale_y_continuous(labels = scientific) +
  NULL 

contaminants_species <- summary_species_combined %>% ungroup() %>%
  filter(group != "All" & type != "Nutrients") %>%
  mutate(species = fct_relevel(species, "Coho", "Chinook", "Chum", "Sockeye", "Pink")) %>% 
  ggplot(aes(x = year, y = median, fill = species)) + 
  geom_line() +
  geom_ribbon(alpha = 0.2, aes(ymin = low75, ymax = high75)) +
  geom_ribbon(alpha = 0.6, aes(ymin = low50, ymax = high50)) +
  facet_wrap( ~ chemical, scales = "free_y", ncol = 1, strip.position = "right") +
  labs(y = "Kg per year exported via salmon escapement",
       fill = "Species",
       x = "Year",
       # subtitle = "Export by Species (kg/y)",
       title = "") +
  scale_fill_viridis_d(direction = -1, option = 'A',
                       guide = guide_legend(reverse = TRUE,
                                            override.aes = list(alpha = 1,
                                                                size = 5))) +
  theme(text = element_text(size = 11),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 11)) +
  theme(axis.title.y = element_blank(),
        # axis.title.x = element_blank(),
        plot.title = element_text(size = 11)) +
  scale_y_continuous(labels = scientific) +
  NULL 

species_legend <- get_legend(contaminants_species + 
                               theme(legend.text = element_text(size = 10),
                                     legend.title = element_text(size = 10)))

totals_all <- plot_grid(tag_facet(nutrients_total + guides(fill = F),
                                  tag_pool = c("a", "c", "e", "g"),
                                  open = "", 
                                  close = ")",
                                  size = 2.5), 
                        tag_facet(contaminants_total + guides(fill = F),
                                  tag_pool = c("i", "k", "m", "o"),
                                  open = "", close = ")",
                                  size = 2.5,
                                  # y = -Inf, 
                                  vjust = 1, 
                                  hjust = -0.3),
                        align = "v",
                        ncol = 1)

species_all <- plot_grid(tag_facet(nutrients_species + guides(fill = F) +
                                     guides(fill = F) +
                                     labs(y = ""),
                                   tag_pool = c("b", "d", "f", "h"),
                                   open = "", 
                                   close = ")",
                                   size = 2.5) , 
                         tag_facet(contaminants_species +
                                     guides(fill = F),
                                   tag_pool = c("j", "l", "n", "p"),
                                   open = "", close = ")",
                                   size = 2.5,
                                   x = -Inf, 
                                   vjust = 1, 
                                   hjust = -0.3),
                         align = "v",
                         ncol = 1)

flux_fig_time <- plot_grid(totals_all,
                           species_all,
                           species_legend,
                           ncol = 3,
                           rel_widths = c(0.17, 0.15, 0.1))


saveRDS(flux_fig_time, file = "plots/flux_fig_time.rds")
ggsave(flux_fig_time, file = "plots/flux_fig_time.jpg", dpi = 500, width = 6, height = 9)




# Totals by region --------------------------------------------------------


summary_location_combined <- flux_predictions %>% 
  ungroup() %>% 
  filter(species != "All") %>% 
  group_by(location, iter, chemical) %>% 
  summarize(total_flux = sum(g_flux/1000)) %>% 
  mutate(type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "Nutrients",
                          TRUE ~ "Contaminants"),
         chemical = fct_relevel(chemical, "N", "Hg", "P", "DDT", "DHA", "PBDE", "EPA", "PCBS"),
         # group = case_when(species == "All" ~ "All",
         #                   TRUE ~ "Species"),
         units = "Kg")

labels_6 <- tibble(letters = paste0(letters[1:8], ")")) %>% 
  mutate(chemical = c("N", "Hg", "P", "DDT", "DHA", "PBDE", "EPA", "PCBS"),
         label = paste0(letters, " ", chemical),
         x = "SEAK") %>% 
  mutate(chemical = fct_relevel(chemical, "N", "Hg", "P", "DDT", "DHA", "PBDE", "EPA", "PCBS"))

total_chem_plot <- summary_location_combined %>% 
  ggplot() +
  geom_boxplot(aes(x = location, y = total_flux/1000, fill = location),
               outlier.shape = NA) +
  # scale_y_log10(labels = scientific, expand = c(0.2, 0)) +
  # scale_x_discrete(expand = c(0.5,0)) +
  facet_wrap( ~ chemical, scales = "free_x", ncol = 2) + 
  scale_fill_grey(start = 0.4, end = 1) +
  guides(fill = F) +
  labs(y = "Cumulative Metric Tons (1975-2015)") +
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


# Sum over years ---------------------------------------------------------------

sum_over_all_years <- flux_predictions %>%
  ungroup() %>% 
  group_by(chemical, iter, species) %>% 
  summarize(total = sum(g_flux/1000/1000)) %>% 
  mutate(units = "metric tons") %>% 
  group_by(species, chemical, units) %>% 
  summarize(mean = mean(total),
            sd = sd(total), 
            median = median(total),
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
                           TRUE ~ "Species"))

sum_by_species <- sum_over_all_years %>% 
  select(species, chemical, units, mean, sd) %>% 
  mutate(mean = case_when(mean <0.2 ~ round(mean, 3),
                          TRUE ~ round(mean,0)),
         sd = case_when(sd < 0.2 ~ round(sd, 3),
                        TRUE ~ round(sd, 0))) %>% 
  mutate(mean_sd = paste0(mean,  " \u00B1 ", sd)) %>% 
  select(-mean, -sd) %>% 
  pivot_wider(names_from = species, values_from = mean_sd) %>% 
  arrange(chemical, All)

write.csv(sum_by_species, file = "tables/sum_by_species.csv", row.names = F)

# Average over years ---------------------------------------------------------------
ave_over_all_years <- flux_predictions %>%
  ungroup() %>% 
  group_by(chemical, iter, species) %>% 
  summarize(total = mean(g_flux/1000/1000)) %>% # says "total", but function is mean. 
  mutate(units = "metric tons") %>% 
  group_by(species, chemical, units) %>% 
  summarize(mean = mean(total),
            sd = sd(total), 
            median = median(total),
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
                           TRUE ~ "Species"))

ave_by_species <- ave_over_all_years %>% 
  select(species, chemical, units, mean, sd) %>% 
  mutate(mean = case_when(mean <0.2 ~ round(mean, 4),
                          TRUE ~ round(mean,0)),
         sd = case_when(sd < 0.2 ~ round(sd, 6),
                        TRUE ~ round(sd, 0))) %>% 
  mutate(mean_sd = paste0(mean,  " \u00B1 ", sd)) %>% 
  select(-mean, -sd) %>% 
  pivot_wider(names_from = species, values_from = mean_sd) %>% 
  arrange(chemical, All)

write.csv(ave_by_species, file = "tables/ave_by_species.csv", row.names = F)







# Species proportional contributions --------------------------------------

chem_species_prop <- flux_predictions %>% 
  pivot_wider(names_from = species, values_from = g_flux) %>% 
  pivot_longer(cols = c(Chinook, Chum, Coho, Pink, Sockeye), names_to = "species") %>% 
  mutate(proportion = value/All)

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

proportion_contributions_species <- tag_facet(chem_species_total_summary %>% 
  ggplot(aes(x = year, y = median)) + 
  geom_area(position = "fill", aes(fill = species)) +
  facet_wrap( ~chemical, nrow = 2) +
  scale_fill_grey(start = 0.9, end = 0.2) +
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
  size = 3)
            

saveRDS(proportion_contributions_species, file = "plots/proportion_contributions_species.rds")  
ggsave(proportion_contributions_species, file = "plots/proportion_contributions_species.jpg",
       dpi = 400, width = 7, height = 3)


# Species proportional contributions by region --------------------------------------

chem_species_prop <- flux_predictions %>% 
  pivot_wider(names_from = species, values_from = g_flux) %>% 
  pivot_longer(cols = c(Chinook, Chum, Coho, Pink, Sockeye), names_to = "species") %>% 
  mutate(proportion = value/All)

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

proportion_contributions_species_locations <- 
  chem_species_location_total_summary %>% 
    ggplot(aes(x = year, y = median)) + 
    geom_area(position = "fill", aes(fill = species)) +
    facet_grid(chemical ~ location) +
    # scale_fill_grey(start = 0.9, end = 0.2) +
  scale_fill_viridis_d(direction = -1, option = "A") +
    scale_x_continuous(breaks = c(1980, 1995, 2010)) +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    coord_cartesian(ylim = c(0, 1.1)) +
    labs(y = "Median proportional contributions to\nnutrient and contaminant flux",
         fill = "Species",
         x = "Year") +
    # geom_text(data = chem_species_total_summary %>% distinct(chemical, type, species = NA),
    #           aes(x = 1975 + 12, y = 1.08, label = chemical),
    #           size = 2.5) +
    theme(text = element_text(size = 11),
          axis.title = element_text(size = 11),
          legend.text = element_text(size = 11),
          # strip.text = element_blank(),
          # strip.background = element_blank(),
          panel.spacing = unit(0.2, "lines"))


saveRDS(proportion_contributions_species_locations, file = "plots/proportion_contributions_species_locations.rds")  
ggsave(proportion_contributions_species_locations, file = "plots/proportion_contributions_species_locations.jpg",
       dpi = 400, width = 7, height = 9)

chem_species_prop_summary %>% 
  filter(species %in% c("Chum", "Pink", "Sockeye") &
           year == min(year) | year == max(year)) %>%
  group_by(chemical, location, species, year) %>% 
  filter(median == min(median)) %>% 
  filter(location == "BeringSea") %>% 
  arrange(type) %>% 
  select(chemical, species, location, median, year) %>% 
  pivot_wider(names_from = species, values_from = median)


