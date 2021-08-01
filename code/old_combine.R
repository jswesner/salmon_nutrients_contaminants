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

# 1) Load Posteriors -----------------------------------------------------

gam_salmon_posts <- readRDS("posteriors/gam_salmon_posts.rds") # Salmon escapement in kg wet mass
hg_posts <- readRDS("posteriors/hg_posts.rds") # Mercury concentrations in ug per kg wet mass
pcb_posts <- readRDS("posteriors/pcb_posts.rds") # PCB concentrations in ug per kg wet mass
nit_posts <- readRDS("posteriors/nit_posts.rds") # Nitrogen concentrations in g per kg wet mass
phos_posts <- readRDS("posteriors/phos_posts.rds") # Phosphorous concentrations in g per kg wet mass
epa_posts <- readRDS("posteriors/epa_posts.rds") # EPA concentrations in g per kg wet mass
dha_posts <- readRDS("posteriors/dha_posts.rds") # DHA concentrations in g per kg wet mass
pbde_posts <- readRDS("posteriors/pbde_posts.rds") # DHA concentrations in ug per kg wet mass
ddt_posts <- readRDS("posteriors/ddt_posts.rds") # DHA concentrations in ug per kg wet mass


all_toplot <- readRDS(file = "data/derived_quantities/all_toplot.rds") #load or re-create with chunks 3-7

# 2) Plot Escapement Biomass -----------------------------------------------------
# BY SPECIES
#raw data
d_short <- readRDS("data/d_short.rds") %>% 
  separate(species, c("species", "family"))


d_toplot <- d_short %>% select(year, species, location, y) %>% 
  pivot_wider(names_from = species, values_from = y) %>% 
  mutate(Total = Pink + Chum + Sockeye + Chinook + Coho) %>% 
  pivot_longer(cols = c(-year, -location), names_to = "species") %>% 
  group_by(species, year) %>% 
  summarize(value = sum(value)) %>% 
  mutate(species = as.factor(str_trim(species)),
         species = fct_relevel(species, "Total", "Pink", "Sockeye", "Chum"))

#posterior predictions - estimate total salmon mass for species. Sum over locations
salmon_mass <- gam_salmon_posts %>% 
  select(-metric_tons, -value, -species_location, -family) %>% 
  #calculate total Hg and kg 
  pivot_wider(names_from = species, values_from = kg) %>% 
  mutate(Total = Chinook + Chum + Coho + Pink + Sockeye) %>% 
  pivot_longer(cols = c(-location, -year, -iter), names_to = "species", values_to = "kg") %>%
  mutate(metric_tons = kg*0.001)


salmon_mass_toplot <- salmon_mass %>% 
  group_by(species, year, iter) %>% 
  summarize(metric_tons = sum(metric_tons)) %>% 
  group_by(species, year) %>% 
  summarize(low = quantile(metric_tons, probs = 0.05),
            med = median(metric_tons),
            upper = quantile(metric_tons, probs = 0.95),
            low50 = quantile(metric_tons, probs = 0.25),
            upper50 = quantile(metric_tons, probs = 0.75)) %>% 
  mutate(species = as.factor(str_trim(species)),
         species = fct_relevel(species, "Total", "Pink", "Sockeye", "Chum"))


total_escapement <- salmon_mass_toplot %>% 
  ggplot(aes(x = year)) + 
  geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.2) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.2) +
  geom_line(aes(y = med)) +
  facet_grid(.~species) +
  labs(y = "Escapement: Metric tons per year",
       x = "Year") +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  scale_y_continuous(label = comma) +
  theme(text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  geom_point(data = d_toplot %>% group_by(species, year) %>% 
               summarize(value = sum(value)),
             aes(y = value)) 

total_escapement <- readRDS("plots/total_escapement.rds")
saveRDS(total_escapement, file = "plots/total_escapement.rds")
ggsave(total_escapement, file = "plots/total_escapement.jpg", dpi = 500, width = 9, height = 2)


# BY LOCATION

salmon_mass_toplot_loc <- salmon_mass %>% 
  filter(species == "Total") %>% 
  select(location, year, species, iter, metric_tons) %>%
  pivot_wider(names_from = location, values_from = metric_tons) %>% 
  mutate(Total = BCWC + BeringSea + CentralAK + SEAK) %>% 
  pivot_longer(cols = c(-year, -species, -iter), names_to = "location", values_to = "metric_tons") %>% 
  group_by(location, year) %>% 
  summarize(low = quantile(metric_tons, probs = 0.05),
            med = median(metric_tons),
            upper = quantile(metric_tons, probs = 0.95),
            low50 = quantile(metric_tons, probs = 0.25),
            upper50 = quantile(metric_tons, probs = 0.75)) %>% 
  mutate(location = as.factor(str_trim(location)),
         location = fct_relevel(location, "Total", "BCWC", "SEAK"))

d_toplot_loc <- d_short %>% select(year, species, location, y) %>% 
  pivot_wider(names_from = location, values_from = y) %>% 
  mutate(Total = BCWC + BeringSea + CentralAK + SEAK) %>% 
  pivot_longer(cols = c(-year, -species), names_to = "location") %>% 
  group_by(location, year) %>% 
  summarize(value = sum(value)) %>% 
  mutate(location = as.factor(str_trim(location)),
         location = fct_relevel(location, "Total", "BCWC", "SEAK"))


total_escapement_loc <- salmon_mass_toplot_loc %>% 
  ggplot(aes(x = year)) + 
  geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.2) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.2) +
  geom_line(aes(y = med)) +
  facet_grid(.~location) +
  labs(y = "Escapement: Metric tons per year",
       x = "Year") +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  scale_y_continuous(label = comma) +
  theme(text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  geom_point(data = d_toplot_loc %>% group_by(location, year) %>% 
               summarize(value = sum(value)),
             aes(y = value)) 


# 3) Estimate Escapement Hg ----------------------------------------------------------------

# Monte Carlo combine posteriors hg and salmon mass to get kg of mercury in escapement
n = 100

kg_hg <- gam_salmon_posts %>% filter(iter <= n) %>% 
  rename(iter_salmon = iter) %>% 
  left_join(hg_posts %>% filter(iter <= n) %>% rename(iter_hg = iter)) %>% 
  mutate(kghg = ug_kg_ww*kg/1e+09) %>% 
  select(species, location, year, iter_salmon, iter_hg, kghg) 


kg_hg_withtotal <- kg_hg %>% 
  group_by(species, year, iter_salmon, iter_hg) %>% 
  summarize(total = sum(kghg)) %>% 
  pivot_wider(names_from = species, values_from = total) %>% 
  mutate(Total = Chinook + Chum + Coho + Pink + Sockeye) %>% 
  pivot_longer(cols = c(-year, -iter_salmon, -iter_hg), names_to = "species", values_to = "kghg")


# 4) Estimate Escapement PCBs -----------------------------------------------------


# Monte Carlo combine posteriors  hg and salmon mass to get kg of mercury in escapement
n = 100

kg_pcb <- gam_salmon_posts %>% filter(iter <= n) %>% 
  rename(iter_salmon = iter) %>% 
  left_join(pcb_posts %>% filter(iter <= n) %>% rename(iter_pcb = iter)) %>% 
  mutate(kgpcb = ug_kg_ww*kg/1e+09) %>% 
  select(species, location, year, iter_salmon, iter_pcb, kgpcb) 


kg_pcb_withtotal <- kg_pcb %>% 
  group_by(species, year, iter_salmon, iter_pcb) %>% 
  summarize(total = sum(kgpcb)) %>% 
  pivot_wider(names_from = species, values_from = total) %>% 
  mutate(Total = Chinook + Chum + Coho + Pink + Sockeye) %>% 
  pivot_longer(cols = c(-year, -iter_salmon, -iter_pcb), names_to = "species", values_to = "kgpcb")


# 5) Estimate Escapement Nitrogen ---------------------------------------------

# Monte Carlo combine posteriors  hg and salmon mass to get kg of mercury in escapement
n = 100

kg_N <- gam_salmon_posts %>% filter(iter <= n) %>% 
  rename(iter_salmon = iter) %>% 
  left_join(nit_posts %>% filter(iter <= n) %>% rename(iter_N = iter)) %>% 
  mutate(kgN = g_kg_N*kg/1000) %>% 
  select(species, location, year, iter_salmon, iter_N, kgN) 


kg_N_withtotal <- kg_N %>% 
  group_by(species, year, iter_salmon, iter_N) %>% 
  summarize(total = sum(kgN)) %>% 
  pivot_wider(names_from = species, values_from = total) %>% 
  mutate(Total = Chinook + Chum + Coho + Pink + Sockeye) %>% 
  pivot_longer(cols = c(-year, -iter_salmon, -iter_N), names_to = "species", values_to = "kgN")

# 6) Estimate Escapement Phos ---------------------------------------------

# Monte Carlo combine posteriors  hg and salmon mass to get kg of mercury in escapement
n = 100

kg_P <- gam_salmon_posts %>% filter(iter <= n) %>% 
  rename(iter_salmon = iter) %>% 
  left_join(phos_posts %>% filter(iter <= n) %>% rename(iter_P = iter)) %>% 
  mutate(kgP = g_kg_ww*kg/1000) %>% 
  select(species, location, year, iter_salmon, iter_P, kgP) 


kg_P_withtotal <- kg_P %>% 
  group_by(species, year, iter_salmon, iter_P) %>% 
  summarize(total = sum(kgP)) %>% 
  pivot_wider(names_from = species, values_from = total) %>% 
  mutate(Total = Chinook + Chum + Coho + Pink + Sockeye) %>% 
  pivot_longer(cols = c(-year, -iter_salmon, -iter_P), names_to = "species", values_to = "kgP")

# 6b) Estimate Escapement DHA ---------------------------------------------
# Monte Carlo combine posteriors
n = 100

kg_dha <- gam_salmon_posts %>% filter(iter <= n) %>% 
  rename(iter_salmon = iter) %>% 
  left_join(dha_posts %>% filter(iter <= n) %>% rename(iter_dha = iter)) %>% 
  mutate(kgdha = g_kg_ww*kg/1000) %>% 
  select(species, location, year, iter_salmon, iter_dha, kgdha) 


kg_dha_withtotal <- kg_dha %>% 
  group_by(species, year, iter_salmon, iter_dha) %>% 
  summarize(total = sum(kgdha)) %>% 
  pivot_wider(names_from = species, values_from = total) %>% 
  mutate(Total = Chinook + Chum + Coho + Pink + Sockeye) %>% 
  pivot_longer(cols = c(-year, -iter_salmon, -iter_dha), names_to = "species", values_to = "kgdha")

# 6c) Estimate Escapement EPA ---------------------------------------------
# Monte Carlo combine posteriors
n = 100

kg_epa <- gam_salmon_posts %>% filter(iter <= n) %>% 
  rename(iter_salmon = iter) %>% 
  left_join(epa_posts %>% filter(iter <= n) %>% rename(iter_epa = iter)) %>% 
  mutate(kgepa = g_kg_ww*kg/1000) %>% 
  select(species, location, year, iter_salmon, iter_epa, kgepa) 


kg_epa_withtotal <- kg_epa %>% 
  group_by(species, year, iter_salmon, iter_epa) %>% 
  summarize(total = sum(kgepa)) %>% 
  pivot_wider(names_from = species, values_from = total) %>% 
  mutate(Total = Chinook + Chum + Coho + Pink + Sockeye) %>% 
  pivot_longer(cols = c(-year, -iter_salmon, -iter_epa), names_to = "species", values_to = "kgepa")


# 6d) Estimate Escapement PBDE ---------------------------------------------
# Monte Carlo combine posteriors
n = 100

kg_pbde <- gam_salmon_posts %>% filter(iter <= n) %>% 
  rename(iter_salmon = iter) %>% 
  left_join(pbde_posts %>% filter(iter <= n) %>% rename(iter_pbde = iter)) %>% 
  mutate(kgpbde = ug_kg_ww*kg/1e+09) %>% 
  select(species, location, year, iter_salmon, iter_pbde, kgpbde) 


kg_pbde_withtotal <- kg_pbde %>% 
  group_by(species, year, iter_salmon, iter_pbde) %>% 
  summarize(total = sum(kgpbde)) %>% 
  pivot_wider(names_from = species, values_from = total) %>% 
  mutate(Total = Chinook + Chum + Coho + Pink + Sockeye) %>% 
  pivot_longer(cols = c(-year, -iter_salmon, -iter_pbde), names_to = "species", values_to = "kgpbde")

# 6e) Estimate Escapement DDT ---------------------------------------------
# Monte Carlo combine posteriors
n = 100

kg_ddt <- gam_salmon_posts %>% filter(iter <= n) %>% 
  rename(iter_salmon = iter) %>% 
  left_join(ddt_posts %>% filter(iter <= n) %>% rename(iter_ddt = iter)) %>% 
  mutate(kgddt = ug_kg_ww*kg/1e+09) %>% 
  select(species, location, year, iter_salmon, iter_ddt, kgddt) 


kg_ddt_withtotal <- kg_ddt %>% 
  group_by(species, year, iter_salmon, iter_ddt) %>% 
  summarize(total = sum(kgddt)) %>% 
  pivot_wider(names_from = species, values_from = total) %>% 
  mutate(Total = Chinook + Chum + Coho + Pink + Sockeye) %>% 
  pivot_longer(cols = c(-year, -iter_salmon, -iter_ddt), names_to = "species", values_to = "kgddt")



# 7) Make plot data--------------------------------------------------------------

P_toplot <- kg_P_withtotal %>% 
  group_by(species, year) %>% 
  summarize(low = quantile(kgP, probs = 0.05),
            med = median(kgP),
            upper = quantile(kgP, probs = 0.95),
            low50 = quantile(kgP, probs = 0.25),
            upper50 = quantile(kgP, probs = 0.75)) %>% 
  mutate(rank_species = case_when(species == "All" ~ "a. Total",
                                  species == "Chinook" ~ "e. Chinook",
                                  species == "Pink" ~ "b. Pink",
                                  species == "Chum" ~ "c. Chum",
                                  species == "Sockeye" ~ "d. Sockeye",
                                  TRUE ~ "f. Coho"),
         chemical = "P") 

N_toplot <- kg_N_withtotal %>% 
  group_by(species, year) %>% 
  summarize(low = quantile(kgN, probs = 0.05),
            med = median(kgN),
            upper = quantile(kgN, probs = 0.95),
            low50 = quantile(kgN, probs = 0.25),
            upper50 = quantile(kgN, probs = 0.75)) %>% 
  mutate(rank_species = case_when(species == "All" ~ "a. Total",
                                  species == "Chinook" ~ "e. Chinook",
                                  species == "Pink" ~ "b. Pink",
                                  species == "Chum" ~ "c. Chum",
                                  species == "Sockeye" ~ "d. Sockeye",
                                  TRUE ~ "f. Coho"),
         chemical = "N") 

PCB_toplot <- kg_pcb_withtotal %>% 
  group_by(species, year) %>% 
  summarize(low = quantile(kgpcb, probs = 0.05),
            med = median(kgpcb),
            upper = quantile(kgpcb, probs = 0.95),
            low50 = quantile(kgpcb, probs = 0.25),
            upper50 = quantile(kgpcb, probs = 0.75)) %>% 
  mutate(rank_species = case_when(species == "All" ~ "a. Total",
                                  species == "Chinook" ~ "e. Chinook",
                                  species == "Pink" ~ "b. Pink",
                                  species == "Chum" ~ "c. Chum",
                                  species == "Sockeye" ~ "d. Sockeye",
                                  TRUE ~ "f. Coho"),
         chemical = "PCBs") 

hg_toplot <- kg_hg_withtotal %>% 
  group_by(species, year) %>% 
  summarize(low = quantile(kghg, probs = 0.05),
            med = median(kghg),
            upper = quantile(kghg, probs = 0.95),
            low50 = quantile(kghg, probs = 0.25),
            upper50 = quantile(kghg, probs = 0.75)) %>% 
  mutate(rank_species = case_when(species == "All" ~ "a. Total",
                                  species == "Chinook" ~ "e. Chinook",
                                  species == "Pink" ~ "b. Pink",
                                  species == "Chum" ~ "c. Chum",
                                  species == "Sockeye" ~ "d. Sockeye",
                                  TRUE ~ "f. Coho"),
         chemical = "Hg") 

epa_toplot <- kg_epa_withtotal %>% 
  group_by(species, year) %>% 
  summarize(low = quantile(kgepa, probs = 0.05),
            med = median(kgepa),
            upper = quantile(kgepa, probs = 0.95),
            low50 = quantile(kgepa, probs = 0.25),
            upper50 = quantile(kgepa, probs = 0.75)) %>% 
  mutate(rank_species = case_when(species == "All" ~ "a. Total",
                                  species == "Chinook" ~ "e. Chinook",
                                  species == "Pink" ~ "b. Pink",
                                  species == "Chum" ~ "c. Chum",
                                  species == "Sockeye" ~ "d. Sockeye",
                                  TRUE ~ "f. Coho"),
         chemical = "EPA") 

dha_toplot <- kg_dha_withtotal %>% 
  group_by(species, year) %>% 
  summarize(low = quantile(kgdha, probs = 0.125),
            med = median(kgdha),
            upper = quantile(kgdha, probs = 0.875),
            low50 = quantile(kgdha, probs = 0.25),
            upper50 = quantile(kgdha, probs = 0.75)) %>% 
  mutate(rank_species = case_when(species == "All" ~ "a. Total",
                                  species == "Chinook" ~ "e. Chinook",
                                  species == "Pink" ~ "b. Pink",
                                  species == "Chum" ~ "c. Chum",
                                  species == "Sockeye" ~ "d. Sockeye",
                                  TRUE ~ "f. Coho"),
         chemical = "DHA") 

ddt_toplot <- kg_ddt_withtotal %>% 
  group_by(species, year) %>% 
  summarize(low = quantile(kgddt, probs = 0.125),
            med = median(kgddt),
            upper = quantile(kgddt, probs = 0.875),
            low50 = quantile(kgddt, probs = 0.25),
            upper50 = quantile(kgddt, probs = 0.75)) %>% 
  mutate(rank_species = case_when(species == "All" ~ "a. Total",
                                  species == "Chinook" ~ "e. Chinook",
                                  species == "Pink" ~ "b. Pink",
                                  species == "Chum" ~ "c. Chum",
                                  species == "Sockeye" ~ "d. Sockeye",
                                  TRUE ~ "f. Coho"),
         chemical = "DDT") 

pbde_toplot <- kg_pbde_withtotal %>% 
  group_by(species, year) %>% 
  summarize(low = quantile(kgpbde, probs = 0.125),
            med = median(kgpbde),
            upper = quantile(kgpbde, probs = 0.875),
            low50 = quantile(kgpbde, probs = 0.25),
            upper50 = quantile(kgpbde, probs = 0.75)) %>% 
  mutate(rank_species = case_when(species == "All" ~ "a. Total",
                                  species == "Chinook" ~ "e. Chinook",
                                  species == "Pink" ~ "b. Pink",
                                  species == "Chum" ~ "c. Chum",
                                  species == "Sockeye" ~ "d. Sockeye",
                                  TRUE ~ "f. Coho"),
         chemical = "PBDE") 

# combine
all_toplot <- bind_rows(P_toplot,
          N_toplot,
          PCB_toplot,
          hg_toplot,
          epa_toplot, 
          dha_toplot,
          ddt_toplot,
          pbde_toplot) %>% 
  mutate(species = as.factor(str_trim(species)),
         species = fct_relevel(species, "Total", "Pink", "Sockeye", "Chum"),
         chemical = as.factor(str_trim(chemical)),
         chemical = fct_relevel(chemical, "N", "P", "Hg")) %>% 
  mutate(type = case_when(chemical %in% c("P", "N", "EPA", "DHA") ~ "Nutrients",
                          TRUE ~ "Contaminants"),
         group = case_when(species == "Total" ~ "Total", TRUE ~ "Species")) 

saveRDS(all_toplot, file = "data/derived_quantities/all_toplot.rds")

summaries_for_plots <- all_toplot %>% 
  group_by(species, chemical) %>% 
  summarize(total_median = sum(med),
            median_median = median(med),
            max = max(upper)) %>% 
  mutate(year = max(all_toplot$year)) %>% 
  mutate(type = case_when(chemical %in% c("P", "N", "EPA", "DHA") ~ "Nutrients",
                          TRUE ~ "Contaminants"),
         group = case_when(species == "Total" ~ "Total", TRUE ~ "Species")) %>% 
  mutate(median_median = round(median_median, 0),
         total_median = round(total_median, 0),
         median_tons = median_median/1000,
         total_tons = total_median/1000)

# 7b) Make plots--------------------------------------------------------------

totals_plot_nut <- all_toplot %>% 
  filter(species == "Total") %>%
  filter(type == "Nutrients") %>%
  filter(!is.na(chemical)) %>% 
  mutate(chemical = fct_relevel(chemical, "N", "P", "DHA", "EPA")) %>% 
  ggplot(aes(x = year)) + 
  geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.2) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.3) +
  geom_line(aes(y = med)) +
  # geom_text_repel(data = species_labels %>% 
  #                   filter(type == "Nutrients"), aes(y = med, label = species)) +
  facet_grid(chemical ~ ., scales = "free_y") +
  labs(y = "Kg per year exported via salmon escapement",
       fill = "Species",
       x = "Year",
       subtitle = "Total Export (kg/y)",
       title = "Nutrients") +
  # scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015, 2020)) +
  scale_y_continuous(labels = comma) +
  scale_fill_viridis_d(direction = 1, option = 'A') +
  scale_color_colorblind() +
  theme(text = element_text(size = 11),
        axis.title = element_text(size = 11)) +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_blank()) +
  geom_text(data = summaries_for_plots %>% filter(type == "Nutrients"&
                                                    group == "Total"),
            aes(label = paste0("Cumulative kg: ", scientific(total_median)),
                y = max + 0.07*max,
                x = year - 15),
            size = 2.5) +
  geom_text(data = summaries_for_plots %>% filter(type == "Nutrients"&
                                                    group == "Total"),
            aes(label = paste0("Median kg: ", scientific(median_median)),
                y = max + 0.25*max,
                x = year - 15),
            size = 2.5) +
  NULL

totals_plot_cont <- all_toplot %>% 
  filter(species == "Total") %>%
  filter(type != "Nutrients") %>%
  filter(!is.na(chemical)) %>% 
  ggplot(aes(x = year)) + 
  geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.2) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.3) +
  geom_line(aes(y = med)) +
  # geom_text_repel(data = species_labels %>% 
  #                   filter(type == "Nutrients"), aes(y = med, label = species)) +
  facet_grid(chemical ~ ., scales = "free_y") +
  labs(y = "Kg per year exported via salmon escapement",
       fill = "Species",
       x = "Year",
       title = "Contaminants") +
  # scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015, 2020)) +
  scale_y_continuous(labels = comma) +
  scale_fill_viridis_d(direction = 1, option = 'A') +
  scale_color_colorblind() +
  theme(text = element_text(size = 11),
        axis.title = element_text(size = 11)) +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_blank()) +
  geom_text(data = summaries_for_plots %>% filter(type == "Contaminants") %>% 
              filter(group == "Total"),
            aes(label = paste0("Cumulative kg: ", scientific(total_median)),
                y = max + 0.07*max,
                x = year - 15),
            size = 2.5) +
  geom_text(data = summaries_for_plots %>% filter(type != "Nutrients"&
                                                    group == "Total"),
            aes(label = paste0("Median kg: ", scientific(median_median)),
                y = max + 0.25*max,
                x = year - 15),
            size = 2.5) +
  NULL


species_labels <- all_toplot %>% 
  mutate(type = case_when(chemical %in% c("P", "N", "EPA", "DHA") ~ "Nutrients",
                          TRUE ~ "Contaminants"),
         group = case_when(species == "Total" ~ "Total", TRUE ~ "Species")) %>% 
  filter(species != "Total") %>%
  # filter(type == "Nutrients") %>% 
  filter(!is.na(chemical)) %>% 
  mutate(chemical = fct_relevel(chemical, "N", "P", "DHA", "EPA")) %>%
  filter(year == max(year)) %>% 
  mutate(year = year)



species_plot_nut <- all_toplot %>% 
  mutate(type = case_when(chemical %in% c("P", "N", "EPA", "DHA") ~ "Nutrients",
                          TRUE ~ "Contaminants"),
         group = case_when(species == "Total" ~ "Total", TRUE ~ "Species")) %>% 
  filter(species != "Total") %>%
  filter(type == "Nutrients") %>%
  filter(!is.na(chemical)) %>% 
  mutate(chemical = fct_relevel(chemical, "N", "P", "DHA", "EPA"),
         species = fct_relevel(species, "Coho", "Chinook", "Chum", "Sockeye")) %>% 
  ggplot(aes(x = year, fill = species)) + 
  # geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.2) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.7) +
  geom_line(aes(y = med)) +
  # geom_text_repel(data = species_labels, aes(y = med, color = species, label = species),
  #                 nudge_x = 5) +
  facet_grid(chemical ~ ., scales = "free_y") +
  labs(y = "Kg per year exported via salmon escapement",
       fill = "Species",
       x = "Year",
       subtitle = "Export by Species (kg/y)",
       title = "") +
  # scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015, 2020)) +
  scale_y_continuous(labels = comma) +
  scale_fill_viridis_d(direction = -1, option = 'A',
                       guide = guide_legend(reverse = TRUE)) +
  scale_color_viridis_d(direction = 1, option = 'A') +
  theme(text = element_text(size = 11),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 11)) +
  NULL


species_plot_cont <- all_toplot %>% 
  mutate(type = case_when(chemical %in% c("P", "N", "EPA", "DHA") ~ "Nutrients",
                          TRUE ~ "Contaminants"),
         group = case_when(species == "Total" ~ "Total", TRUE ~ "Species")) %>% 
  filter(species != "Total") %>%
  filter(type != "Nutrients") %>%
  filter(!is.na(chemical)) %>% 
  mutate(species = fct_relevel(species, "Coho", "Chinook", "Chum", "Sockeye")) %>% 
  ggplot(aes(x = year, fill = species)) + 
  # geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.2) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.7) +
  geom_line(aes(y = med)) +
  # geom_text_repel(data = species_labels, aes(y = med, color = species, label = species),
  #                 nudge_x = 5) +
  facet_grid(chemical ~ ., scales = "free_y") +
  labs(y = "Kg per year exported via salmon escapement",
       fill = "Species",
       x = "Year",
       subtitle = "") +
  # scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015, 2020)) +
  scale_y_continuous(labels = comma) +
  scale_fill_viridis_d(direction = -1, option = 'A',
                       guide = guide_legend(reverse = TRUE)) +
  scale_color_viridis_d(direction = 1, option = 'A') +
  theme(text = element_text(size = 11),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 11)) +
  NULL

species_legend <- get_legend(species_plot_nut)

totals_all <- plot_grid(tag_facet(totals_plot_nut + theme(axis.title.x = element_blank()),
                                  tag_pool = c("a", "c", "e", "g"),
                                  open = "", 
                                  close = ")",
                                  size = 2.5), 
                        tag_facet(totals_plot_cont,
                        tag_pool = c("i", "k", "m", "o"),
                        open = "", close = ")",
                        size = 2.5,
                        # y = -Inf, 
                        vjust = 1, 
                        hjust = -0.3),
                        align = "v",
                        ncol = 1)

species_all <- plot_grid(tag_facet(species_plot_nut + theme(axis.title.x = element_blank()) +
                           guides(fill = F) +
                           labs(y = ""),
                           tag_pool = c("b", "d", "f", "h"),
                           open = "", 
                           close = ")",
                           size = 2.5) + 
                           theme(strip.text = element_text()), 
                        tag_facet(species_plot_cont +
                          labs(y = "") + 
                          guides(fill = F),
                        tag_pool = c("j", "l", "n", "p"),
                        open = "", close = ")",
                        size = 2.5,
                        x = -Inf, 
                        vjust = 1, 
                        hjust = -0.3) +
                          theme(strip.text = element_text()),
                        align = "v",
                        ncol = 1)

flux_fig_time <- plot_grid(totals_all,
                           species_all,
                           species_legend,
                           ncol = 3,
                           rel_widths = c(0.4, 0.4, 0.15))


saveRDS(flux_fig_time, file = "plots/flux_fig_time.rds")
ggsave(flux_fig_time, file = "plots/flux_fig_time.jpg", dpi = 500, width = 6, height = 9)



# 8) Summarize Everything -------------------------------------------------

all_toplot %>% 
  group_by(species, chemical) %>% 
  filter(med == max(med)) %>% 
  arrange(chemical, med)


# 9) Contaminants Among Species ----------------------------
