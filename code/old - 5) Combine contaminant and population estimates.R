library(brms)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(cowplot)

theme_set(theme_default())

# 1) bring in posteriors -----------------------------------------------------

gam_salmon_posts <- readRDS("posteriors/gam_salmon_posts.rds") # Salmon escapement in kg wet mass
hg_posts <- readRDS("posteriors/hg_posts.rds") # Mercury concentrations in ug per kg wet mass
pcb_posts <- readRDS("posteriors/pcb_posts.rds") # PCB concentrations in ug per kg wet mass
nit_posts <- readRDS("posteriors/nit_posts.rds") # Nitrogen concentrations in g per kg wet mass
phos_posts <- readRDS("posteriors/phos_posts.rds") # Phosphorous concentrations in g per kg wet mass



# 2) Plot Escapement Biomass -----------------------------------------------------

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
  scale_y_continuous(labels = comma) +
  theme(text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  geom_point(data = d_toplot,
             aes(y = value)) 


saveRDS(total_escapement, file = "plots/total_escapement.rds")
ggsave()

# 3) Plot Escapement Hg ----------------------------------------------------------------
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


# make plot
total_hg_plot <- kg_hg_withtotal %>% 
  filter(species == "Total") %>% 
  group_by(species, year) %>% 
  summarize(low = quantile(kghg, probs = 0.05),
            med = median(kghg),
            upper = quantile(kghg, probs = 0.95),
            low50 = quantile(kghg, probs = 0.25),
            upper50 = quantile(kghg, probs = 0.75)) %>% 
  mutate(rank_species = "a. Total") %>% 
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.2) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.2) +
  geom_line(aes(y = med, group = species)) +
  labs(y = "kg Hg per year") +
  scale_fill_viridis_d(direction = -1) +
  scale_color_viridis_d(direction = -1) +
  facet_grid(.~rank_species) +
  guides(fill = F,
         color = F) +
  theme_default() +
  NULL

species_hg_plot <- kg_hg_withtotal %>% 
  filter(species != "Total") %>% 
  group_by(species, year) %>% 
  summarize(low = quantile(kghg, probs = 0.05),
            med = median(kghg),
            upper = quantile(kghg, probs = 0.95),
            low50 = quantile(kghg, probs = 0.25),
            upper50 = quantile(kghg, probs = 0.75)) %>% 
  mutate(rank_species = case_when(species == "Chinook" ~ "e. Chinook",
                                  species == "Pink" ~ "b. Pink",
                                  species == "Chum" ~ "c. Chum",
                                  species == "Sockeye" ~ "d. Sockeye",
                                  TRUE ~ "f. Coho")) %>% 
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.2) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.2) +
  geom_line(aes(y = med, group = species)) +
  labs(y = "kg Hg per year") +
  scale_fill_viridis_d(direction = -1) +
  scale_color_viridis_d(direction = -1) +
  facet_grid(.~rank_species) +
  guides(fill = F,
         color = F) +
  theme_default() +
  NULL


totalhg_title <- ggdraw() + draw_label("Hg exported summed over all locations", fontface='bold')
totalhg_plot <- plot_grid(total_hg_plot, species_hg_plot, ncol = 2,
                          rel_widths = c(1, 3))
totalhg_title_plot <- plot_grid(totalhg_title, totalhg_plot, ncol = 1, rel_heights = c(0.2, 1))

mass_hg_plot <- plot_grid(total_title_plot, totalhg_title_plot, ncol = 1, align = "h")

saveRDS(totalhg_title_plot, file = "plots/totalhg_title_plot.rds")


# 4) Plot Escapement PCBs -----------------------------------------------------


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


# make plot
total_pcb_plot <- kg_pcb_withtotal %>% 
  filter(species == "Total") %>% 
  group_by(species, year) %>% 
  summarize(low = quantile(kgpcb, probs = 0.05),
            med = median(kgpcb),
            upper = quantile(kgpcb, probs = 0.95),
            low50 = quantile(kgpcb, probs = 0.25),
            upper50 = quantile(kgpcb, probs = 0.75)) %>% 
  mutate(rank_species = "a. Total") %>% 
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.2) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.2) +
  geom_line(aes(y = med, group = species)) +
  labs(y = "kg PCBs per year") +
  scale_fill_viridis_d(direction = -1) +
  scale_color_viridis_d(direction = -1) +
  facet_grid(.~rank_species) +
  guides(fill = F,
         color = F) +
  theme_default() +
  NULL

species_pcb_plot <- kg_pcb_withtotal %>% 
  filter(species != "Total") %>% 
  group_by(species, year) %>% 
  summarize(low = quantile(kgpcb, probs = 0.05),
            med = median(kgpcb),
            upper = quantile(kgpcb, probs = 0.95),
            low50 = quantile(kgpcb, probs = 0.25),
            upper50 = quantile(kgpcb, probs = 0.75)) %>% 
  mutate(rank_species = case_when(species == "Chinook" ~ "e. Chinook",
                                  species == "Pink" ~ "b. Pink",
                                  species == "Chum" ~ "c. Chum",
                                  species == "Sockeye" ~ "d. Sockeye",
                                  TRUE ~ "f. Coho")) %>% 
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.2) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.2) +
  geom_line(aes(y = med, group = species)) +
  labs(y = "kg PCBs per year") +
  scale_fill_viridis_d(direction = -1) +
  scale_color_viridis_d(direction = -1) +
  facet_grid(.~rank_species) +
  guides(fill = F,
         color = F) +
  theme_default() +
  NULL


totalpcb_title <- ggdraw() + draw_label("PCBs exported summed over all locations", fontface='bold')
totalpcb_plot <- plot_grid(total_pcb_plot, species_pcb_plot, ncol = 2,
                          rel_widths = c(1, 3))
totalpcb_title_plot <- plot_grid(totalpcb_title, totalpcb_plot, ncol = 1, rel_heights = c(0.2, 1))

saveRDS(totalpcb_title_plot, file = "plots/totalpcb_title_plot.rds")


# 5) Plot Escapement Nitrogen ---------------------------------------------

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


# make plot
total_N_plot <- kg_N_withtotal %>% 
  filter(species == "Total") %>% 
  group_by(species, year) %>% 
  summarize(low = quantile(kgN, probs = 0.05),
            med = median(kgN),
            upper = quantile(kgN, probs = 0.95),
            low50 = quantile(kgN, probs = 0.25),
            upper50 = quantile(kgN, probs = 0.75)) %>% 
  mutate(rank_species = "a. Total") %>% 
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.2) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.2) +
  geom_line(aes(y = med, group = species)) +
  labs(y = "kg N per year") +
  scale_fill_viridis_d(direction = -1) +
  scale_color_viridis_d(direction = -1) +
  facet_grid(.~rank_species) +
  guides(fill = F,
         color = F) +
  theme_default() +
  NULL

species_N_plot <- kg_N_withtotal %>% 
  filter(species != "Total") %>% 
  group_by(species, year) %>% 
  summarize(low = quantile(kgN, probs = 0.05),
            med = median(kgN),
            upper = quantile(kgN, probs = 0.95),
            low50 = quantile(kgN, probs = 0.25),
            upper50 = quantile(kgN, probs = 0.75)) %>% 
  mutate(rank_species = case_when(species == "Chinook" ~ "e. Chinook",
                                  species == "Pink" ~ "b. Pink",
                                  species == "Chum" ~ "c. Chum",
                                  species == "Sockeye" ~ "d. Sockeye",
                                  TRUE ~ "f. Coho")) %>% 
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.2) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.2) +
  geom_line(aes(y = med, group = species)) +
  labs(y = "kg N per year") +
  scale_fill_viridis_d(direction = -1) +
  scale_color_viridis_d(direction = -1) +
  facet_grid(.~rank_species) +
  guides(fill = F,
         color = F) +
  theme_default() +
  NULL


totalN_title <- ggdraw() + draw_label("N exported summed over all locatioN", fontface='bold')
totalN_plot <- plot_grid(total_N_plot, species_N_plot, ncol = 2,
                           rel_widths = c(1, 3))
totalN_title_plot <- plot_grid(totalN_title, totalN_plot, ncol = 1, rel_heights = c(0.2, 1))

saveRDS(totalN_title_plot, file = "plots/totalN_title_plot.rds")


# 6) Plot Escapement Phos ---------------------------------------------

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


# make plot
total_P_plot <- kg_P_withtotal %>% 
  filter(species == "Total") %>% 
  group_by(species, year) %>% 
  summarize(low = quantile(kgP, probs = 0.05),
            med = median(kgP),
            upper = quantile(kgP, probs = 0.95),
            low50 = quantile(kgP, probs = 0.25),
            upper50 = quantile(kgP, probs = 0.75)) %>% 
  mutate(rank_species = "a. Total") %>% 
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.2) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.2) +
  geom_line(aes(y = med, group = species)) +
  labs(y = "kg P per year") +
  scale_fill_viridis_d(direction = -1) +
  scale_color_viridis_d(direction = -1) +
  facet_grid(.~rank_species) +
  guides(fill = F,
         color = F) +
  theme_default() +
  NULL

species_P_plot <- kg_P_withtotal %>% 
  filter(species != "Total") %>% 
  group_by(species, year) %>% 
  summarize(low = quantile(kgP, probs = 0.05),
            med = median(kgP),
            upper = quantile(kgP, probs = 0.95),
            low50 = quantile(kgP, probs = 0.25),
            upper50 = quantile(kgP, probs = 0.75)) %>% 
  mutate(rank_species = case_when(species == "Chinook" ~ "e. Chinook",
                                  species == "Pink" ~ "b. Pink",
                                  species == "Chum" ~ "c. Chum",
                                  species == "Sockeye" ~ "d. Sockeye",
                                  TRUE ~ "f. Coho")) %>% 
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.2) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.2) +
  geom_line(aes(y = med, group = species)) +
  labs(y = "kg P per year") +
  scale_fill_viridis_d(direction = -1) +
  scale_color_viridis_d(direction = -1) +
  facet_grid(.~rank_species) +
  guides(fill = F,
         color = F) +
  theme_default() +
  NULL


totalP_title <- ggdraw() + draw_label("P exported summed over all locatioN", fontface='bold')
totalP_plot <- plot_grid(total_P_plot, species_P_plot, ncol = 2,
                         rel_widths = c(1, 3))
totalP_title_plot <- plot_grid(totalP_title, totalP_plot, ncol = 1, rel_heights = c(0.2, 1))

saveRDS(totalP_title_plot, file = "plots/totalP_title_plot.rds")



# 7) Plot Everything --------------------------------------------------------------

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


# combine
all_toplot <- bind_rows(P_toplot,
          N_toplot,
          PCB_toplot,
          hg_toplot) %>% 
  mutate(species = as.factor(str_trim(species)),
         species = fct_relevel(species, "Total", "Pink", "Sockeye", "Chum"),
         chemical = as.factor(str_trim(chemical)),
         chemical = fct_relevel(chemical, "N", "P", "Hg")) 

all_chemicals <- all_toplot %>% 
  ggplot(aes(x = year, fill = chemical)) + 
  geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.2) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.2) +
  geom_line(aes(y = med)) +
  facet_grid(chemical~species, scales = "free") +
  labs(y = "Kg per year exported via salmon escapement",
       fill = "Chemical",
       x = "Year") +
  # scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  scale_y_continuous(labels = comma) +
  scale_fill_colorblind() +
  theme(text = element_text(size = 12),
        axis.title = element_text(size = 14))

saveRDS(all_chemicals, file = "plots/all_chemicals.rds")
ggsave(all_chemicals, file = "plots/all_chemicals.jpg", dpi = 500, width = 10, height = 6)

# 8) Summarize Everything -------------------------------------------------

all_toplot %>% 
  group_by(species, chemical) %>% 
  filter(med == max(med)) %>% 
  arrange(chemical, med)
