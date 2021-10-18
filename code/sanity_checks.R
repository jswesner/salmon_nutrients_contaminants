library(tidyverse)

# load data
nut_cont <- readRDS("data/nut_cont.rds")
d <- read_csv("data/salmon_metric_tons.csv") %>% 
  pivot_longer(cols = c(-units, -Year), names_to = "name", values_to = "y") %>% 
  separate(name, c("location", "species"), sep = "_", remove = F) %>% 
  separate(species, c("species", "family")) %>% 
  mutate(location = str_replace(location, " ", "")) %>% 
  clean_names() %>% 
  drop_na(y) %>% 
  group_by(name) %>% 
  mutate(time = year - min(year) + 1) %>% 
  ungroup() %>% 
  mutate(y_10000 = y/10000,
         y_kg = y*1000)

# load posteriors
flux_fig_time <- readRDS(file = "plots/flux_fig_time.rds")
all_chem_posts <- readRDS(file = "data/derived_quantities/all_chem_posts.rds")


# plot posteriors against raw data
ggplot() +
  geom_violin(data = all_chem_posts, aes(x = species, y = .epred)) + 
  geom_point(data = nut_cont %>% 
               mutate(chemical = case_when(chemical == "PBDEs" ~ "PBDE",
                                chemical == "DDTs" ~ "DDT",
                                TRUE ~ chemical)), 
             aes(x = species, y = mean_concentration_standardized)) +
  facet_wrap(~chemical , scales = "free_y")  +
  scale_y_log10()







# raw data estimates of chemical flux
chem_escape <- d %>% 
  filter(year > 1975) %>% 
  group_by(location, year, species) %>%
  summarize(mean_kg = median(y_kg))

mean_raw_contaminants <- nut_cont %>% group_by(chemical, species) %>% 
  summarize(mean_mgkg = median(mean_concentration_standardized))

raw_flux <- chem_escape %>% left_join(mean_raw_contaminants) %>% 
  mutate(mg_chem = mean_kg*mean_mgkg,
         kg_chem = mg_chem/1e6,
         source = "raw data") %>% 
  select(species, location, year, source, kg_chem, chemical)


median_modeled_flux <- flux_predictions %>% 
  ungroup() %>% 
  group_by(species, location, year, chemical) %>% 
  summarize(kg_chem = median(chem_flux_mg/1e6),
            kg_chem_sd = sd(chem_flux_mg/1e6)) %>% 
  mutate(source = "model") %>% 
  select(species, location, year, source, kg_chem, chemical, kg_chem_sd) %>% 
  mutate(chemical = case_when(chemical == "DDT" ~ "DDTs",
                              chemical == "PBDE" ~ "PBDEs",
                              TRUE ~ chemical))

raw_modeled_flux <- bind_rows(raw_flux, median_modeled_flux)

# check overlap of raw data esitmates to modeled estimates
raw_modeled_flux %>% 
  filter(species == "Pink") %>% 
  ggplot(aes(x = year, y = kg_chem, fill = interaction(source, location)))  +
  geom_ribbon(aes(ymin = kg_chem - kg_chem_sd, ymax = kg_chem + kg_chem_sd)) + 
  geom_line() + 
  facet_wrap(chemical~location, scales = "free_y", ncol = 4)


