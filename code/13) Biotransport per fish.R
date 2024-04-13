millions_fish = fish_escapement <- read_csv("data/raw_data/fish_escapement.csv") %>% clean_names() %>% 
  pivot_longer(cols = c(-year, -metric, -source)) %>% 
  mutate(species = case_when(grepl("pink", name) ~ "Pink",
                             grepl("chum", name) ~ "Chum",
                             grepl("sockeye", name) ~ "Sockeye",
                             grepl("chinook", name) ~ "Chinook",
                             TRUE ~ "Coho"),
         location = case_when(grepl("bering", name) ~ "BeringSea",
                              grepl("central", name) ~ "CentralAK",
                              grepl("seak", name) ~ "SEAK",
                              grepl("bc_wc", name) ~ "BCWC")) %>% 
  select(year, species, location, value) %>% 
  rename(millions_of_fish = value)

flux_predictions <- readRDS(file = "posteriors/flux_predictions.rds") # posterior chem export

mg_perfish = flux_predictions %>% 
  # filter(.draw == 1) %>% 
  left_join(millions_fish) %>% 
  mutate(mg_perfish = mg_flux/(millions_of_fish*1e6)) %>%  
  select(species, location, year, .draw, mg_perfish, millions_of_fish, mg_flux) %>% 
  mutate(type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "Nutrients",
                          TRUE ~ "Contaminants"))


mg_perfish %>% 
  group_by(location, .draw, year) %>% 
  reframe(kg_perfish = sum(mg_perfish)/1e6) %>% 
  group_by(location, .draw) %>% 
  reframe(kg_perfish_peryear = mean(kg_perfish)) %>% 
  group_by(location) %>% 
  median_qi(kg_perfish_peryear) %>% 
  arrange(-kg_perfish_peryear)


mg_perfish %>% 
  group_by(location, .draw, year, type) %>% 
  reframe(kg_perfish = sum(mg_perfish)/1e6) %>% 
  group_by(location, .draw, type) %>% 
  reframe(kg_perfish_peryear = mean(kg_perfish)) %>% 
  group_by(location, type) %>% 
  median_qi(kg_perfish_peryear) %>% 
  arrange(-kg_perfish_peryear)

mg_perfish %>% 
  group_by(location, .draw, year) %>% 
  reframe(kg_perfish = sum(mg_perfish)/1e6) %>% 
  group_by(location, .draw) %>% 
  reframe(kg_perfish_peryear = mean(kg_perfish)) %>% 
  group_by(location) %>% 
  median_qi(kg_perfish_peryear) %>% 
  arrange(-kg_perfish_peryear) %>% 
  ggplot(aes(x = reorder(location, -kg_perfish_peryear), y = kg_perfish_peryear)) +
  geom_pointrange(aes(ymin = .lower, ymax = .upper))


fig4_data = read_csv("plots/fig4_data.csv")

fig4_data %>% 
  filter(panel == "b)") %>% 
  group_by(species, .draw) %>% 
  reframe(mean = mean(Contaminants, na.rm = T)) %>% 
  group_by(species) %>% 
  mean_hdi(mean)

fig4_data %>% 
  filter(panel == "b)") %>% 
  group_by(species, .draw) %>% 
  reframe(mean = mean(Nutrients, na.rm = T)) %>% 
  group_by(species) %>% 
  mean_hdi(mean)
