library(tidyverse)
library(brms)
library(readxl)
library(janitor)
library(ggthemes)

nutrients_only <- read_csv("data/contaminants_nutrients.csv") %>% 
  clean_names() %>% 
  filter(tissue != "Liver",
         type == "nutrient") %>% 
  mutate(original_concentration = concentration,
         concentration = case_when(concentration_units == "mg/kg ww" ~ concentration/1000,
                                   concentration_units == "ng/g ww" ~ concentration/1000000,
                                   TRUE ~ concentration),
         original_concentration_units = concentration_units,
         concentration_units = "g/kg ww")



contaminants_only <- read_csv("data/contaminants_nutrients.csv") %>% 
  clean_names() %>% 
  filter(tissue != "Liver",
         type == "contaminant") %>%
  mutate(original_concentration = concentration,
         concentration = case_when(concentration_units == "mg/kg ww" ~ concentration/1000,
                                   concentration_units == "ng/g ww" ~ concentration/1000000,
                                   TRUE ~ concentration),
         original_concentration_units = concentration_units,
         concentration_units = "g/kg ww",
         sex = "NA")

#compare columns and types
compare_df_cols(nutrients_only, contaminants_only)


nut_cont <- bind_rows(contaminants_only, nutrients_only)

saveRDS(nut_cont, file = "data/nut_cont.rds")

#tally where measures are
region_replicates <- temp %>% 
  group_by(region) %>% 
  tally(name = "n_regions")

species_replicates <- temp %>% 
  group_by(species) %>% 
  tally(name = "n_species")

year_replicates <- temp %>% 
  group_by(publication_year) %>% 
  tally(name = "n_year")

contaminant_replicates <- temp %>% 
  group_by(chemical) %>% 
  tally(name = "n_contaminant")




# Which measures to we have? ----------------------------------------------

nut_cont %>% 
  group_by(contaminant) %>% 
  tally() %>% 
  ggplot(aes(x = reorder(contaminant, n), y = n)) + 
  geom_bar(stat = "identity") +
  coord_flip()

# OK to assume MeHg is same as Total Hg? YES----------------------------------
nut_cont %>% 
  filter(grepl("Hg", contaminant)) %>% 
  group_by(species, region, contaminant) %>% 
  summarize(median = median(concentration),
            sd = sd(concentration)) %>% 
  ggplot(aes(x = species, color = contaminant, y = median)) +
  geom_pointrange(aes(ymin = median - sd, ymax = median + sd),
                  position = position_dodge(width = 0.2)) +
  facet_wrap(~region) +
  labs(y = "Hg ng/kg ww")


# What is PCB variation among species and regions? High. Region important...Species less so
nut_cont %>% 
  filter(grepl("PCB", contaminant)) %>% 
  group_by(species, region) %>% 
  summarize(median = median(concentration),
            sd = sd(concentration)) %>% 
  ggplot(aes(x = species, color = region, y = median)) +
  geom_pointrange(aes(ymin = median - sd, ymax = median + sd),
                  position = position_dodge(width = 0.2)) +
  # facet_wrap(~region) +
  labs(y = "PCB's ng/kg ww")


# What is Lipid variation among species and regions? High. Region and species both important
nut_cont %>% 
  filter(grepl("ipid", contaminant)) %>% 
  group_by(species, region) %>% 
  summarize(median = median(concentration),
            sd = sd(concentration)) %>% 
  ggplot(aes(x = species, color = region, y = median)) +
  geom_pointrange(aes(ymin = median - sd, ymax = median + sd),
                  position = position_dodge(width = 0.2)) +
  # facet_wrap(~region) +
  labs(y = "Lipids %")

# What is DDT variation among species and regions? High. Region and species both important
nut_cont %>% 
  filter(grepl("DDT", contaminant)) %>% 
  group_by(species, region) %>% 
  summarize(median = median(concentration),
            sd = sd(concentration)) %>% 
  ggplot(aes(x = species, color = region, y = median)) +
  geom_pointrange(aes(ymin = median - sd, ymax = median + sd),
                  position = position_dodge(width = 0.2)) +
  # facet_wrap(~region) +
  labs(y = "DDT")

# What is PBDE variation among species and regions? High. Region and species both important
nut_cont %>% 
  filter(grepl("PBDE", contaminant)) %>% 
  group_by(species, region) %>% 
  summarize(median = median(concentration),
            sd = sd(concentration)) %>% 
  ggplot(aes(x = species, color = region, y = median)) +
  geom_pointrange(aes(ymin = median - sd, ymax = median + sd),
                  position = position_dodge(width = 0.2)) +
  # facet_wrap(~region) +
  labs(y = "PBDE")

saveRDS(nut_cont, "data/nut_cont.rds")




# What is variation over time?

  
nut_cont %>% 
  mutate(earliest_year_collected = parse_number(data_collection_period)) %>% 
  filter(grepl("Hg", contaminant)) %>% 
  # group_by(earliest_year_collected, region) %>% 
  # summarize(median = median(concentration),
  #           sd = sd(concentration)) %>% 
  ggplot(aes(x = earliest_year_collected, color = region, y = concentration)) +
  geom_point(position = position_jitter(width = 0.1), shape = 21) +
  # geom_pointrange(aes(ymin = median - sd, ymax = median + sd),
  #                 position = position_dodge(width = 0.2)) +
  facet_grid(. ~ species, scales = "free_y") +
  labs(title = "Hg by species",
       y = "Hg ng/kg ww")


nut_cont %>% 
  mutate(earliest_year_collected = parse_number(data_collection_period)) %>% 
  filter(grepl("ipid", contaminant)) %>% 
  # group_by(earliest_year_collected, region) %>% 
  # summarize(median = median(concentration),
  #           sd = sd(concentration)) %>% 
  ggplot(aes(x = earliest_year_collected, color = region, y = concentration)) +
  geom_point(position = position_jitter(width = 0.1), shape = 21) +
  # geom_pointrange(aes(ymin = median - sd, ymax = median + sd),
  #                 position = position_dodge(width = 0.2)) +
  facet_grid(. ~ species, scales = "free_y") +
  labs(title = "% Lipids by species",
       y = "% Lipids ng/kg ww") 


nut_cont %>% 
  mutate(earliest_year_collected = parse_number(data_collection_period)) %>% 
  filter(contaminant == "PCBs") %>% 
  # group_by(earliest_year_collected, region) %>% 
  # summarize(median = median(concentration),
  #           sd = sd(concentration)) %>% 
  ggplot(aes(x = earliest_year_collected, color = region, y = concentration)) +
  geom_point(position = position_jitter(width = 0.1), shape = 21) +
  # geom_pointrange(aes(ymin = median - sd, ymax = median + sd),
  #                 position = position_dodge(width = 0.2)) +
  facet_grid(species ~ ., scales = "free_y") +
  labs(title = "PCBs by species",
       y = "PCBs ng/kg ww")
