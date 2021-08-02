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


nut_cont <- bind_rows(contaminants_only, nutrients_only) %>% 
  mutate(region = case_when(is.na(region) ~ "Unknown",
                            TRUE ~ region),
         chemical = case_when(chemical == "PCBS" ~ "PCBs",
                              TRUE ~ chemical)) %>% 
  filter(region != "Russia" & region != "United Kingdom")

saveRDS(nut_cont, file = "data/nut_cont.rds")


