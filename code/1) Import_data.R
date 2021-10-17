library(tidyverse)
library(brms)
library(readxl)
library(janitor)
library(ggthemes)

nutrients_only <- read_excel("data/raw_data/SalmonFlux_SubsidiesDatabase_09.22.21.xlsx", 
                             sheet = "final nutrients") %>% 
  clean_names() %>% 
  filter(tissue != "Liver") %>% 
  mutate(type = "nutrient") %>%
  mutate(n = as.numeric(n)) %>% 
  rename(chemical = nutrient)



contaminants_only <- read_excel("data/raw_data/SalmonFlux_SubsidiesDatabase_09.22.21.xlsx", 
                                                sheet = "final contaminants") %>% 
  clean_names() %>% 
  filter(tissue != "Liver") %>% 
  mutate(type = "contaminant") %>%
  mutate(sex = "NA") %>% 
  rename(chemical = contaminant) %>% 
  mutate(chemical = case_when(grepl("Hg", chemical) ~ "Hg", TRUE ~ chemical))

#compare columns and types
compare_df_cols(nutrients_only, contaminants_only)


nut_cont <- bind_rows(contaminants_only, nutrients_only) %>% 
  mutate(region = case_when(is.na(region) ~ "Unknown",
                            TRUE ~ region),
         chemical = case_when(chemical == "PCBS" ~ "PCBs",
                              TRUE ~ chemical)) %>% 
  filter(region != "Russia" & region != "United Kingdom") %>% 
  select(-concentration) %>% 
  rename(old_region = region,
         region = manuscript_region)

saveRDS(nut_cont, file = "data/nut_cont.rds")


