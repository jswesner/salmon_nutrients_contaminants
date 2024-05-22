# NOTE: this code shows the original steps to construct the data set for analyses named 'nut_cont.rds' or 'nut_cont.csv'
# During revision, we added new data directly to nut_cont.csv. therefore, the "raw" data is now saved as nut_cont.csv or ...rds and 
# the code below is obsolete. 


# library(tidyverse)
# library(brms)
# library(readxl)
# library(janitor)
# library(ggthemes)
# 
# nutrients_only_original <- read_excel("data/raw_data/SalmonFlux_SubsidiesDatabase_09.22.21.xlsx", 
#                              sheet = "final nutrients") %>% 
#   clean_names() %>% 
#   filter(tissue != "Liver") %>% 
#   mutate(type = "nutrient") %>%
#   mutate(n = as.numeric(n),
#          data_source = "original") %>% 
#   rename(chemical = nutrient)
# 
# 
# new_data_for_revision = read_csv("data/raw_data/new_nutrients.csv") %>% 
#   clean_names() %>% 
#   filter(tissue != "Liver") %>% 
#   mutate(type = "nutrient",
#          publication_year = as.character(publication_year)) %>%
#   mutate(n = as.numeric(n),
#          data_source = "april_2024") 
# 
# # only Donaldson 1967 appears in both data sets
# bind_rows(new_data_for_revision, nutrients_only_original) %>% 
#   group_by(data_source) %>% 
#   distinct(authors) %>% 
#   arrange(authors)
# 
# # the new data (april 2024) has multiple samples. The old data has only
# # one mean sample. Delete the old Donaldson and keep the new one.
# bind_rows(new_data_for_revision, nutrients_only_original) %>% 
#   filter(authors == "Donaldson 1967") %>% 
#   ggplot(aes(x = authors, y = mean_concentration_standardized)) + 
#   geom_point(aes(color = data_source), position = position_dodge(width = 2))
# 
# nutrients_only = bind_rows(new_data_for_revision, 
#                            nutrients_only_original %>% filter(authors != "Donaldson 1967"))
# 
# 
# contaminants_only <- read_excel("data/raw_data/SalmonFlux_SubsidiesDatabase_09.22.21.xlsx", 
#                                                 sheet = "final contaminants") %>% 
#   clean_names() %>% 
#   filter(tissue != "Liver") %>% 
#   mutate(type = "contaminant") %>%
#   mutate(sex = "NA") %>% 
#   rename(chemical = contaminant) %>% 
#   mutate(chemical = case_when(grepl("Hg", chemical) ~ "Hg", TRUE ~ chemical))
# 
# #compare columns and types
# compare_df_cols(nutrients_only, contaminants_only)
# 
# 
# nut_cont <- bind_rows(contaminants_only, nutrients_only) %>% 
#   mutate(region = case_when(is.na(region) ~ "Unknown",
#                             TRUE ~ region),
#          chemical = case_when(chemical == "PCBS" ~ "PCBs",
#                               TRUE ~ chemical)) %>% 
#   filter(region != "Russia" & region != "United Kingdom") %>% 
#   select(-concentration) %>% 
#   rename(old_region = region,
#          region = manuscript_region)
# 
# saveRDS(nut_cont, file = "data/nut_cont.rds")
# write_csv(nut_cont, file = 'data/nut_cont.csv')
# 
# 
# 
# nut_cont %>% 
#   filter(chemical == "DHA" | chemical == "EPA") %>% 
#   ggplot(aes(x = tissue, y = mean_concentration_standardized)) +
#   geom_point() +
#   facet_wrap(~chemical, scales = "free")
