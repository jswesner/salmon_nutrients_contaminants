library(tidyverse)
library(tidybayes)

# load posteriors
gam_salmon_posts <- readRDS("posteriors/gam_salmon_posts.rds")  # Salmon escapement in kg wet mass
all_chem_posts <- readRDS(file = "posteriors/all_chem_posts.rds") %>% ungroup()

# summarize
gam_salmon_posts %>% 
  group_by(species, location, year) %>% 
  summarize(mean = round(mean(metric_tons), 0),
            sd = round(sd(metric_tons), 0)) %>% 
  mutate(units = "Metric Tons") %>%
  glimpse() %>% 
  write_csv(., file = "data/data_releaseUSGS/modeled_biomass_datarelease.csv")


all_chem_posts %>% 
  group_by(species, chemical, units, type) %>% 
  summarize(mean = round(mean(.epred), 0),
            sd = round(sd(.epred), 0)) %>% 
  glimpse() %>% 
  write_csv(., file = "data/data_releaseUSGS/modeled_subsidies_datarelease.csv")


unique(gam_salmon_posts$location)

# get citations from doi
library(RefManageR)
nutrient_contaminants_citations <- read_csv("data/raw_data/nutrient_contaminants.csv") %>% 
  mutate(doi = case_when(grepl("^10.", doi_or_link) ~ doi_or_link)) %>% 
  distinct(doi_or_link, .keep_all = T)


dois = nutrient_contaminants %>% 
  distinct(doi, type, data_collection_period) %>% 
  filter(!is.na(doi))

doi_citations = GetBibEntryWithDOI(doi = dois$doi)

as_tibble(doi_citations) %>% 
  right_join(nutrient_contaminants_citations) %>% 
  write_csv(., file = "data/data_releaseUSGS/nutrient_contamiants_citations_datarelease.csv")

  
