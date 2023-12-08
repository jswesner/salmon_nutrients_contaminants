library(brms)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(cowplot)
library(scales)
library(janitor)
library(ggridges)
library(viridis)

# load posteriors
gam_salmon_posts <- readRDS("posteriors/gam_salmon_posts.rds")  # Salmon escapement in kg wet mass
all_chem_posts <- readRDS(file = "posteriors/all_chem_posts.rds") %>% ungroup()

flux_predictions = readRDS(file = "posteriors/flux_predictions.rds")

# alter these to assume linear decline in EPA and linear increase in Hg
# for Hg, assume that it has doubled from 1975 to 2015. this is based on a reported
# doubling of Hg deposition from the 1980s to 2000s in Fig 8 of Sunderland, E. M., Krabbenhoft, D. P., Moreau, J. W., Strode, S. A., & Landing, W. M. (2009). Mercury sources, distribution, and bioavailability in the North Pacific Ocean: Insights from data and models. Global Biogeochemical Cycles, 23(2).
years <- seq(1976, 2015)
new_epa_values = seq(10000, 3000, length.out = length(years))
new_dha_values = seq(10000, 3000, length.out = length(years))
new_hg_values = seq(0.02, 0.04, length.out = length(years))
new_values = tibble(EPA = new_epa_values, 
                    Hg = new_hg_values,
                    DHA = new_dha_values,
                    year = years) %>% 
  pivot_longer(cols = -year, names_to = "chemical", 
               values_to = "new_mg_kg_chem")


flux_predictions_hypothetical = flux_predictions %>%
  left_join(new_values) %>% 
  mutate(mg_flux_hypothetical = case_when(chemical == "EPA" ~ kg*new_mg_kg_chem,
                                          chemical == "Hg" ~ kg*new_mg_kg_chem,
                                          chemical == "DHA" ~ kg*new_mg_kg_chem,
                                          TRUE ~ mg_flux))

flux_to_compare = flux_predictions_hypothetical %>% 
  ungroup() %>%
  group_by(year, .draw, type, species) %>%
  summarize(total = sum(mg_flux/1000000),
            total_new = sum(mg_flux_hypothetical/1000000)) %>%
  pivot_longer(cols = c(total, total_new)) %>% 
  group_by(year, type, name,  species) %>%
  summarize(median = median(value),
            low50 = quantile(value, probs = 0.25),
            high50 = quantile(value, probs = 1-0.25)) %>%
  ungroup() %>% 
  mutate(type = fct_relevel(type, "Nutrients"))


flux_to_compare %>% 
  ggplot(aes(x = year, y = median, color = species, fill = species)) + 
  geom_line() +
  geom_ribbon(aes(ymin = low50, ymax = high50)) +
  facet_grid(type~name, scales = "free")
