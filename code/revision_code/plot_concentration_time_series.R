library(tidyverse)

# model contaminant and nutrient concentrations in fish

#load data
nut_cont <- readRDS("data/nut_cont.rds")

nut_cont_year_trend = nut_cont %>% 
  mutate(most_recent_year = parse_number(str_sub(data_collection_period, -4, -1))) %>% 
  filter(most_recent_year >1000) %>% 
  ggplot(aes(x = most_recent_year, y = mean_concentration_standardized)) +
  geom_point(aes(color = region, shape = species)) +
  facet_wrap(~chemical, scales = "free") +
  # geom_line(aes(group = interaction(region, species))) +
  scale_y_log10() +
  # geom_boxplot(aes(group = most_recent_year)) +
  xlim(1975, 2020) +
  theme(legend.position = c(0.9, 0.1)) +
  theme(legend.direction = "vertical", legend.box = "horizontal") +
  labs(y = "Tissue Concentration mg/kg ww",
       x = "Most recent year")

nut_cont_year_trend

ggsave(nut_cont_year_trend, file = "plots/revision_plots/nut_cont_year_trend.jpg",
       width = 6.5, height = 5.5)
