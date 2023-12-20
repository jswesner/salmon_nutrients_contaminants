library(tidyverse)
library(brms)

# model contaminant and nutrient concentrations in fish

#load data
nut_cont <- readRDS("data/nut_cont.rds") %>% 
  mutate(location = case_when(grepl("ering", region) ~ "BS",
                              grepl("entral", region) ~ "CAK",
                              region == "AK" ~ "CAK",
                              region == "SEAK" ~ "SEAK",
                              grepl("C/W", region) ~ "BCWC",
                              TRUE ~ "ALL")) %>% 
  mutate(type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "Nutrients",
                          TRUE ~ "Contaminants"),
         labels = case_when(chemical == "Hg" ~ "b) Hg",
                            chemical == "DDTs" ~ "f) DDT",
                            chemical == "PBDEs" ~ "h) PBDE",
                            chemical == "PCBs" ~ "d) PCBs",
                            chemical == "N" ~ "a) N",
                            chemical == "P" ~ "g) P",
                            chemical == "DHA" ~ "c) DHA",
                            TRUE ~ "e) EPA"))

nut_cont_year_trend = nut_cont %>% 
  mutate(most_recent_year = parse_number(str_sub(data_collection_period, -4, -1))) %>% 
  filter(most_recent_year >1000) %>% 
  ggplot(aes(x = most_recent_year, y = mean_concentration_standardized)) +
  geom_point(aes(color = location, shape = species), 
             size = 1) +
  facet_wrap(~labels, nrow = 2, scales = "free_y") +
  # geom_line(aes(group = interaction(region, species))) +
  scale_y_log10() +
  # geom_boxplot(aes(group = most_recent_year)) +
  xlim(1975, 2020) +
  theme_default() +
  # theme(legend.position = c(0.9, 0.1)) +
  theme(legend.direction = "horizontal",
        legend.box = "vertical",
        legend.position = "top", 
        axis.text.x = element_text(size = 8)) +
  labs(y = "Tissue Concentration mg/kg ww",
       x = "Most recent year",
       color = "Region",
       shape = "Species") +
  scale_x_continuous(breaks = c(1975, 1995, 2015))

nut_cont_year_trend

ggview::ggview(nut_cont_year_trend, width = 6.5, height = 4.5)
ggsave(nut_cont_year_trend, file = "plots/revision_plots/nut_cont_year_trend.jpg",
       width = 6.5, height = 4.5)
