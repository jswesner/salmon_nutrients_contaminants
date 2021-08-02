library(tidyverse)
library(readxl)
library(rethinking)
library(rstan)
library(bayesplot)
library(ggthemes)
library(janitor)
library(cowplot)


# Load and plot raw data ----------------------------------------------------------------

d <- read_csv("data/salmon_metric_tons.csv") %>% 
  pivot_longer(cols = c(-units, -Year), names_to = "name", values_to = "y") %>% 
  separate(name, c("location", "species"), sep = "_", remove = F) %>% 
  clean_names() %>% 
  drop_na(y) %>% 
  group_by(name) %>% 
  mutate(time = year - min(year) + 1) %>% 
  ungroup() %>% 
  mutate(y_1000 = y/1000)

d %>% 
  group_by(year) %>% 
  summarize(total = sum(y)) %>% 
  ggplot(aes(x = year, y = total)) + 
  geom_point() + 
  geom_line()


d %>% 
  ggplot(aes(x = year, y = y, color = species)) + 
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~location) +
  labs(y = "Escapement (Metric Tons of Fish)") +
  theme_classic()

# nest data by species and location
d_nest <- d %>%
  group_by(name) %>% 
  nest()


# Fit models --------------------------------------------------------------


# create empty list to store models
# mods <- list()
 
# fit separate models to each species x location combination
# for (i in 1:nrow(d_nest)) {
#   mods[i] <- stan(file = "code/pink_salmon.stan",
#                        data = list(y = d_nest$data[i] %>% as.data.frame() %>% select(y) %>% pull(),
#                                    N = d_nest$data[i] %>% as.data.frame() %>% select(y) %>% nrow(.)))
# }

# saveRDS(mods, file = "models/mods.rds")


# Extract posteriors ------------------------------------------------------

mods <- readRDS(file = "models/mods.rds")

# extract posteriors
fits <- list()
for(i in 1:nrow(d_nest)) {
  fits[[i]] <- as.matrix(mods[[i]], pars = c("pred")) %>% as_tibble() %>% clean_names() %>% mutate(iter = 1:nrow(.)) %>% 
    pivot_longer(cols = -iter) %>% 
    filter(iter <= 1000) %>% 
    mutate(time = parse_number(name),
           location = d_nest$data[i] %>% as.data.frame() %>% select(location) %>% distinct() %>%  pull(),
           species = d_nest$data[i] %>% as.data.frame() %>% select(species) %>% distinct() %>%  pull())
}

# saveRDS(fits, file = "models/fits.rds")

escapement_statespace <- fits %>%
  bind_rows() %>% 
  left_join(d %>% ungroup() %>% select(location, species, time, year)) %>% 
  group_by(species, location, year) %>% 
  summarize(median = median(exp(value)),
            upper = quantile(exp(value), probs = 0.975),
            lower = quantile(exp(value), probs = 0.025)) %>% 
  ungroup() %>% 
  # group_by(time) %>% 
  # summarize(median = median(exp(value)),
  #           lower = quantile(exp(value), probs = 0.025),
  #           upper = quantile(exp(value), probs = 0.975)) %>% 
  # ggplot(aes(x = time-1+min(pink_only_test$year), y = median)) +
  ggplot(aes(x = year, y = median, color = location)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = location), alpha = 0.4) +
  geom_line() +
  facet_grid(species ~ .) +
  # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(data = d, aes(x = year, y = y)) +
  geom_point(data = d, aes(x = year, y =y)) +
  # scale_y_log10() + 
  theme_classic() +
  labs(y = "Millions of Fish (escapement)") +
  NULL

escapement_statespace

saveRDS(escapement_statespace, file = "plots/escapement_statespace.rds")
ggsave(escapement_statespace, file = "plots/excapement_statespace.jpg", dpi = 500, width = 7, height = 10)



escapement_statespace <- readRDS("plots/escapement_statespace.rds")

