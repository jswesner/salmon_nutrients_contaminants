library(tidyverse)
library(brms)
library(rethinking)
library(rstan)
library(bayesplot)
library(ggthemes)
library(janitor)
library(cowplot)

# model salmon time series

rstan_options(auto_write = TRUE)
# Load and plot raw data ----------------------------------------------------------------

d <- read_csv("data/salmon_metric_tons.csv") %>% 
  pivot_longer(cols = c(-units, -Year), names_to = "name", values_to = "y") %>% 
  separate(name, c("location", "species"), sep = "_", remove = F) %>% 
  separate(species, c("species", "family")) %>% 
  mutate(location = str_replace(location, " ", "")) %>% 
  clean_names() %>% 
  drop_na(y) %>% 
  group_by(name) %>% 
  mutate(time = year - min(year) + 1) %>% 
  ungroup() %>% 
  mutate(y_1000 = y/1000)


d_short <- d %>% filter(year >= 1976) %>% 
  mutate(species_location = paste(species,"_",location))

saveRDS(d_short, file = "data/d_short.rds")


d_short %>% 
  ggplot(aes(x = year, y = y_1000, color = species_location)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~location)


# Gamma regression model -------------------------------------------------

# Fit data prior to 1976. Use posteriors from this model as priors

d_long <- d %>% filter(year < 1976) %>% 
  mutate(species_location = paste(species,"_",location)) %>% 
  mutate(y_10000 = y/10000)

# 
# gam_salmon1 <- brm(y_1000 ~ s(year, by = species_location) + (1|species_location),
#                    data = d_long, family = Gamma(link = "log"),
#                    iter = 1000, chains = 1,
#                    prior = c(prior(exponential(1), class = "sd"),
#                              prior(normal(2,1), class = "Intercept")))

# gam_salmon1 <- update(gam_salmon2, newdata = d_long)
# 
# saveRDS(gam_salmon1, file = "models/gam_salmon1.rds")

gam_salmon1 <- readRDS("models/gam_salmon1.rds")

coefs_salmon1 <- fixef(gam_salmon1) %>% as_tibble() %>% 
  mutate(parameter = rownames(fixef(gam_salmon1))) %>% 
  ggplot(aes(x = reorder(parameter, Estimate), y = Estimate, ymin = Q2.5, ymax = Q97.5)) + 
  geom_pointrange() +
  coord_flip()


# Fit final model

# gam_salmon2 <- brm(y_1000 ~ s(year, by = species_location) + (1|species_location),
#                   data = d_short, family = Gamma(link = "log"),
#                   iter = 2000, chains = 4,
#                   prior = c(prior(exponential(1), class = "sd"),
#                             prior(normal(0, 1), class = "Intercept"),
#                             prior(normal(0, 5), class = "b"),   # prior is posterior from gam_salmon1
#                             prior(gamma(4,1), class = "shape"))) # prior is posterior from gam_salmon1
# 
# saveRDS(gam_salmon2, file = "models/gam_salmon2.rds")

gam_salmon2 <- readRDS("models/gam_salmon2.rds")


test2 <- plot(conditional_effects(gam_salmon2, effects = "year:species_location", method = "fitted", 
                                  re_formula = NULL), points = T)


# plot
test2$`year:species_location` +
  facet_wrap(~species_location) +
  NULL

# posteriors

conditional_posts_fitted <- function(fit, effects, conditions = NULL, re_formula = NA){
  library(brms)
  library(tidyverse)
  library(janitor)
  list_of_data <- conditional_effects(fit, effects, conditions, re_formula)[[1]]
  new_names <- list_of_data %>%
    select(-names(fit$data[1])) %>%
    select(-cond__, -effect1__, -estimate__, -se__, -lower__, -upper__) %>%
    remove_empty("cols")

  as_tibble(t(fitted(fit, newdata = list_of_data, re_formula, summary = FALSE))) %>%
    cbind(new_names) %>%
    pivot_longer(cols = contains("V"), names_to = "iter") %>%
    mutate(iter = parse_number(iter))
}

gam_salmon_posts <- conditional_posts_fitted(fit = gam_salmon2,
                                            effects = "species_location",
                                            re_formula = NULL,
                                            conditions = tibble(year = seq(1976, 2015, by = 2))) %>%
  mutate(metric_tons = value*1000,
         kg = metric_tons*1000) %>%
  separate(species_location, sep = " _ ", c("species", "location"), remove = F) %>%
  separate(species, sep = " ", c("species", "family"))

saveRDS(gam_salmon_posts, file = "posteriors/gam_salmon_posts.rds")

