library(tidyverse)
library(brms)
library(rethinking)
library(rstan)
library(bayesplot)
library(ggthemes)
library(janitor)
library(cowplot)

rstan_options(auto_write = TRUE)
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


d_short <- d %>% filter(year >= 1976) %>% 
  mutate(species_location = paste(species,"_",location)) %>% 
  group_by(species_location) %>% 
  mutate(y_s = (y - mean(y))/sd(y)) %>% 
  group_by(species_location) %>% 
  mutate(mean = mean(y),
         min = min(y)) %>% 
  ungroup() %>% 
  mutate(max_mean = max(mean),
         correction = max_mean - mean,
         y_corrected = y + correction,
         denom = 100000,
         y_corrected_1000 = y_corrected/denom)



d_backcorrect <- d_short %>%
  mutate(species_location = paste(species,"_",location)) %>% 
  distinct(species_location, mean, max_mean, correction, denom)

d_short %>% 
  ggplot(aes(x = year, y = y_1000, color = species_location)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~location)


# Gamma regression model -------------------------------------------------

# Fit data prior to 1976. Use posteriors from this model as priors

d_long <- d %>% filter(year < 1976) %>% 
  mutate(species_location = paste(species,"_",location)) %>% 
  group_by(species_location) %>% 
  mutate(y_s = (y - mean(y))/sd(y)) %>% 
  group_by(species_location) %>% 
  mutate(mean = mean(y),
         min = min(y)) %>% 
  ungroup() %>% 
  mutate(max_mean = max(mean),
         correction = max_mean - mean,
         y_corrected = y + correction,
         denom = 100000,
         y_corrected_1000 = y_corrected/denom)

# 
# gam_salmon1 <- brm(y_corrected_1000 ~ s(year, by = species_location) + (1|species_location),
#                    data = d_long, family = Gamma(link = "log"),
#                    iter = 1000, chains = 1,
#                    prior = c(prior(exponential(1), class = "sd"),
#                              prior(normal(0, 1), class = "Intercept")))
# 
# gam_salmon1 <- update(gam_salmon2, newdata = d_long)
# 
# saveRDS(gam_salmon1, file = "models/gam_salmon1.rds")

gam_salmon1 <- readRDS("models/gam_salmon1.rds")

test1 <- plot(conditional_effects(gam_salmon1, effects = "year:species_location", method = "fitted"), points = T)

test1$`year:species_location`$data %>%
  ggplot(aes(x = year, y = estimate__)) + 
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = species_location)) +
  facet_wrap(~species_location) +
  scale_y_log10() +
  geom_point(data = d_long, aes(x = year, y = y_corrected_1000)) +
  NULL

coefs_salmon1 <- fixef(gam_salmon1) %>% as_tibble() %>% 
  mutate(parameter = rownames(coefs_salmon1)) %>% 
  ggplot(aes(x = reorder(parameter, Estimate), y = Estimate, ymin = Q2.5, ymax = Q97.5)) + 
  geom_pointrange() +
  coord_flip()


# Fit final model

# gam_salmon2 <- brm(y_corrected_1000 ~ s(year, by = species_location) + (1|species_location),
#                   data = d_short, family = Gamma(link = "log"),
#                   iter = 1000, chains = 1,
#                   prior = c(prior(exponential(1), class = "sd"),
#                             prior(normal(0, 1), class = "Intercept"),
#                             prior(normal(0, 20), class = "b"),   # prior is posterior from gam_salmon1
#                             prior(gamma(4,4), class = "shape"))) # prior is posterior from gam_salmon1
# 
# saveRDS(gam_salmon2, file = "models/gam_salmon2.rds")

gam_salmon2 <- readRDS("models/gam_salmon2.rds")


test2 <- plot(conditional_effects(gam_salmon2, effects = "year:species_location", method = "fitted"), points = T)


# backtransform and plot
test2$`year:species_location`$data %>%
  left_join(d_backcorrect) %>% 
  mutate(estimate__ = estimate__*denom - correction,
         lower__ = lower__*denom - correction,
         upper__ = upper__*denom - correction) %>% 
  ggplot(aes(x = year, y = estimate__)) + 
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = species_location)) +
  facet_wrap(~species_location) +
  # scale_y_log10() +
  geom_point(data = d_short, aes(x = year, y = y)) +
  NULL



# posteriors

conditional_posts_fitted <- function(fit, effects, conditions = NULL){
  library(brms)
  library(tidyverse)
  library(janitor)
  list_of_data <- conditional_effects(fit, effects, conditions)[[1]]
  new_names <- list_of_data %>% 
    select(-names(fit$data[1])) %>% 
    select(-cond__, -effect1__, -estimate__, -se__, -lower__, -upper__) %>% 
    remove_empty("cols")
  
  as_tibble(t(fitted(fit, newdata = list_of_data, re_formula = NA, summary = FALSE))) %>%
    cbind(new_names) %>%
    pivot_longer(cols = contains("V"), names_to = "iter") %>%
    mutate(iter = parse_number(iter))
} 

gam_salmon_post <- conditional_posts_fitted(fit = gam_salmon2, 
                                            effects = "species_location", 
                                            conditions = data_frame(year = seq(1976, 2015, by = 1)))

gam_salmon_total <- gam_salmon_post %>% 
  left_join(d_backcorrect) %>% 
  mutate(value_raw = value*denom - correction) %>% 
  ungroup() %>% 
  group_by(iter, year) %>% 
  summarize(total_metric_tons = sum(value_raw))

d_total <- d_short %>% 
  group_by(year) %>% 
  summarize(total_raw = sum(y))

gam_salmon_total %>% 
  group_by(year) %>% 
  summarize(median = median(total_metric_tons),
            lower = quantile(total_metric_tons, probs = 0.025),
            upper = quantile(total_metric_tons, probs = 0.975)) %>% 
  ggplot(aes(x = year)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) + 
  geom_line(aes(y = median)) +
  geom_point(data = d_total, aes(x = year, y = total_raw))
