library(tidyverse)
library(readxl)
library(brms)
library(janitor)
library(rethinking)
library(brms)

d <- read_excel("data/ruggerone_dataclean_oct9.xlsx") %>% 
  pivot_longer(cols = -Year) %>% 
  separate(name, c("location", "species"), sep = "_") %>% 
  clean_names()



unique(d$species)

salmon_plot <- d %>% 
  ggplot(aes(x = year, y = value, color = species)) + 
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~location) +
  labs(y = "Escapement (Millions of Fish)") +
  theme_classic()

salmon_plot

ggsave(salmon_plot, file = 'plots/salmon_plot.jpg', dpi = 500, width = 8, height = 7)

#dir.create("plots")
# file.remove("script_escapement.R")

pink_only <- d %>% filter(species == "pink" & location %in% c("BCWC","CentralAK", "BeringSea")) %>% mutate(y_lag = lag(value),
                                                                             y_lag2 = lag(value, 2),
                                                                             y = value,
                                                                             y_lagc = y_lag - mean(y_lag, na.rm = T),
                                                                             y_lag2c = y_lag2 - mean(y_lag2, na.rm =T))


# gam
get_prior(y ~ s(year, bs = 'cc', k = 20), data = pink_only)

gam_test <- brm(y ~ s(year, bs = 'cc', k = 15), data = pink_only)

plot(conditional_effects(gam_test), points = T)

post_gam <- posterior_samples(gam_test)


# time series example
d2 <- read.delim("~/GitHub/salmon_mercury/data/time_series_example/d.txt")
dat<-list(N=100,x1=d2$x1,x2=d2$x2,x3=d2$x3,y=d2$y)
fit1 = stan(file='code/time_series_example.stan',data=dat,iter=1000,chains=4)


prior_lag = tibble(b_lag = c(0.01, 0.1, 0.5, 1, 2),
                   int = c(12, 12, 12, 12, 12),
                   mod = c("a", "b", "c", "d", "e")) %>% 
  expand_grid(y_lag = pink_only$y_lag) %>% 
  mutate(year = rep(pink_only$year, 5),
         y = int + y_lag*b_lag,
         ylog = exp(int + y_lag*b_lag))



prior_lag %>% 
  ggplot(aes(x = year, y = y, color = b_lag)) + 
  geom_line(aes(group = b_lag))





get_prior(y ~ y_lag, data = pink_only)

fit_lag_bering <- brm(y ~ y_lag:location + y_lag2:location + (1|location),
                      data = pink_only,
                      family = gaussian(),
               prior = c(prior(normal(0, 1), class = "b"),
                         prior(normal(15, 5), class = "Intercept"),
                         prior(exponential(1), class = "sd")),
               chains = 1)


# fit_lag_bering <- brm(y ~ y_lag:location + y_lag2:location + (1|location),
#                       data = pink_only,
#                       family = Gamma(link = "log"),
#                prior = c(prior(normal(0, 0.1), class = "b"),
#                          prior(normal(1, 1), class = "Intercept"),
#                          prior(gamma(0.01, 0.01), class = "shape"),
#                          prior(exponential(1), class = "sd")))


post_lag <- posterior_samples(fit_lag_bering) %>% mutate(iter = 1:nrow(.)) %>% 
  sample_n(300) %>% 
  clean_names() %>% 
  select(-lp, -sigma, -sd_location_intercept) %>% 
  as_tibble() %>% 
  glimpse()


post_fit <- post_lag %>%
  mutate(BCWC_intercept = b_intercept + r_location_bcwc_intercept,
         BCWC_bylag = b_y_lag_location_bcwc,
         BCWC_bylag2 = b_location_bcwc_y_lag2,
         BeringSea_intercept = b_intercept + r_location_bering_sea_intercept,
         BeringSea_bylag = b_y_lag_location_bering_sea,
         BeringSea_bylag2 = b_location_bering_sea_y_lag2,
         CentralAK_intercept = b_intercept + r_location_central_ak_intercept,
         CentralAK_bylag = b_y_lag_location_central_ak,
         CentralAK_bylag2 = b_location_central_ak_y_lag2) %>% 
  select(BCWC_intercept, BCWC_bylag, BCWC_bylag2,
         BeringSea_intercept, BeringSea_bylag, BeringSea_bylag2,
         CentralAK_intercept, CentralAK_bylag, CentralAK_bylag2,
         iter) %>% 
  pivot_longer(cols = -iter) %>% 
  separate(name, c("location", "parameter")) %>% 
  pivot_wider(names_from = parameter, values_from = value) %>% 
  left_join(pink_only %>% select(y_lag, y_lag2, year, location), by = "location") %>% 
  mutate(y_fit = intercept + bylag*y_lag + bylag2*y_lag2) %>% 
  glimpse()


  
post_fit %>% 
  ggplot(aes(x = year-2, y = y_fit)) + 
  geom_point(data = pink_only, aes(y = y, x = year)) +
  # geom_line(alpha = 0.05, aes(group = interaction(location,iter))) +
  facet_wrap(~location) +
  # coord_cartesian(ylim = c(-2, 100)) +
  # scale_y_log10() +
  NULL

pinktest <- pink_only %>% select(y_lag, y_lag2, year)
post_fittest <- post_lag %>% clean_names() %>%
  expand_grid(pinktest) %>% 
  drop_na() %>% 
  mutate(bcwc_y = b_intercept + b_y_lag_location_bcwc*y_lag + b_location_bcwc_y_lag2*y_lag2)

post_fittest %>% 
  ggplot(aes(x = year, y = bcwc_y, group = iter)) +
  geom_line()

  
  pink_only$lag <- c(NA, pink_only$y[1:(n-1)])

test <- tibble(int = 12.57, 
               b = 0.02,
               sigma = 8.71) %>% 
  expand_grid(y_lag = pink_only$y_lag) %>% 
  mutate(year = pink_only$year) %>% 
  mutate(y_fit = int + b*y_lag + rnorm(1, 0, sigma) )

test %>% 
  ggplot(aes(x = year, y = y_fit)) + 
  geom_line() +
  geom_point(data = pink_only, aes(x = year, y = y))



plot(conditional_effects(fit_lag), points = T)

sims <- as.matrix(fit_lag)
n <- nrow(pink_only)
n_sims <- nrow(sims)
y_rep <- array(NA, c(n_sims, n))
for (s in 1:n_sims){
  y_rep[s,1] <- y[1]
for (t in 2:n){
  y_rep[s,t] <- sims[s, "(Intercept)"] + sims[s, "y_lag"] * y_rep[s, t-1] + 
                       rnorm(1, 0, sims[s, "sigma"])}
}





# Gaussian Process

pink_only_test <- d %>% filter(species == "pink" & location %in% c("BCWC")) %>% mutate(y_lag = lag(value),
                                                                                                           y_lag2 = lag(value, 2),
                                                                                                           y = value,
                                                                                                           y_lagc = y_lag - mean(y_lag, na.rm = T),
                                                                                       y_lag2c = y_lag2 - mean(y_lag2, na.rm =T))


bcwc_brm <- brm(y ~ y_lag + y_lag2, data = pink_only_test)

bcwc_post <- posterior_samples(bcwc_brm) %>% as_tibble() %>% clean_names() %>% mutate(iter = 1:nrow(.))

bcwc_fit <- bcwc_post %>% expand_grid(pink_only_test %>% select(y_lag, y_lag2, year)) %>% 
  mutate(y_fit = b_intercept + b_y_lag*y_lag + b_y_lag2*y_lag2)


bcwc_fit %>% 
  filter(iter <= 300) %>% 
  ggplot(aes(x = year-2, y = y_fit)) + 
  geom_line(alpha = 0.1, aes(group = iter)) +
  geom_point(data = pink_only_test, aes(x = year, y = y), shape = 21, fill = "yellow")










testgp <- brm(y ~ gp(year), data = pink_only_test)


postgp <- posterior_samples(testgp) %>% clean_names() %>% mutate(iter = 1:nrow(.)) %>% 
  pivot_longer(cols = c(-b_intercept, -sdgp_gpyear, -lscale_gpyear, -sigma, -lp)) %>% 
  mutate(yfit = b_intercept + sdgp_gpyear + value) %>% 
  separate(name, c("delete","delete2","year"))


postgp_sum <- postgp %>% 
  group_by(year) %>% 
  drop_na() %>% 
  summarize(median = median(yfit),
            lower = quantile(yfit, probs = 0.025),
            upper = quantile(yfit, probs = 0.975)) %>% 
  mutate(year = as.numeric(year))


postgp_sum %>% 
  ggplot(aes(x = year, y = median, ymax = upper, ymin = lower)) + 
  geom_ribbon(alpha = 0.2) +


plot(conditional_effects(testgp), points = T)


testgp2 <- brm(y ~ gp(year, by = location), data = pink_only)


# arima
bayes_fit <- brm(
  y ~ 1 + year + ar(p = 1),
  data = pink_only_test
)

plot(conditional_effects(bayes_fit), points = T)






set.seed(250)
t_drift = arima.sim(list(order = c(1,0,0), ar = 0.8), n = 50) + 0.50 * seq(1,50)
t_drift_df = data_frame(
  time = seq_along(t_drift),
  y = as.vector(t_drift)
)

m <- brm(y ~ 1 + year, autocor = cor_ar(~ 1, p = 1, cov = TRUE), data = pink_only_test,
         family = Gamma(link = "log"))

library(tidybayes)

pink_only_test %>%
  add_predicted_draws(m, n = 500) %>%
  ggplot(aes(x = year, y = .prediction)) +
  geom_line(aes(group = .draw), alpha = 1/20) +
  geom_point(aes(y = y), data = pink_only_test) +
  scale_y_log10()


