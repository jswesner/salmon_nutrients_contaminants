d_small <- d %>% filter(location %in% c("BeringSea", "BCWC", "SEAK")) %>% 
  mutate(location_int = as.integer(as.factor(location)))


fit_int <- stan(file = "code/pink_salmon_interaction.stan",             
                data = list(y = d_small$y,
                            N = nrow(d_small),
                            location = d_small$location_int),
                iter = 500, chains = 1)
