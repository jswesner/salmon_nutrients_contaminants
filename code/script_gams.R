d_small <- d %>% filter(species == "pink") %>% 
  mutate(site = as.integer(as.factor(location)),
         y_10K = y*100)



mod_int <- stan(file = "code/pink_salmon_interaction.stan", data = list(y = d_small$y,
                                                 site = d_small$site,
                                                 N = nrow(d_small),
                                                 J = length(unique(d_small$site))))



test <- extract.samples(mod_int)



fit_int <- as.matrix(mod_int, pars = c("pred")) %>% as_tibble() %>% clean_names() %>% mutate(iter = 1:nrow(.)) %>% 
  pivot_longer(cols = -iter) %>% 
  mutate(time = parse_number(name))

fit_int %>% 
  group_by(time) %>% 
  summarize(median = median(exp(value)),
            lower = quantile(value, probs = 0.025),
            upper = quantile(value, probs = 0.975)) %>% 
  ggplot(aes(x = time, y = median)) + 
  geom_line() +
  geom_point(data = d_small, aes(y = y))
  






library(brms)


gam_test <- brm(y_10K ~ s(year, by = location, bs = "cs", k = 30) + (1|location), data = d_small,
               chains = 1, iter = 1000,
               family = Gamma(link = "log"))

post_gamtest <- posterior_samples(gam_test)

test_plot <- plot(conditional_effects(gam_test, effects = 'year:location'), points = T)

test_plot$`year:location` +
  # scale_y_log10() +
  facet_wrap(~location)
