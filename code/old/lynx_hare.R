library(rethinking)
library(tidyverse)

data(Lynx_Hare_model)
cat(Lynx_Hare_model)

dat_list <- list(
  N = nrow(Lynx_Hare),
  pelts = Lynx_Hare[,2:3])


lynx_mod <- stan(file = "code/lynx_hare.stan", data = dat_list)

lynx_mod

post_lynx <- extract.samples(lynx_mod) 

post_lynx_pelts <- post_lynx$pelts_pred %>% as_tibble() %>% mutate(iter = 1:nrow(.)) 

post_lynx_pelts %>% pivot_longer(cols = -iter) %>% 
  filter(iter <=10) %>% 
  mutate(sample = parse_number(name),
         year = rep(1:21, 20),
         species = as.factor(rep(rep(1:2, each = 21),10))) %>% 
  ggplot(aes(x = year, y = value, color = species)) +
  geom_line(aes(group = interaction(species,iter)))



# try it with pink salmon
pink_only_test <- d %>% filter(species == "pink", location == "SEAK") %>% mutate(y = value)

fish <- pink_only_test$y

library(atsar)
library(rstan)
library(janitor)

data = list(y = pink_only_test$y,
            N = nrow(pink_only_test))

ss_ar_lognormal <- stan(file = "code/pink_salmon.stan", data = data)
seak_lognormal <- stan(file = "code/pink_salmon.stan", data = data)

post_as <- as.matrix(seak_lognormal, pars = c("pred")) %>% as_tibble() %>% clean_names() %>% mutate(iter = 1:nrow(.)) %>% 
  pivot_longer(cols = -iter) %>% 
  mutate(time = parse_number(name))

post_as %>% 
  filter(iter <= 1000) %>%
  # group_by(time) %>% 
  # summarize(median = median(exp(value)),
  #           lower = quantile(exp(value), probs = 0.025),
  #           upper = quantile(exp(value), probs = 0.975)) %>% 
  # ggplot(aes(x = time-1+min(pink_only_test$year), y = median)) +
  ggplot(aes(x = time -1 + min(pink_only_test$year), y = exp(value))) +
  geom_line(aes(group = iter), alpha = 0.1) +
  # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(data = pink_only_test, aes(x = year, y = y), color = "maroon") +
  geom_point(data = pink_only_test, aes(x = year, y =y), color = "maroon")


