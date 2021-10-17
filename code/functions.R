conditional_posts_fitted <- function(fit, effects, conditions = NULL){
  library(brms)
  library(tidyverse)
  library(janitor)
  list_of_data <- conditional_effects(fit, effects, conditions)[[1]]
  new_names <- list_of_data %>% 
    dplyr::select(-names(fit$data[1])) %>% 
    select(-cond__, -effect1__, -estimate__, -se__, -lower__, -upper__) %>% 
    remove_empty("cols")
  
  as_tibble(t(fitted(fit, newdata = list_of_data, re_formula = NA, summary = FALSE))) %>%
    cbind(new_names) %>%
    pivot_longer(cols = contains("V"), names_to = "iter") %>%
    mutate(iter = parse_number(iter))
} 

sim_gamma_priors <- function(prior_intercept = NULL, prior_b = NULL,
                             prior_sd = NULL, prior_shape = NULL) {
  tibble(chinook = prior_b,
         chum = prior_b,
         pink = prior_b,
         sd = prior_sd,
         shape = prior_shape) %>% 
    pivot_longer(cols = c(-shape, -sd), names_to = "species") %>% 
    mutate(exp_value = exp(value),
           exp_valuerand = exp(value + rnorm(nrow(.),0, sd)),
           scale = exp_value/shape,
           prior_simdata = rgamma(nrow(.), shape = shape, scale = scale)) %>% 
    dplyr::select(species, exp_value, exp_valuerand, prior_simdata) %>% 
    pivot_longer(cols = c(exp_value, exp_valuerand, prior_simdata)) %>% 
    ggplot(aes(x = species, y = value)) + 
    geom_violin(aes(group = species)) +
    facet_wrap(~name) +
    NULL
}

get_posts <- function(model){
  posterior_samples(model) %>% 
    mutate(iter = 1:nrow(.)) %>% 
    clean_names() %>% 
    as_tibble() %>% 
    mutate(chems_units = model$chems_units)
}