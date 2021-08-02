conditional_posts_fitted <- function(fit, effects, conditions = NULL, re_formula = NA){
  list_of_data <- conditional_effects(fit, effects, conditions)[[1]]
  col_i <- which(colnames(list_of_data) == names(fit$data[1])) - 1
  new_names <- list_of_data[(1:col_i)]
  
  as_tibble(t(fitted(fit, newdata = list_of_data, re_formula, summary = FALSE))) %>%
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
    select(species, exp_value, exp_valuerand, prior_simdata) %>% 
    pivot_longer(cols = c(exp_value, exp_valuerand, prior_simdata)) %>% 
    ggplot(aes(x = species, y = value)) + 
    geom_violin(aes(group = species)) +
    facet_wrap(~name) +
    NULL
}