library(brms)
library(tidyverse)
library(ggridges)
library(bayesplot)
source("code/functions.R")

# check models

# 1) bring in models
gam_salmon2 <- readRDS("models/gam_salmon2.rds")
hg_model <- readRDS("models/hg_model.rds") 
pcb_model <- readRDS("models/pcb_model.rds")
nit_model <- readRDS("models/nit_model.rds") 
phos_model <- readRDS("models/phos_model.rds") 
epa_model <- readRDS("models/epa_model.rds")
dha_model <- readRDS("models/dha_model.rds")
pbde_model <- readRDS("models/pbde_model.rds")
ddt_model <- readRDS("models/ddt_model.rds")

# 2) Posterior predictive checks
# pp_check(gam_salmon2) + scale_x_log10()
pp_check(hg_model) + scale_x_log10()
pp_check(pcb_model) + scale_x_log10()
pp_check(nit_model) + scale_x_log10()
pp_check(phos_model) + scale_x_log10()
pp_check(dha_model) + scale_x_log10()
pp_check(epa_model) + scale_x_log10()

# pp_check(gam_salmon2, type = "boxplot") + scale_y_log10()
pp_check(hg_model, type = "boxplot") + scale_y_log10()
pp_check(pcb_model, type = "boxplot") + scale_y_log10()
pp_check(nit_model, type = "boxplot") + scale_y_log10()
pp_check(phos_model, type = "scatter_avg") + scale_y_log10() 


# 3) Test out any suspect models

nit_posts <- readRDS("posteriors/nit_posts.rds")
nit_posts %>% 
  ggplot(aes(x = species, y = .epred)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(data = nit_model$data %>%  as_tibble() %>% mutate(species = "All"),
             aes(x = species, y = mean_concentration_standardized))

phos_posts <- readRDS("posteriors/phos_posts.rds")
phos_posts %>% 
  ggplot(aes(x = species, y = .epred)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(data = phos_model$data %>%  as_tibble() %>% mutate(species = "All"),
              aes(x = species, y = mean_concentration_standardized))


library(tidyverse)

# draw posteriors

stat = "mean"
hg_stat = pp_check(hg_model, type = "stat", stat = stat)
pcb_stat = pp_check(pcb_model, type = "stat", stat = stat)
nit_stat = pp_check(nit_model, type = "stat", stat = stat)
phos_stat = pp_check(phos_model, type = "stat", stat = stat)
dha_stat = pp_check(dha_model, type = "stat", stat = stat)
epa_stat = pp_check(epa_model, type = "stat", stat = stat)
ddt_stat = pp_check(ddt_model, type = "stat", stat = stat)
pbde_stat = pp_check(pbde_model, type = "stat", stat = stat)


# Function to calculate bayesian p-value
calculate_p_value <- function(model) {
  model$data %>% 
    mutate(true_value = layer_data(model, 2)[[2]],
           diff = value - true_value) %>% 
    reframe(p_value = sum(diff > 0) / nrow(.)) %>% 
    select(p_value)  # Selecting only the p_value column
}

models <- list(e_hg_stat = hg_stat, 
                 f_pcb_stat = pcb_stat,
                 a_nit_stat = nit_stat,
                 b_phos_stat = phos_stat,
                 d_dha_stat = dha_stat,
                 c_epa_stat = epa_stat,
                 h_ddt_stat = ddt_stat,
                 g_pbde_stat = pbde_stat)

# Applying the function to each dataset in the list
processed_data <- lapply(names(models), function(name) {
  dataset <- models[[name]]
  processed <- calculate_p_value(dataset) %>% mutate(dataset = name)
})

# Combine results into a single tibble
combined_pvalues <- bind_rows(processed_data) %>% arrange(dataset) %>% 
  mutate(p_value = round(p_value, 2))

combined_pvalues









# posterior predictive - GAMs. Have to do this by hand, since pp_check won't work with gams that have a "by = " variable.

# get posts
post_gam_preds = gam_salmon2$data %>% 
  add_predicted_draws(gam_salmon2, ndraws = 500) %>% 
  pivot_longer(cols = c(y_10000, .prediction)) %>% 
  mutate(.draw = case_when(name == "y_10000" ~ 0,
                           TRUE ~ .draw)) %>% 
  distinct(species_location, year, .draw, name, value)

escapement_pvalue = post_gam_preds %>% 
  filter(.draw != 0) %>% 
  ungroup %>% 
  mutate(true_value = mean(gam_salmon2$data$y_10000)) %>% 
  mutate(diff = value - true_value) %>% 
  reframe(p_value = sum(diff>0)/nrow(.)) %>% 
  mutate(p_value = round(p_value, 2))

pp_check_colors = bayesplot::color_scheme_get()

theme_set(bayesplot::theme_default())

escapement_stat = post_gam_preds %>% 
  filter(.draw != 0) %>% 
  group_by(.draw, name) %>% 
  reframe(median = mean(value*1000)) %>% 
  ggplot(aes(x = median)) + 
  geom_histogram(fill = pp_check_colors$light,
                 color = pp_check_colors$light_highlight) +
  bayesplot_theme_set() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) + 
  geom_vline(data = post_gam_preds %>% 
               filter(.draw == 0) %>% 
               group_by(.draw, name) %>% 
               reframe(median = mean(value*1000)),
             aes(xintercept = median),
             linewidth = 1.5, 
             color = pp_check_colors$dark) +
  scale_y_continuous(expand = c(0, 0)) + 
  labs(title = "Escapement",
       subtitle = paste("Bayes P = ", escapement_pvalue[[1]]),
       x = "Escapement metric tons wet mass/yr")
  
ggview::ggview(escapement_stat, width = 6.5, height = 6.5)
ggsave(escapement_stat, width = 6.5, height = 6.5, file = "plots/Figure_S6.jpg",
       dpi = 500)
saveRDS(escapement_stat, file =  "plots/Figure_S6.rds")

# combine
library(patchwork)
a = nit_stat + 
  guides(fill = "none", color = "none") +
  theme(legend.position = c(0.9, 0.8)) +
  labs(title = "a) N",
       subtitle = paste("Bayes P = ", combined_pvalues$p_value[[1]]))
b = phos_stat + 
  # guides(fill = "none", color = "none") +
  theme(legend.position = c(0.8, 0.8)) +
  labs(title = "b) P",
       subtitle = paste("Bayes P = ", combined_pvalues$p_value[[2]]))
c = epa_stat + 
  guides(fill = "none", color = "none") +
  theme(legend.position = c(0.8, 0.8)) +
  labs(title = "c) EPA",
       subtitle = paste("Bayes P = ", combined_pvalues$p_value[[3]]))
d = dha_stat + 
  guides(fill = "none", color = "none") +
  theme(legend.position = c(0.8, 0.8)) +
  labs(title = "d) DHA",
       subtitle = paste("Bayes P = ", combined_pvalues$p_value[[4]]))
e = hg_stat + 
  guides(fill = "none", color = "none") +
  theme(legend.position = c(0.8, 0.8)) +
  labs(title = "e) Hg",
       subtitle = paste("Bayes P = ", combined_pvalues$p_value[[5]]))
f = pcb_stat + 
  guides(fill = "none", color = "none") +
  theme(legend.position = c(0.8, 0.8)) +
  labs(title = "f) PCBs",
       subtitle = paste("Bayes P = ", combined_pvalues$p_value[[6]]))
g = pbde_stat + 
  guides(fill = "none", color = "none") +
  theme(legend.position = c(0.8, 0.8)) +
  labs(title = "g) PBDEs",
       subtitle = paste("Bayes P = ", combined_pvalues$p_value[[7]]))
h = ddt_stat + 
  guides(fill = "none", color = "none") +
  theme(legend.position = c(0.8, 0.8)) +
  labs(title = "h) DDTs",
       subtitle = paste("Bayes P = ", combined_pvalues$p_value[[8]]))

postpred_contnut = (a + b + c + d)/(e + f + g + h)
ggview::ggview(postpred_contnut, width = 6.5, height = 8)
ggsave(postpred_contnut, width = 6.5, height = 8, file = "plots/Figure_S6.jpg", dpi = 500)
saveRDS(postpred_contnut, file = "plots/Figure_S6.rds")

## grouped means
hg_stat_grouped = pp_check(hg_model, type = "stat_grouped", group = "species", stat = stat)
pcb_stat_grouped = pp_check(pcb_model, type = "stat_grouped", group = "species", stat = stat)
nit_stat_grouped = pp_check(nit_model, type = "stat_grouped", group = "species", stat = stat)
phos_stat_grouped = pp_check(phos_model, type = "stat_grouped", group = "species", stat = stat)
dha_stat_grouped = pp_check(dha_model, type = "stat_grouped", group = "species", stat = stat)
epa_stat_grouped = pp_check(epa_model, type = "stat_grouped", group = "species", stat = stat)
ddt_stat_grouped = pp_check(ddt_model, type = "stat_grouped", group = "species", stat = stat)
pbde_stat_grouped = pp_check(pbde_model, type = "stat_grouped", group = "species", stat = stat)




