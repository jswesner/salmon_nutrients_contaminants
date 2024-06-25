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

gam_salmon2

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
    dplyr::select(p_value)  # Selecting only the p_value column
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
ggsave(escapement_stat, width = 6.5, height = 6.5, file = "plots/fig_s6.jpg",
       dpi = 500)
saveRDS(escapement_stat, file =  "plots/fig_s6.rds")

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
ggsave(postpred_contnut, width = 6.5, height = 8, file = "plots/fig_s7.jpg", dpi = 500)
saveRDS(postpred_contnut, file = "plots/fig_s7.rds")

## grouped means
hg_stat_grouped = pp_check(hg_model, type = "stat_grouped", group = "species", stat = stat)
pcb_stat_grouped = pp_check(pcb_model, type = "stat_grouped", group = "species", stat = stat)
nit_stat_grouped = pp_check(nit_model, type = "stat_grouped", group = "species", stat = stat)
phos_stat_grouped = pp_check(phos_model, type = "stat_grouped", group = "species", stat = stat)
dha_stat_grouped = pp_check(dha_model, type = "stat_grouped", group = "species", stat = stat)
epa_stat_grouped = pp_check(epa_model, type = "stat_grouped", group = "species", stat = stat)
ddt_stat_grouped = pp_check(ddt_model, type = "stat_grouped", group = "species", stat = stat)
pbde_stat_grouped = pp_check(pbde_model, type = "stat_grouped", group = "species", stat = stat)


# divergence --------------------------------------------------------------
mod_div = list(ddt_model,
                    pbde_model,
                    pcb_model,
                    hg_model,
                    # hg_model_adapt,
                    nit_model,
                    phos_model,
                    dha_model,
                    epa_model,
                    gam_salmon2)

cont_list = c("ddt", "pbde", "pcb", "hg",
              "nit", "phos", "dha", "epa",
              "salmon_abundance")

# extract the number of divergent transitions

divergence_list = NULL

for(i in 1:length(mod_div)){
  np = nuts_params(mod_div[[i]])
  divergence_list[[i]] = tibble(divergences = sum(subset(np, Parameter == "divergent__")$Value),
                                model = cont_list[[i]])
}

bind_rows(divergence_list)



# rhats -------------------------------------------------------------------
# load hg model with looser priors (see code below for fitting this model)
hg_model_adapt = readRDS(file = "models/hg_model_adapt.rds")

hg_model = update(hg_model)

mod_original = list(ddt_model,
                    pbde_model,
                    pcb_model,
                    hg_model,
                    hg_model_adapt,
                    nit_model,
                    phos_model,
                    dha_model,
                    epa_model)

cont_list = c("ddt", "pbde", "pcb", "hg", "hg_adapt", "nit", "phos", "dha", "epa")

rhat_list = NULL

for(i in 1:length(mod_original)){
  rhat_list[[i]] = brms::rhat(mod_original[[i]]) %>% as.list() %>% 
    as_tibble() %>% pivot_longer(cols = everything()) %>% 
    mutate(chemical = cont_list[i])
}

# check if rhats are >1.01. 
fig_s8_data = bind_rows(rhat_list) %>% 
  mutate(over = case_when(value > 1.01 ~ "too high",
                          TRUE ~ "good"),
         label = case_when(value > 1.01 ~ name)) %>%
  mutate(panel = case_when(chemical == "nit" ~ "N",
                           chemical == "phos" ~ "P",
                           chemical == "dha" ~ "DHA",
                           chemical == "epa" ~ "EPA",
                           chemical == "hg" ~ "Hg",
                           chemical == "hg_adapt" ~ "Hg_adapt",
                           chemical == "pcb" ~ "PCBs",
                           chemical == "ddt" ~ "DDTs",
                           chemical == "pbde" ~ "PBDEs")) %>% 
  mutate(panel = as.factor(panel),
         panel = fct_relevel(panel, "N", "P", "DHA", "EPA", "Hg", "Hg_adapt", "PCBs", "DDTs"))


write_csv(fig_s8_data, file = "plots/fig_s8_data.csv")

# there are some high rhats at all parameter levels for the hg model (beta, sd, varying intercepts). Try increasing adapt_delta
hg_model_adapt = update(hg_model,
                        control = list(adapt_delta = 0.9))

saveRDS(hg_model_adapt, file = "models/hg_model_adapt.rds")

# compare inferences from original `hg_model` to `hg_adapt`

hg_posts = hg_model$data %>% 
  distinct(species) %>% 
  add_epred_draws(hg_model, re_formula = NA) %>% 
  mutate(model = "hg_original")

hg_adapt_posts = hg_model$data %>% 
  distinct(species) %>% 
  add_epred_draws(hg_model_adapt, re_formula = NA) %>% 
  mutate(model = "hg_adapt")

fig_s9_data = bind_rows(hg_adapt_posts,
          hg_posts) 

write_csv(fig_s9_data, file = "plots/fig_s9_data.csv")


