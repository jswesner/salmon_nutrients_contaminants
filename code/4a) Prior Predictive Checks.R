library(tidybayes)
library(tidyverse)
library(ggthemes)
library(scales)
library(brms)

# 1) bring in fitted models
gam_salmon2 <- readRDS("models/gam_salmon2.rds")
hg_model <- readRDS("models/hg_model.rds") 
pcb_model <- readRDS("models/pcb_model.rds")
nit_model <- readRDS("models/nit_model.rds") 
phos_model <- readRDS("models/phos_model.rds") 
epa_model <- readRDS("models/epa_model.rds")
dha_model <- readRDS("models/dha_model.rds")
pbde_model <- readRDS("models/pbde_model.rds")
ddt_model <- readRDS("models/ddt_model.rds")

# 2) re-fit with priors only
# hg_model_prior = update(hg_model, sample_prior = "only")
# pcb_model_prior = update(pcb_model, sample_prior = "only")
# pbde_model_prior = update(pbde_model, sample_prior = "only")
# ddt_model_prior = update(ddt_model, sample_prior = "only")
# nit_model_prior = update(nit_model, sample_prior = "only")
# phos_model_prior = update(phos_model, sample_prior = "only")
# dha_model_prior = update(dha_model, sample_prior = "only")
# epa_model_prior = update(epa_model, sample_prior = "only")
# 
# saveRDS(hg_model_prior, file = "models/hg_model_prior.rds")
# saveRDS(pcb_model_prior, file = "models/pcb_model_prior.rds")
# saveRDS(pbde_model_prior, file = "models/pbde_model_prior.rds")
# saveRDS(ddt_model_prior, file = "models/ddt_model_prior.rds")
# saveRDS(nit_model_prior , file = "models/nit_model_prior.rds")
# saveRDS(phos_model_prior, file = "models/phos_model_prior.rds")
# saveRDS(dha_model_prior, file = "models/dha_model_prior.rds")
# saveRDS(epa_model_prior, file = "models/epa_model_prior.rds")

# this is already done. Bring in prior models
gam_salmon2_prior = readRDS("models/gam_salmon2_prior.rds")
hg_model_prior = readRDS(file = "models/hg_model_prior.rds")
pcb_model_prior = readRDS(file = "models/pcb_model_prior.rds")
pbde_model_prior = readRDS(file = "models/pbde_model_prior.rds")
ddt_model_prior = readRDS(file = "models/ddt_model_prior.rds")
nit_model_prior = readRDS(file = "models/nit_model_prior.rds")
phos_model_prior = readRDS(file = "models/phos_model_prior.rds")
dha_model_prior = readRDS(file = "models/dha_model_prior.rds")
epa_model_prior = readRDS(file = "models/epa_model_prior.rds")

# get prior table
p1 = gam_salmon2_prior$prior %>% mutate(model = "Escapement")
p2 = hg_model$prior %>% mutate(model = "hg")
p3 = pcb_model$prior %>% mutate(model = "pcb")
p4 = pbde_model$prior %>% mutate(model = "pbde")
p5 = ddt_model$prior %>% mutate(model = "ddt_model")
p6 = nit_model$prior %>% mutate(model = "nit_model")
p7 = phos_model$prior %>% mutate(model = "phos_model")
p8 = dha_model$prior %>% mutate(model = "dha_model")
p9 = epa_model$prior %>% mutate(model = "epa_model")

prior_table = bind_rows(p1,p2,p3,p4,p5,p6,p7,p8,p9)
write_csv(prior_table, file = "ms/revision/prior_table.csv")

# Compare analyte prior and posterior --------------------------------------------------
# 3) Get all prior samples
# Make a list
prior_models = list(`e) Hg` = hg_model_prior, 
                    `f) PCBs` = pcb_model_prior, 
                    `h) PBDEs` = pbde_model_prior, 
                    `g) DDT` = ddt_model_prior,
                    `a) N` = nit_model_prior, 
                    `b) P` = phos_model_prior, 
                    `c) DHA` = dha_model_prior, 
                    `d) EPA` = epa_model_prior)
# Make an empty object
prior_samples_temp = NULL

# Extract samples from each model
for(i in 1:length(prior_models)){
  prior_samples_temp[[i]] = prior_models[[i]]$data %>% 
    select(-mean_concentration_standardized, -authors, -region) %>% 
    distinct() %>% 
    add_epred_draws(prior_models[[i]], re_formula = NA) %>% 
    mutate(model = "Prior",
           analyte = names(prior_models[i]))
}

# 4) Get all posterior samples
# Make a list
posterior_models = list(`e) Hg` = hg_model, 
                        `f) PCBs` = pcb_model, 
                        `h) PBDEs` = pbde_model, 
                        `g) DDT` = ddt_model,
                        `a) N` = nit_model, 
                        `b) P` = phos_model, 
                        `c) DHA` = dha_model, 
                        `d) EPA` = epa_model)

# Make an empty object
posterior_samples_temp = NULL

# Extract samples from each model
for(i in 1:length(posterior_models)){
  posterior_samples_temp[[i]] = posterior_models[[i]]$data %>% 
    select(-mean_concentration_standardized, -authors, -region) %>% 
    distinct() %>% 
    add_epred_draws(posterior_models[[i]], re_formula = NA) %>% 
    mutate(model = "Posterior",
           analyte = names(posterior_models[i]))
}

# 4) Combine samples
# bind all samples
prior_post_samples = bind_rows(prior_samples_temp, posterior_samples_temp)

# add species names to N and P (they are intercept only models, so have to do this by hand)
prior_post_NP = prior_post_samples %>% 
  filter(analyte == "a) N" | analyte == "b) P") %>% 
  ungroup %>% 
  select(-species) %>% 
  expand_grid(species = c("Sockeye", "Pink", "Coho", "Chum", "Chinook"))

# combine with species names for N and P
prior_post_all = prior_post_samples %>% filter(!is.na(species)) %>% 
  bind_rows(prior_post_NP)


# 5) plot prior vs posteriors

prior_post_analytes = prior_post_all %>% 
  mutate(model = as.factor(model),
         model = fct_relevel(model, "Prior")) %>% 
  ggplot(aes(x = .epred, y = species, fill = model)) + 
  stat_slab(alpha = 0.6, scale = 1) +
  facet_wrap(~analyte, scales = "free_y", ncol = 2) +
  scale_x_log10(labels = comma) + 
  scale_fill_manual(values = c("#000000", "#e69f00")) +
  labs(y = "",
       color = "",
       fill = "",
       x = "Tissue Concentration (mg/kg ww)") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0))

ggview::ggview(prior_post_analytes, width = 6.5, height = 8)

ggsave(prior_post_analytes, file = "plots/ms_plots/prior_post_analytes.jpg", 
       width = 6.5, height = 8, dpi = 500)
saveRDS(prior_post_analytes, file = "plots/ms_plots/prior_post_analytes.rds")









# Compare escapement prior and posterior ----------------------------------

escape_priors = plot(conditional_effects(gam_salmon2_prior, effect = "year:species_location"))
escape_posteriors = plot(conditional_effects(gam_salmon2, effect = "year:species_location"))

escape_prior_post = escape_priors$`year:species_location`$data %>% mutate(model = "Prior") %>% 
  bind_rows(escape_posteriors $`year:species_location`$data %>% mutate(model = "Posterior"))

prior_post_escapement = escape_prior_post %>% 
  mutate(model = as.factor(model),
         model = fct_relevel(model, "Prior")) %>%
  separate(species_location, into = c("species", "location")) %>%
  mutate(location = as.factor(location),
         location = fct_relevel(location, "SEAK", "BeringSea", "CentralAK")) %>% 
  ggplot(aes(x = year, y = estimate__, ymin = lower__, ymax = upper__)) + 
  geom_ribbon(aes(fill = model, alpha = model)) + 
  geom_line(aes(group = model), linewidth = 0.1) +
  facet_grid(species ~ location) + 
  scale_y_log10(labels = comma) +
  scale_alpha_manual(values = c(0.3, 0.9)) + 
  scale_fill_manual(values = c("#000000", "#e69f00")) +
  guides(alpha = "none",
         colour = guide_legend(override.aes = list(alpha = .03))) +
  labs(color = "",
       fill = "",
       x = "Year", 
       y = "Return biomass: metric tons per year") +
  theme(text = element_text(size = 9),
        legend.text = element_text(size = 9),
        axis.text.x = element_text(size = 7))

ggview::ggview(prior_post_escapement, width = 6.5, height = 6)

ggsave(prior_post_escapement, file = "plots/ms_plots/prior_post_escapement.jpg", 
       width = 6.5, height = 6, dpi = 500)
saveRDS(prior_post_escapement, file = "plots/ms_plots/prior_post_escapement.rds")




# Prior vs Posterior parameter values -----------
# Contaminants and Nutrients only

prior_params_temp = NULL

# Extract samples from each model
for(i in 1:length(prior_models)){
  prior_params_temp[[i]] = prior_models[[i]] %>% 
    as_draws_df()  %>% 
    select(-lprior, -lp__, -.chain, -.iteration, -.draw) %>% 
    select(-starts_with("r_")) %>% 
    mutate(model = "Prior",
           analyte = names(prior_models[i]))
}


posterior_params_temp = NULL

# Extract samples from each model
for(i in 1:length(posterior_models)){
  posterior_params_temp[[i]] = posterior_models[[i]] %>% 
    as_draws_df()  %>% 
    select(-lp__, -.chain, -.iteration, -.draw) %>% 
    select(-starts_with("r_")) %>% 
    mutate(model = "Posterior",
           analyte = names(posterior_models[i]))
}

prior_post_params = bind_rows(posterior_params_temp, prior_params_temp) %>% 
  pivot_longer(cols = c(-lprior, -model, -analyte, -starts_with("prior"))) %>% 
  filter(!is.na(value)) %>% 
  mutate(model = as.factor(model),
         model = fct_relevel(model, "Prior")) 


prior_post_parameters = prior_post_params %>% 
  group_by(analyte, model, name) %>% 
  median_qi(value) %>% 
  ggplot(aes(x = name, color = model, y = value)) + 
  # stat_pointinterval(position = position_dodge(width = 0.5),
  #                    size = 0.02) +
  # stat_slab(position = position_dodge(width = 0.5),
  #                    size = 0.4) +
  geom_point(size = 0.4, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = .lower, ymax = .upper), 
                 linewidth = 0.1, 
                 position = position_dodge(width = 0.5)) + 
  facet_wrap(~analyte, ncol = 2) +
  coord_flip()  + 
  scale_color_manual(values = c("#000000", "#e69f00")) +
  labs(x = "Parameter Value",
       y = "Parameter Name",
       color = "") +
  theme_default() 

ggview::ggview(prior_post_parameters, width = 6.5, height = 8)
ggsave(prior_post_parameters, width = 6.5, height = 8,
       file = "plots/ms_plots/prior_post_parameters.jpg", dpi = 500)
saveRDS(prior_post_parameters, file = "plots/ms_plots/prior_post_parameters.rds")


# Escapement
escapement_params_prior = gam_salmon2_prior %>% as_draws_df() %>% mutate(model = "Prior")
escapement_params_posterior = gam_salmon2 %>% as_draws_df() %>% mutate(model = "Posterior")

escapement_params = bind_rows(escapement_params_posterior,
          escapement_params_prior) %>% 
  select(starts_with("bs_syear"), starts_with("sds"),b_Intercept, model) %>% 
  pivot_longer(cols = -model)

prior_post_parameters_escapement = escapement_params %>% 
  group_by(model, name) %>% 
  median_qi(value) %>%
  separate(name, into = c("parameter", "group"), extra = "merge") %>% 
  mutate(parameter = case_when(parameter == "b" ~ "bs",
                               TRUE ~ parameter)) %>% 
  mutate(group = str_remove(group, "syearspecies_location"),
         group = str_remove(group, "syear:species_location")) %>% 
  mutate(group = as.factor(group),
         group = fct_relevel(group, "Intercept"),
         ) %>% 
  ggplot(aes(x = group, color = model, y = value)) + 
  # stat_pointinterval(position = position_dodge(width = 0.5),
  #                    size = 0.02) +
  # stat_slab(position = position_dodge(width = 0.5),
  #                    size = 0.4) +
  facet_wrap(~parameter, scales = "free_x") +
  geom_point(size = 0.4, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = .lower, ymax = .upper), 
                 linewidth = 0.1, 
                 position = position_dodge(width = 0.5)) + 
  coord_flip()  + 
  scale_color_manual(values = c("#000000", "#e69f00")) +
  labs(x = "Parameter Value",
       y = "Parameter Name",
       color = "") +
  theme_default() +
  theme(axis.text.y = element_text(size = 9))

ggview::ggview(prior_post_parameters_escapement, width = 6.5, height = 5)
ggsave(prior_post_parameters_escapement, width = 6.5, height = 6,
       file = "plots/ms_plots/prior_post_parameters_escapement.jpg", dpi = 500)
saveRDS(prior_post_parameters_escapement, file = "plots/ms_plots/prior_post_parameters_escapement.rds")

