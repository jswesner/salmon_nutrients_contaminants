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
write_csv(prior_table, file = "tables/prior_table.csv")

# Compare analyte prior and posterior --------------------------------------------------
# 3) Get all prior samples
# Make a list
prior_models = list(`Hg` = hg_model_prior, 
                    `PCBs` = pcb_model_prior, 
                    `PBDEs` = pbde_model_prior, 
                    `DDTs` = ddt_model_prior,
                    `N` = nit_model_prior, 
                    `P` = phos_model_prior, 
                    `DHA` = dha_model_prior, 
                    `EPA` = epa_model_prior)
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
posterior_models = list(`Hg` = hg_model, 
                        `PCBs` = pcb_model, 
                        `PBDEs` = pbde_model, 
                        `DDTs` = ddt_model,
                        `N` = nit_model, 
                        `P` = phos_model, 
                        `DHA` = dha_model, 
                        `EPA` = epa_model)

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

# add species names to P (it is intercept only, so have to do this by hand)
prior_post_P = prior_post_samples %>% 
  filter(analyte == "b) P") %>% 
  ungroup %>% 
  select(-species) %>% 
  expand_grid(species = c("Sockeye", "Pink", "Coho", "Chum", "Chinook"))

# combine with species names for N and P
prior_post_all = prior_post_samples %>% filter(!is.na(species)) %>% 
  bind_rows(prior_post_P)

saveRDS(prior_post_all, file = "plots/fig_s5_data.rds")




# Compare escapement prior and posterior ----------------------------------

escape_priors = plot(conditional_effects(gam_salmon2_prior, effect = "year:species_location"))
escape_posteriors = plot(conditional_effects(gam_salmon2, effect = "year:species_location"))

escape_prior_post = escape_priors$`year:species_location`$data %>% mutate(model = "Prior") %>% 
  bind_rows(escape_posteriors $`year:species_location`$data %>% mutate(model = "Posterior"))

saveRDS(escape_prior_post, file = "plots/fig_s4_data.rds")



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
         model = fct_relevel(model, "Prior")) %>% 
  mutate(parameter_type = case_when(name == "shape" ~ "Shape",
                                    grepl("sd_", name) ~ "Hyperparameter",
                                    name == "b_Intercept" ~ "Intercept",
                                    TRUE ~ "Betas"))

saveRDS(prior_post_params, file = "plots/fig_s2_data.rds")


# show for N
prior_N_plot = prior_post_params %>% 
  filter(analyte == "a) N") %>% 
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
  facet_wrap(~name, ncol = 2, scales = "free") +
  coord_flip()  + 
  scale_color_manual(values = c("#000000", "#e69f00")) +
  labs(y = "Parameter Value",
       x = "Parameter Name",
       color = "") +
  theme_default() 

ggsave(prior_N_plot, file = "plots/prior_N_plot.jpg", width = 6.5, height = 6)

# Escapement
escapement_params_prior = gam_salmon2_prior %>% as_draws_df() %>% mutate(model = "Prior")
escapement_params_posterior = gam_salmon2 %>% as_draws_df() %>% mutate(model = "Posterior")

escapement_params = bind_rows(escapement_params_posterior,
          escapement_params_prior) %>% 
  select(starts_with("bs_syear"), starts_with("sds"),b_Intercept, model) %>% 
  pivot_longer(cols = -model)

saveRDS(escapement_params, file = "plots/fig_s3_data.rds")
