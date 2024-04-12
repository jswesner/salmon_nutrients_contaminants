library(tidyverse)
library(brms)
library(tidybayes)
library(scales)
library(modelr)

# model contaminant and nutrient concentrations in fish

#load data
nut_cont <- readRDS("data/nut_cont.rds")

source("code/functions.R")


# single model ------------------------------------------------------------
# consider using this instead of separate models
nut_cont_scaled = nut_cont %>% 
  ungroup %>% 
  mutate(scaling = max(mean_concentration_standardized)) %>% 
  glimpse() %>% 
  mutate(scaled_conc = mean_concentration_standardized/scaling) %>% 
  filter(species != "All")

nut_cont_scaled %>% 
  ggplot(aes(x = scaled_conc, y = mean_concentration_standardized)) + 
  geom_point() 

scaled_brm = brm(scaled_conc ~ 1 + species*chemical + (1|authors) + (1|region),
                 family = Gamma(link = "log"),
                 data = nut_cont_scaled,
                 iter = 1000, chains = 1,
                 cores = 4)
saveRDS(scaled_brm, file = "models/scaled_brm.rds")
#extract posteriors

single_posts = scaled_brm$data %>% 
  distinct(species, chemical) %>% 
  add_epred_draws(scaled_brm, re_formula = NA) %>% 
  mutate(scaling = unique(nut_cont_scaled$scaling),
         .epred = .epred*scaling,
         model = "single model") %>% 
  select(-scaling) %>% 
  mutate(chemical = case_when(chemical == "DDT" ~ "DDTs",
                              chemical == "PBDE" ~ "PBDEs",
                              TRUE ~ chemical)) %>% 
  mutate(type = case_when(chemical == "N" | chemical == "P" | chemical =="DHA" | chemical == "EPA" ~ "Nutrients",
                          TRUE ~ "Contaminants"),
         chemical = as.factor(chemical),
         chemical = fct_relevel(chemical, "N", 
                                "P", 
                                "DHA", 
                                "EPA",
                                "Hg",
                                "DDTs"))

saveRDS(single_posts, file = "posteriors/single_posts.rds")


# Hg ----------------------------------------------------------------------

#data prep: check for duplicates
nut_cont %>% 
  filter(grepl("Hg", chemical)) %>% 
  group_by(specific_location, mean_concentration_standardized, n, species, region, data_collection_period) %>% 
  filter(n>1)

hg_data <- nut_cont %>% 
  filter(grepl("Hg", chemical)) %>% 
  mutate(mean_concentration_standardized = case_when(tissue == "Whole Body" ~ mean_concentration_standardized,
                                   TRUE ~ mean_concentration_standardized*0.74))

#check priors
#plot priors - only for a couple species, since all will be the same (same prior)
#NOTE - Model uses a no-intercept formula, so all groups get the same prior
prior_b = rnorm(1000, -5.5, 2)
prior_sd = rexp(1000, 2)
prior_shape = rgamma(1000, 5, 3)

sim_gamma_priors(prior_b = prior_b,
                 prior_sd = prior_sd,
                 prior_shape = prior_shape) +
  scale_y_log10(labels = comma, breaks = c(1e-4, 1e-3, 1e-2, 1e-1, 1e0, 1e1)) +
  geom_hline(yintercept = c(0.03, 0.018, 0.059, 0.028, 0.015, 0.028, 0.108, 0.056,
                            0.05))
# Prior ranges are for 
# prior generates ranges from the literature of ~ 0 to 0.1 mg/kg, e.g. https://www.sciencedirect.com/science/article/pii/S0048969718343055 

# fit model
hg_model <- brm(mean_concentration_standardized ~ 0 + species + (1|authors) + (1|region),
                family = Gamma(link = "log"),
                data = hg_data,
                prior = c(prior(normal(-5.5, 2), class = "b"),
                          prior(exponential(2), class = "sd"),
                          prior(gamma(5, 3), class = "shape")),
                cores = 4, 
                file = "models/hg_model.rds",
                file_refit = "on_change")


plot(conditional_effects(hg_model), points = T)

#extract posteriors

hg_posts <- hg_data %>% ungroup %>% 
  distinct(species, concentration_units_standardized)%>% 
  add_epred_draws(hg_model, re_formula = NA) %>%
  mutate(chemical = "Hg")

saveRDS(hg_posts, file = "posteriors/hg_posts.rds")


# DHA  ----------------------------------------------------------------------

#data prep: check for duplicates
nut_cont %>% 
  filter(grepl("DHA", chemical)) %>% 
  group_by(specific_location, mean_concentration_standardized, n, species, region, data_collection_period) %>% 
  filter(n>1) 

dha_data <- nut_cont %>% 
  filter(grepl("DHA", chemical)) %>% 
  distinct() 

#check priors
#Sprague, M., Dick, J. R., & Tocher, D. R. (2016). Impact of sustainable feeds on omega-3 long-chain fatty acid 
#levels in farmed Atlantic salmon, 2006–2015. Scientific reports, 6(1), 1-9.
#reported ~ 10000 EPA + DHA mg/kg ww. ! HALF IS DHA 

#plot priors - only for a couple species, since all will be the same (same prior)
#NOTE - Model uses a no-intercept formula, so all groups get the same prior
prior_b = rnorm(1000, 8.5, 1)
prior_sd = rexp(1000, 2)
prior_shape = rgamma(1000, 5, 3)

sim_gamma_priors(prior_b = prior_b,
                 prior_sd = prior_sd,
                 prior_shape = prior_shape) +
  scale_y_log10(labels = comma) +
  geom_hline(yintercept = 5000)

# prior generates ranges from the literature of ~ 0 to 100 ug/kg, e.g. https://www.sciencedirect.com/science/article/pii/S0048969718343055 

# fit model
dha_model <- brm(mean_concentration_standardized ~ 0 + species + (1|authors) + (1|region),
                family = Gamma(link = "log"),
                data = dha_data,
                prior = c(prior(normal(8.5, 1), class = "b"),
                          prior(exponential(2), class = "sd"),
                          prior(gamma(5, 3), class = "shape")),
                file = "models/dha_model.rds",
                file_refit = "on_change",
                cores = 4)

plot(conditional_effects(dha_model), points = T)

#extract posteriors

dha_posts <- dha_data %>% ungroup %>% 
  distinct(species, concentration_units_standardized) %>% 
  add_epred_draws(dha_model, re_formula = NA) %>% 
  mutate(chemical = "DHA")

saveRDS(dha_posts, file = "posteriors/dha_posts.rds")


# EPA  ----------------------------------------------------------------------

#data prep: check for duplicates
nut_cont %>% 
  filter(grepl("EPA", fa_class)) %>% 
  group_by(specific_location, mean_concentration_standardized, n, species, region, data_collection_period) %>% 
  filter(n>1) 

epa_data <- nut_cont %>% 
  filter(grepl("EPA", chemical)) %>% 
  distinct() 

#check priors
#Sprague, M., Dick, J. R., & Tocher, D. R. (2016). Impact of sustainable feeds on omega-3 long-chain fatty acid 
#levels in farmed Atlantic salmon, 2006–2015. Scientific reports, 6(1), 1-9.
#reported ~ 5000 epa mg/kg ww. ~ HALF IS EPA

#plot priors - only for a couple species, since all will be the same (same prior)
#NOTE - Model uses a no-intercept formula, so all groups get the same prior
prior_b = rnorm(1000, 8.5, 1)
prior_sd = rexp(1000, 3)
prior_shape = rgamma(1000, 5, 3)

sim_gamma_priors(prior_b = prior_b,
                 prior_sd = prior_sd,
                 prior_shape = prior_shape) +
  geom_hline(yintercept = 5000) +
  scale_y_log10(labels = comma)

# prior generates ranges from the literature of ~ 0 to 100 ug/kg, e.g. https://www.sciencedirect.com/science/article/pii/S0048969718343055 

# fit model
epa_model <- brm(mean_concentration_standardized ~ 0 + species + (1|authors) + (1|region),
                 family = Gamma(link = "log"),
                 data = epa_data,
                 prior = c(prior(normal(8.5,1), class = "b"),
                           prior(exponential(3), class = "sd"),
                           prior(gamma(5, 3), class = "shape")),
                 file = "models/epa_model.rds",
                 file_refit = "on_change",
                 cores = 4)

plot(conditional_effects(epa_model), points = T)

#extract posteriors

epa_posts <-  epa_data %>% ungroup %>% 
  distinct(species, concentration_units_standardized) %>% 
  add_epred_draws(epa_model, re_formula = NA) %>% 
  mutate(chemical = "EPA")

saveRDS(epa_posts, file = "posteriors/epa_posts.rds")

# Nitrogen ----------------------------------------------------------------------

#check for duplicates
nit_data <- nut_cont %>% 
  filter(chemical == "N") %>% 
  filter(species != "All") %>%
  mutate(region = case_when(is.na(region) ~ "All",
                            TRUE ~ region))

#check priors
#plot priors - only for one species, since all will be the same (same prior)
prior_b = rnorm(1000, 10, .5)
prior_sd = rexp(1000, 5)
prior_shape = rgamma(1000, 10, 1)

sim_gamma_priors(prior_b = prior_b,
                 prior_sd = prior_sd,
                 prior_shape = prior_shape) +
  scale_y_log10() +
  geom_hline(yintercept = 25000)

#" Salmon molar N:P ratios range from 12:1 to 15:1 (Gende 2002
# "Gende, S. M., Edwards, R. T., Willson, M. F., & Wipfli, M. S. (2002). Pacific salmon in aquatic and terrestrial ecosystems. BioScience, 52(10), 917-928."
# plot above generates some values above 75000 mg/kg N, but mostly lower than that. 

# fit model - no random effects b/c only 6 samples total
nit_model <- brm(mean_concentration_standardized ~ 1 + species + (1|authors) + (1|region),
                 family = Gamma(link = "log"),
                 data = nit_data,
                 prior = c(prior(normal(10, 0.5), class = "Intercept"),
          prior(gamma(10, 1), class = "shape"),
          prior(normal(0, 0.5), class = "b"),
          prior(exponential(5), class = "sd")),
          file = "models/nit_model.rds",
          file_refit = "on_change",
          cores = 4)

# extract posteriors
# # posterior extraction is slightly different code b/c there are no fixed effects in this model
# nit_posts <- as_draws_df(nit_model) %>% 
#   mutate(.draw = 1:nrow(.)) %>% 
#   mutate(.epred = exp(b_Intercept),
#          chemical = "N") %>% 
#   expand_grid(species = nit_data %>% filter(species != "All") %>% distinct(species) %>% pull)

nit_posts = nit_model$data %>% 
  distinct(species, authors, region) %>% 
  mutate(concentration_units_standardized = unique(nit_data$concentration_units_standardized)) %>% 
  add_epred_draws(nit_model) %>% 
  group_by(species, concentration_units_standardized, .draw) %>% 
  reframe(.epred = mean(.epred)) %>% 
  mutate(chemical = "N")

nit_posts %>% 
  ggplot(aes(x = species, y = .epred)) +
  geom_violin() +
  geom_point(data = nit_data,
             aes(y = mean_concentration_standardized)) +
  ylim(15000, 60000)

saveRDS(nit_posts, file = "posteriors/nit_posts.rds")

# Phosphorous ----------------------------------------------------------------------

#check for duplicates

phos_data <- nut_cont %>% 
  filter(chemical == "P") %>% 
  filter(species != "All") %>% 
  distinct()

#check priors
#plot priors - only for a couple species, since all will be the same (same prior)

#" Salmon molar N:P ratios range from 12:1 to 15:1 (Gende 2002
# "Gende, S. M., Edwards, R. T., Willson, M. F., & Wipfli, M. S. (2002). Pacific salmon in aquatic and terrestrial ecosystems. BioScience, 52(10), 917-928."
# Given the above ratios, 2000-5000 mg/kg of P is typical for salmon.

prior_b = rnorm(1000, 8, 0.5)
prior_sd = rexp(1000, 8)
prior_shape = rgamma(1000, 4, 2)

sim_gamma_priors(prior_b = prior_b,
                 prior_sd = prior_sd,
                 prior_shape = prior_shape) +
  scale_y_log10() +
  geom_hline(yintercept = 3500)



# fit model
phos_model <- brm(mean_concentration_standardized ~ 1 + (1|authors) + (1|region),
                  family = Gamma(link = "log"),
                  data = phos_data,
                  prior = c(prior(normal(8, 0.5), class = "Intercept"),
                            prior(gamma(4, 2), class = "shape"),
                            prior(exponential(8), class = "sd")),
                  file = "models/phos_model.rds",
                  file_refit = "on_change",
                  cores = 4)

# extract posteriors
phos_posts = phos_data %>% ungroup %>% 
  distinct(species, authors, region, concentration_units_standardized) %>% 
  add_epred_draws(phos_model) %>% 
  group_by(species, concentration_units_standardized, .draw) %>% 
  reframe(.epred = mean(.epred)) %>% 
  mutate(chemical = "P")

phos_posts %>% 
  ggplot(aes(x = species, y = .epred)) +
  geom_violin() +
  geom_point(data = phos_data,
             aes(y = mean_concentration_standardized)) 

saveRDS(phos_posts, file = "posteriors/phos_posts.rds")

# PCB ----------------------------------------------------------------------

#data prep: check for duplicates
pcb_data <- nut_cont %>% 
  filter(chemical == "PCBs") %>% 
  mutate(mean_concentration_standardized = case_when(tissue == "Whole Body" ~ mean_concentration_standardized,
                                   TRUE ~ mean_concentration_standardized*2.3)) %>% 
  distinct()
    
#check priors
#plot priors - only for a couple species, since all will be the same (same prior)
prior_b = rnorm(1000, -5.5, 1)
prior_sd = rexp(1000, 2)
prior_shape = rgamma(1000, 5, 2)

sim_gamma_priors(prior_b = prior_b,
                 prior_sd = prior_sd,
                 prior_shape = prior_shape) +
  scale_y_log10(labels = comma) +
  annotation_logticks() +
  geom_hline(yintercept = c(0.0025, 
                            0.0095,
                            0.028))


#numbers above are consistent with Montory, M., Habit, E., Fernandez, P., Grimalt, J. O., & Barra, R. (2010). PCBs and PBDEs in wild Chinook salmon (Oncorhynchus tshawytscha) in the Northern Patagonia, Chile. Chemosphere, 78(10), 1193-1199.
# 25-78 ng/g (e.g., 0.025 to 0.078 mg/kg) from Montroy et al. 2010 abstract. *NOTE ng/g is the same as ug/kg

# fit model
pcb_model <- brm(mean_concentration_standardized ~ 0 + species + (1|authors) + (1|region),
                 family = Gamma(link = "log"),
                 data = pcb_data,
                 prior = c(prior(normal(-5.5, 1), class = "b"),
                           prior(exponential(2), class = "sd"),
                           prior(gamma(5, 2), class = "shape")),
                 sample_prior = T,
                 file = "models/pcb_model.rds",
                 file_refit = "on_change",
                 cores = 4)

plot(conditional_effects(pcb_model), points = T)

#extract posteriors
pcb_posts <- pcb_data %>% ungroup %>% 
  distinct(species, concentration_units_standardized) %>% 
  add_epred_draws(pcb_model, re_formula = NA) %>% 
  mutate(chemical = "PCBs")

saveRDS(pcb_posts, file = "posteriors/pcb_posts.RDS")


# PBDE ----------------------------------------------------------------------

#data prep: check for duplicates

#correct for whole body, fillet with skin, and fillet without skin using Stone, D. (2006). Polybrominated diphenyl ethers and polychlorinated biphenyls in different tissue types from Chinook salmon 
#(Oncorhynchus tshawytscha). Bulletin of environmental contamination and toxicology, 76(1), 148-154.

pbde_data <- nut_cont %>%
  filter(grepl("PBD", chemical)) %>% 
  mutate(mean_concentration_standardized = case_when(tissue == "Fillet" ~ mean_concentration_standardized*1.5,
                                   tissue == "Fillet+Skin" ~ mean_concentration_standardized*1.27,
                                   TRUE ~ mean_concentration_standardized))

#check priors
#plot priors - only for a couple species, since all will be the same (same prior)
prior_b = rnorm(1000, -7, 2)
prior_sd = rexp(1000, 3)
prior_shape = rgamma(1000, 5, 1)

sim_gamma_priors(prior_b = prior_b,
                 prior_sd = prior_sd,
                 prior_shape = prior_shape) +
  scale_y_log10() +
  geom_hline(yintercept = 0.0018)


#numbers above are consistent with Stone, D. (2006). Polybrominated diphenyl ethers and polychlorinated biphenyls in different tissue types from Chinook salmon 
#(Oncorhynchus tshawytscha). Bulletin of environmental contamination and toxicology, 76(1), 148-154. They found
# ~ 1.5-2.3 ug/kg PBDEs (0.0015mg/kg to 0.0023 mg/kg) in Chinook Salmon from Oregon.

# fit model
pbde_model <- brm(mean_concentration_standardized ~ 0 + species + (1|authors) + (1|region),
                 family = Gamma(link = "log"),
                 data = pbde_data,
                 prior = c(prior(normal(-7, 2), class = "b"),
                           prior(exponential(3), class = "sd"),
                           prior(gamma(5, 1), class = "shape")),
                 file = "models/pbde_model.rds",
                 file_refit = "on_change",
                 cores = 4)

plot(conditional_effects(pbde_model), points = T)

#extract posteriors

pbde_posts <-  pbde_data %>% ungroup %>% 
  distinct(species, concentration_units_standardized) %>% 
  add_epred_draws(pbde_model, re_formula = NA) %>% 
  mutate(chemical = "PBDE")

saveRDS(pbde_posts, file = "posteriors/pbde_posts.RDS")

# DDT ----------------------------------------------------------------------

#data prep: check for duplicates

ddt_data <- nut_cont %>% 
  filter(grepl("DDT", chemical)) %>% 
  distinct() %>% 
  mutate(mean_concentration_standardized = case_when(tissue == "Fillet" ~ mean_concentration_standardized*1.66,
                                                     tissue == "Fillet+Skin" ~ mean_concentration_standardized*1.66,
                                                     TRUE ~ mean_concentration_standardized))

#check priors
#plot priors - only for a couple species, since all will be the same (same prior)
prior_b = rnorm(1000, -7, 2)
prior_sd = rexp(1000, 3)
prior_shape = rgamma(1000, 5, 1)

sim_gamma_priors(prior_b = prior_b,
                 prior_sd = prior_sd,
                 prior_shape = prior_shape) +
  scale_y_log10() +
  geom_hline(yintercept = 0.004)

# Consistent with ~0.004 mg/kg ww (4ppb) as in Anderson, R. B., & Everhart, W. H. (1966). Concentrations of DDT in landlocked salmon (Salmo salar) at Sebago Lake, Maine. Transactions of the American Fisheries Society, 95(2), 160-164.

# fit model
ddt_model <- brm(mean_concentration_standardized ~ 0 + species + (1|authors) + (1|region),
                  family = Gamma(link = "log"),
                  data = ddt_data,
                  prior = c(prior(normal(-7, 2), class = "b"),
                            prior(exponential(3), class = "sd"),
                            prior(gamma(5, 1), class = "shape")),
                 file = "models/ddt_model.rds",
                 file_refit = "on_change",
                 cores = 4)

plot(conditional_effects(ddt_model), points = T)

#extract posteriors

ddt_posts <- ddt_data %>% ungroup %>% 
  distinct(species, concentration_units_standardized) %>% 
  add_epred_draws(ddt_model, re_formula = NA) %>%
  mutate(chemical = "DDT")

saveRDS(ddt_posts, file = "posteriors/ddt_posts.RDS")
