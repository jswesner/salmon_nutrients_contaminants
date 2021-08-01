library(tidyverse)
library(brms)
library(readxl)
library(janitor)
library(ggthemes)
library(scales)
library(ggridges)

# model contaminant and nutrient concentrations in fish

#load contaminant data
nut_cont <- readRDS("data/nut_cont.rds")



# Functions ---------------------------------------------------------------

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



# Hg ----------------------------------------------------------------------

#data prep: check for duplicates
nut_cont %>% 
  filter(grepl("Hg", chemical)) %>% 
  group_by(specific_location, concentration, n, species, region, data_collection_period) %>% 
  filter(n>1)

hg_data <- nut_cont %>% 
  filter(grepl("Hg", chemical)) %>% 
  mutate(concentration = case_when(tissue == "Whole Body" ~ concentration,
                                   TRUE ~ concentration*0.7),
         original_concentration = case_when(tissue == "Whole Body" ~ original_concentration,
                                   TRUE ~ original_concentration*0.7),
         concentration = concentration*1000000,
         concentration_units = "ug/kg")

hg_data %>% 
  ggplot(aes(x = species, y = concentration, color = reorder(region, concentration))) +
  geom_point(position = position_dodge(width = 0.2)) + 
  scale_y_log10()

#check priors
#plot priors - only for a couple species, since all will be the same (same prior)
#NOTE - Model uses a no-intercept formula, so all groups get the same prior
prior_b = rnorm(1000, 4, 1)
prior_sd = rexp(1000, 2)
prior_shape = rgamma(1000, 5, 3)

sim_gamma_priors(prior_b = prior_b,
                 prior_sd = prior_sd,
                 prior_shape = prior_shape) +
  scale_y_log10()

# prior generates ranges from the literature of ~ 0 to 100 ug/kg, e.g. https://www.sciencedirect.com/science/article/pii/S0048969718343055 

# fit model
# hg_model <- brm(concentration ~ 0 + species + (1|authors) + (1|region),
#                 family = Gamma(link = "log"),
#                 data = hg_data,
#                 prior = c(prior(normal(4, 1), class = "b"),
#                           prior(exponential(1), class = "sd"),
#                           prior(gamma(5, 3), class = "shape")))
# 
# saveRDS(hg_model, file = "models/hg_model.rds")
hg_model <- readRDS("models/hg_model.rds")

plot(conditional_effects(hg_model), points = T)

#extract posteriors

hg_posts <- conditional_posts_fitted(hg_model, effects = "species") %>% 
  rename(ug_kg_ww = value) %>% mutate(chemical = "Hg")

saveRDS(hg_posts, file = "posteriors/hg_posts.rds")


# DHA  ----------------------------------------------------------------------

#data prep: check for duplicates
nut_cont %>% 
  filter(grepl("DHA", fa_class)) %>% 
  group_by(specific_location, concentration, n, species, region, data_collection_period) %>% 
  filter(n>1) 

dha_data <- nut_cont %>% 
  mutate(concentration = concentration/10) %>% 
  filter(grepl("DHA", fa_class)) %>% 
  distinct() 

dha_data %>% 
  ggplot(aes(x = species, y = concentration, color = reorder(region, concentration))) +
  geom_point(position = position_dodge(width = 0.2)) + 
  scale_y_log10()

#check priors
#Sprague, M., Dick, J. R., & Tocher, D. R. (2016). Impact of sustainable feeds on omega-3 long-chain fatty acid 
#levels in farmed Atlantic salmon, 2006–2015. Scientific reports, 6(1), 1-9.
#reported ~ 0.1 to 0.4 EPA + DHA g/kg ww. ! HALF IS DHA 

#plot priors - only for a couple species, since all will be the same (same prior)
#NOTE - Model uses a no-intercept formula, so all groups get the same prior
prior_b = rnorm(1000, -4, 2)
prior_sd = rexp(1000, 3)
prior_shape = rgamma(1000, 5, 3)

sim_gamma_priors(prior_b = prior_b,
                 prior_sd = prior_sd,
                 prior_shape = prior_shape) +
  scale_y_log10(labels = comma)

# prior generates ranges from the literature of ~ 0 to 100 ug/kg, e.g. https://www.sciencedirect.com/science/article/pii/S0048969718343055 

# fit model
dha_model <- brm(concentration ~ 0 + species + (1|authors) + (1|region),
                family = Gamma(link = "log"),
                data = dha_data,
                prior = c(prior(normal(-4, 2), class = "b"),
                          prior(exponential(3), class = "sd"),
                          prior(gamma(5, 3), class = "shape")))

saveRDS(dha_model, file = "models/dha_model.rds")
dha_model <- readRDS("models/dha_model.rds")

plot(conditional_effects(dha_model), points = T)

#extract posteriors

dha_posts <- conditional_posts_fitted(dha_model, effects = "species") %>% 
  rename(g_kg_ww = value) %>% mutate(chemical = "DHA")

saveRDS(dha_posts, file = "posteriors/dha_posts.rds")


# EPA  ----------------------------------------------------------------------

#data prep: check for duplicates
nut_cont %>% 
  filter(grepl("EPA", fa_class)) %>% 
  group_by(specific_location, concentration, n, species, region, data_collection_period) %>% 
  filter(n>1) 

epa_data <- nut_cont %>% 
  mutate(concentration = concentration/10) %>% 
  filter(grepl("EPA", fa_class)) %>% 
  distinct() 

epa_data %>% 
  ggplot(aes(x = species, y = concentration, color = reorder(region, concentration))) +
  geom_point(position = position_dodge(width = 0.2)) + 
  scale_y_log10()

#check priors
#Sprague, M., Dick, J. R., & Tocher, D. R. (2016). Impact of sustainable feeds on omega-3 long-chain fatty acid 
#levels in farmed Atlantic salmon, 2006–2015. Scientific reports, 6(1), 1-9.
#reported ~ 0.1 to 0.4 epa g/kg ww. ~ HALF IS EPA

#plot priors - only for a couple species, since all will be the same (same prior)
#NOTE - Model uses a no-intercept formula, so all groups get the same prior
prior_b = rnorm(1000, -4, 2)
prior_sd = rexp(1000, 3)
prior_shape = rgamma(1000, 5, 3)

sim_gamma_priors(prior_b = prior_b,
                 prior_sd = prior_sd,
                 prior_shape = prior_shape) +
  scale_y_log10(labels = comma)

# prior generates ranges from the literature of ~ 0 to 100 ug/kg, e.g. https://www.sciencedirect.com/science/article/pii/S0048969718343055 

# fit model
epa_model <- brm(concentration ~ 0 + species + (1|authors) + (1|region),
                 family = Gamma(link = "log"),
                 data = epa_data,
                 prior = c(prior(normal(-4, 2), class = "b"),
                           prior(exponential(3), class = "sd"),
                           prior(gamma(5, 3), class = "shape")))

saveRDS(epa_model, file = "models/epa_model.rds")
epa_model <- readRDS("models/epa_model.rds")

plot(conditional_effects(epa_model), points = T)

#extract posteriors

epa_posts <- conditional_posts_fitted(epa_model, effects = "species") %>% 
  rename(g_kg_ww = value) %>% mutate(chemical = "EPA")

saveRDS(epa_posts, file = "posteriors/epa_posts.rds")

# Nitrogen ----------------------------------------------------------------------
#****Assume values are for wet weight. Some derived from Ricker 1981, which reports 4-5 kg salmon size, which we assume is wet mass, but none state explicitly. 

#check for duplicates
nut_cont %>% 
  filter(grepl("%N", chemical)) %>% 
  group_by(specific_location, concentration, n, species, region, data_collection_period) %>% 
  filter(n() >1)

nit_data <- nut_cont %>% 
  filter(grepl("%N", chemical)) %>% 
  mutate(concentration = concentration*10) #convert g/100g to g/kg

#check priors
#plot priors - only for one species, since all will be the same (same prior)
prior_b = rnorm(1000, 2, 1)
prior_sd = rexp(1000, 2)
prior_shape = rgamma(1000, 5, 3)

sim_gamma_priors(prior_b = prior_b,
                 prior_sd = prior_sd,
                 prior_shape = prior_shape) +
  scale_y_log10() +
  annotation_logticks()



# plot above generates some values above 75 g/kg N, but mostly lower than 10. 

# fit model - no random effects b/c only 6 samples total
nit_model <- brm(concentration ~ 1,
                 family = Gamma(link = "log"),
                 data = nit_data,
                 prior = c(prior(normal(2, 1), class = "Intercept"),
                           prior(gamma(5, 3), class = "shape")))

saveRDS(nit_model, file = "models/nit_model.rds")

nit_model <- readRDS("models/nit_model.rds")

print(nit_model)

nit_posts <- posterior_samples(nit_model) %>% 
  mutate(g_kg_N = exp(b_Intercept),
         iter = 1:nrow(.)) %>% 
  expand_grid(species = unique(nit_data$species)) %>% mutate(chemical = "N")

saveRDS(nit_posts, file = "posteriors/nit_posts.rds")

# Phosphorous ----------------------------------------------------------------------

#check for duplicates
nut_cont %>% 
  filter(grepl("%P", chemical)) %>% 
  group_by(specific_location, concentration, n, species, region, data_collection_period) %>% 
  filter(n() >1)

phos_data <- nut_cont %>% 
  filter(grepl("%P", chemical)) %>% 
  mutate(concentration = concentration*10) #convert g/100g to g/kg

#check priors
#plot priors - only for a couple species, since all will be the same (same prior)

#" Salmon molar N:P ratios range from 12:1 to 15:1 (Gende 2002
# "Gende, S. M., Edwards, R. T., Willson, M. F., & Wipfli, M. S. (2002). Pacific salmon in aquatic and terrestrial ecosystems. BioScience, 52(10), 917-928."
# Given the above ratios, 2-5 g/kg of P is typical for salmon.

prior_b = rnorm(1000, 1, 0.25)
prior_sd = rexp(1000, 8)
prior_shape = rgamma(1000, 4, 2)

sim_gamma_priors(prior_b = prior_b,
                 prior_sd = prior_sd,
                 prior_shape = prior_shape)



# fit model
phos_model <- brm(concentration ~ 1,
                 family = Gamma(link = "log"),
                 data = phos_data,
                 prior = c(prior(normal(1, 0.25), class = "Intercept"),
                           prior(gamma(4, 2), class = "shape")))

print(phos_model)

saveRDS(phos_model, file = "models/phos_model.rds")
phos_model <- readRDS("models/phos_model.rds")


phos_posts <- posterior_samples(phos_model) %>% 
  mutate(g_kg_ww = exp(b_Intercept),
         iter = 1:nrow(.)) %>% 
  expand_grid(species = unique(phos_data$species)) %>% mutate(chemical = "P")

saveRDS(phos_posts, file = "posteriors/phos_posts.rds")



# PCB ----------------------------------------------------------------------

#data prep: check for duplicates
nut_cont %>% 
  filter(grepl("PCBs", chemical)) %>% 
  group_by(specific_location, concentration, n, species, region, data_collection_period) %>% 
  filter(n() >1) 

pcb_data <- nut_cont %>% 
  distinct(specific_location, authors, concentration, n, species, region, data_collection_period, tissue, concentration_units, chemical, original_concentration) %>% 
  filter(grepl("PCB", chemical)) %>% 
  filter(chemical != "PCB Cogener 153") %>% 
  mutate(concentration = case_when(tissue == "Whole Body" ~ concentration,
                                   TRUE ~ concentration*2.3),
         concentration = concentration*1e+6,
         concentration_units = "ug_kg",
         original_concentration = case_when(tissue == "Whole Body" ~ original_concentration,
                                            TRUE ~ original_concentration*2.3))
    
#check priors
#plot priors - only for a couple species, since all will be the same (same prior)
prior_b = rnorm(1000, 3, 3)
prior_sd = rexp(1000, 2)
prior_shape = rgamma(1000, 5, 2)

sim_gamma_priors(prior_b = prior_b,
                 prior_sd = prior_sd,
                 prior_shape = prior_shape) +
  scale_y_log10(labels = comma, breaks = c(0, 1, 10, 100, 1000, 10000)) +
  annotation_logticks()


#numbers above are consistent with Montory, M., Habit, E., Fernandez, P., Grimalt, J. O., & Barra, R. (2010). PCBs and PBDEs in wild Chinook salmon (Oncorhynchus tshawytscha) in the Northern Patagonia, Chile. Chemosphere, 78(10), 1193-1199.
# 25-78 ng/g from Montroy et al. 2010 abstract. *NOTE ng/g is the same as ug/kg

# fit model
# pcb_model <- brm(concentration ~ 0 + species + (1|authors) + (1|region),
#                  family = Gamma(link = "log"),
#                  data = pcb_data,
#                  prior = c(prior(normal(3, 3), class = "b"),
#                            prior(exponential(2), class = "sd"),
#                            prior(gamma(2, 2), class = "shape")),
#                  sample_prior = T)
# 
# 
# saveRDS(pcb_model, file = "models/pcb_model.rds")
pcb_model <- readRDS("models/pcb_model.rds")


plot(conditional_effects(pcb_model), points = T)

#extract posteriors
pcb_posts <- conditional_posts_fitted(pcb_model, effects = "species") %>%
  rename(ug_kg_ww = value) %>% mutate(chemical = "PCBs")

# #add individual mass data
# pcb_posts_perind <- pcb_posts %>% left_join(fish_mass) %>% 
#   mutate(ug_per_ind = ng_per_kg*mean_kg/1000)

saveRDS(pcb_posts, file = "posteriors/pcb_posts.RDS")


# PBDE ----------------------------------------------------------------------

#data prep: check for duplicates
nut_cont %>% 
  filter(grepl("PBD", chemical)) %>% 
  group_by(specific_location, concentration, n, species, region, data_collection_period) %>% 
  filter(n() >1) 

#correct for whole body, fillet with skin, and fillet without skin using Stone, D. (2006). Polybrominated diphenyl ethers and polychlorinated biphenyls in different tissue types from Chinook salmon 
#(Oncorhynchus tshawytscha). Bulletin of environmental contamination and toxicology, 76(1), 148-154.

pbde_data <- nut_cont %>% 
  distinct(specific_location, authors, concentration, n, species, region, data_collection_period, tissue, concentration_units, chemical, original_concentration) %>% 
  filter(grepl("PBD", chemical)) %>% 
  mutate(concentration = concentration*1e+6,
         concentration_units = "ug_kg",
         concentration = case_when(tissue == "Fillet" ~ concentration*1.5,
                                   tissue == "Fillet+Skin" ~ concentration*1.27,
                                   TRUE ~ concentration))

#check priors
#plot priors - only for a couple species, since all will be the same (same prior)
prior_b = rnorm(1000, 0, 1)
prior_sd = rexp(1000, 3)
prior_shape = rgamma(1000, 2, 1)

plot_prior_pbde <- sim_gamma_priors(prior_b = prior_b,
                 prior_sd = prior_sd,
                 prior_shape = prior_shape)

plot_prior_pbde +
  ylim(0, 20)

#numbers above are consistent with Stone, D. (2006). Polybrominated diphenyl ethers and polychlorinated biphenyls in different tissue types from Chinook salmon 
#(Oncorhynchus tshawytscha). Bulletin of environmental contamination and toxicology, 76(1), 148-154. They found
# ~ 1.5-2.3 ug/kg PBDEs in Chinook Salmon from Oregon.

# fit model
# pbde_model <- brm(concentration ~ 0 + species + (1|authors) + (1|region),
#                  family = Gamma(link = "log"),
#                  data = pbde_data,
#                  prior = c(prior(normal(0, 1), class = "b"),
#                            prior(exponential(3), class = "sd"),
#                            prior(gamma(2, 1), class = "shape")))
# 
# saveRDS(pbde_model, file = "models/pbde_model.rds")
pbde_model <- readRDS("models/pbde_model.rds")


plot(conditional_effects(pbde_model), points = T)



#extract posteriors

pbde_posts <- conditional_posts_fitted(pbde_model, effects = "species") %>%
  rename(ug_kg_ww = value) %>% mutate(chemical = "PBDE")

saveRDS(pbde_posts, file = "posteriors/pbde_posts.RDS")

# DDT ----------------------------------------------------------------------

#data prep: check for duplicates
nut_cont %>% 
  filter(grepl("DDT", chemical)) %>% 
  group_by(specific_location, concentration, n, species, region, data_collection_period) %>% 
  filter(n() >1) 

#correct for whole body, fillet with skin, and fillet without skin using Stone, D. (2006). Polybrominated diphenyl ethers and polychlorinated biphenyls in different tissue types from Chinook salmon 
#(Oncorhynchus tshawytscha). Bulletin of environmental contamination and toxicology, 76(1), 148-154.

ddt_data <- nut_cont %>% 
  distinct(specific_location, authors, concentration, n, species, region, data_collection_period, tissue, concentration_units, chemical, original_concentration) %>% 
  filter(grepl("DDT", chemical)) %>% 
  mutate(concentration = concentration*1e+6,
         concentration_units = "ug_kg")


ddt_data %>% 
  ggplot(aes(x = tissue, y = concentration)) + 
  geom_point()

#check priors
#plot priors - only for a couple species, since all will be the same (same prior)
prior_b = rnorm(1000, 0, 2)
prior_sd = rexp(1000, 3)
prior_shape = rgamma(1000, 2, 1)

plot_prior_ddt <- sim_gamma_priors(prior_b = prior_b,
                                    prior_sd = prior_sd,
                                    prior_shape = prior_shape)

plot_prior_ddt +
  ylim(0, 20)

#numbers above are consistent with Stone, D. (2006). Polybrominated diphenyl ethers and polychlorinated biphenyls in different tissue types from Chinook salmon 
#(Oncorhynchus tshawytscha). Bulletin of environmental contamination and toxicology, 76(1), 148-154. They found
# ~ 1.5-2.3 ug/kg ddts in Chinook Salmon from Oregon.

# fit model
ddt_model <- brm(concentration ~ 0 + species + (1|authors) + (1|region),
                  family = Gamma(link = "log"),
                  data = ddt_data,
                  prior = c(prior(normal(0, 2), class = "b"),
                            prior(exponential(3), class = "sd"),
                            prior(gamma(2, 1), class = "shape")))



saveRDS(ddt_model, file = "models/ddt_model.rds")
ddt_model <- readRDS("models/ddt_model.rds")


plot(conditional_effects(ddt_model), points = T)


#extract posteriors

ddt_posts <- conditional_posts_fitted(ddt_model, effects = "species") %>%
  rename(ug_kg_ww = value) %>% mutate(chemical = "DDT")

saveRDS(ddt_posts, file = "posteriors/ddt_posts.RDS")
