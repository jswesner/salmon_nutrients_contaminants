library(tidyverse)
library(brms)
library(tidybayes)
library(scales)
library(modelr)

#load data
nut_cont <- readRDS("data/nut_cont.rds")

source("code/functions.R")

#load original models

hg_model <- readRDS("models/hg_model.rds") 
pcb_model <- readRDS("models/pcb_model.rds")
nit_model <- readRDS("models/nit_model.rds") 
phos_model <- readRDS("models/phos_model.rds") 
epa_model <- readRDS("models/epa_model.rds")
dha_model <- readRDS("models/dha_model.rds")
pbde_model <- readRDS("models/pbde_model.rds")
ddt_model <- readRDS("models/ddt_model.rds")

#refit with sigma weighted by n


# Hg ----------------------------------------------------------------------

hg_data <- nut_cont %>% 
  filter(grepl("Hg", chemical)) %>% 
  mutate(mean_concentration_standardized = case_when(tissue == "Whole Body" ~ mean_concentration_standardized,
                                                     TRUE ~ mean_concentration_standardized*0.74),
         n1 = 1/n)

hg_model_weighted <- brm(bf(log(mean_concentration_standardized) ~ 0 + species + (1|authors) +
                      (1|region),
                    sigma~1-offset(log(sqrt(n)))),
                 data = hg_data,
                 prior = c(prior(normal(-5.5, 2), class = "b"),
                           prior(exponential(2), class = "sd")),
                 cores = 4,
                 file = "models/hg_model_weighted.rds",
                 file_refit = "on_change")


hg_original = conditional_effects(hg_model)
hg_weight = conditional_effects(hg_model_weighted)

hg_original$species %>% 
  ggplot(aes(x = species, y = estimate__)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__)) +
  ylim(0, 0.1)

hg_weight$species %>% 
  ggplot(aes(x = species, y = exp(estimate__))) +
  geom_pointrange(aes(ymin = exp(lower__), ymax = exp(upper__))) +
  ylim(0, 0.1)

# DHA ---------------------------------------------------------------------

dha_data <- nut_cont %>% 
  filter(grepl("DHA", chemical)) %>% 
  distinct() 

dha_model_weighted <-update(hg_model_weighted,
                 newdata = dha_data,
                 prior = c(prior(normal(8.5, 1), class = "b"),
                           prior(exponential(2), class = "sd")),
                 file = "models/dha_model_weighted.rds",
                 file_refit = "on_change",
                 cores = 4)

conditional_effects(dha_model)
conditional_effects(dha_model_weighted)



# EPA ---------------------------------------------------------------------

epa_data <- nut_cont %>% 
  filter(grepl("EPA", chemical)) %>% 
  distinct() 

epa_model_weighted = update(dha_model_weighted,
                            newdata = epa_data,
                            prior = c(prior(normal(8.5,1), class = "b"),
                                      prior(exponential(3), class = "sd")),
                            file = "models/epa_model_weighted.rds",
                            file_refit = "on_change",
                            cores = 4)

conditional_effects(epa_model_weighted)


# N -----------------------------------------------------------------------

nit_data <- nut_cont %>% 
  filter(chemical == "N") %>% 
  filter(species != "All") %>%
  mutate(region = case_when(is.na(region) ~ "All",
                            TRUE ~ region)) %>% 
  mutate(mean_concentration_standardized = case_when(tissue == "muscle" ~ 0.89*mean_concentration_standardized,
                                                     TRUE ~ mean_concentration_standardized))

nit_model_weighted = update(dha_model_weighted,
                            newdata = nit_data,
                            prior = c(prior(normal(10,1), class = "b"),
                                      prior(exponential(0.5), class = "sd")),
                            file = "models/nit_model_weighted.rds",
                            file_refit = "on_change",
                            cores = 4)

conditional_effects(nit_model_weighted)
conditional_effects(nit_model)

# P -----------------------------------------------------------------------

phos_data <- nut_cont %>% 
  filter(chemical == "P") %>% 
  filter(species != "All") %>% 
  distinct() %>% 
  mutate(mean_concentration_standardized = case_when(tissue == "muscle" ~ 1.32*mean_concentration_standardized,
                                                     TRUE ~ mean_concentration_standardized))

# fit model
phos_model_weighted <- brm(bf(log(mean_concentration_standardized) ~ 1 + species + (1|authors) + (1|region),
                  sigma~1-offset(log(sqrt(n)))),
                  data = phos_data,
                  prior = c(prior(normal(8, 0.5), class = "Intercept"),
                            prior(normal(0, 0.5), class = "b")),
                  file = "models/phos_model_weighted.rds",
                  file_refit = "on_change",
                  cores = 4)

conditional_effects(phos_model_weighted)
conditional_effects(phos_model)


# PCB ---------------------------------------------------------------------

pcb_data <- nut_cont %>% 
  filter(chemical == "PCBs") %>% 
  mutate(mean_concentration_standardized = case_when(tissue == "Whole Body" ~ mean_concentration_standardized,
                                                     TRUE ~ mean_concentration_standardized*2.3)) %>% 
  distinct()

pcb_model_weighted <- brm(bf(log(mean_concentration_standardized) ~ 0 + species + (1|authors) + (1|region),
                             sigma~1-offset(log(sqrt(n)))),
                 # family = Gamma(link = "log"),
                 data = pcb_data,
                 prior = c(prior(normal(-5.5, 1), class = "b"),
                           prior(exponential(2), class = "sd")),
                 # sample_prior = T,
                 file = "models/pcb_model_weighted.rds",
                 file_refit = "on_change",
                 cores = 4)



# PBDE --------------------------------------------------------------------


pbde_data <- nut_cont %>%
  filter(grepl("PBD", chemical)) %>% 
  mutate(mean_concentration_standardized = case_when(tissue == "Fillet" ~ mean_concentration_standardized*1.5,
                                                     tissue == "Fillet+Skin" ~ mean_concentration_standardized*1.27,
                                                     TRUE ~ mean_concentration_standardized))

pbde_model_weighted <- brm(bf(log(mean_concentration_standardized) ~ 0 + species + (1|authors) + (1|region),
                     sigma~1-offset(log(sqrt(n)))),
                  # family = Gamma(link = "log"),
                  data = pbde_data,
                  prior = c(prior(normal(-7, 2), class = "b"),
                            prior(exponential(3), class = "sd")),
                  file = "models/pbde_model_weighted.rds",
                  file_refit = "on_change",
                  cores = 4)

# DDT ----------------------------------------------------------------------

#data prep: check for duplicates

ddt_data <- nut_cont %>% 
  filter(grepl("DDT", chemical)) %>% 
  distinct() %>% 
  mutate(mean_concentration_standardized = case_when(tissue == "Fillet" ~ mean_concentration_standardized*1.66,
                                                     tissue == "Fillet+Skin" ~ mean_concentration_standardized*1.66,
                                                     TRUE ~ mean_concentration_standardized))

ddt_model_weighted <- brm(bf(log(mean_concentration_standardized) ~ 0 + species + (1|authors) + (1|region),
                    sigma~1-offset(log(sqrt(n)))),
                    # family = Gamma(link = "log"),
                 data = ddt_data,
                 prior = c(prior(normal(-7, 2), class = "b"),
                           prior(exponential(3), class = "sd")),
                 file = "models/ddt_model_weighted.rds",
                 file_refit = "on_change",
                 cores = 4)




# get posts ---------------------------------------------------------------

mod_list = list(ddt_model_weighted,
                pbde_model_weighted,
                pcb_model_weighted,
                hg_model_weighted,
                nit_model_weighted,
                phos_model_weighted,
                dha_model_weighted,
                epa_model_weighted)

cont_list = c("ddt", "pbde", "pcb", "hg", "nit", "phos", "dha", "epa")

posts_list = NULL

for(i in 1:length(mod_list)){
  posts_list[[i]] = mod_list[[i]]$data %>% 
    distinct(species) %>% 
    mutate(n = 1) %>% 
    add_epred_draws(mod_list[[i]], re_formula = NA) %>% 
    mutate(conc = exp(.epred),
           chemical = cont_list[i],
           model = "weighted")
  }


bind_rows(posts_list) %>% 
  ggplot(aes(x = species, y = conc)) + 
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~chemical, scales = "free") +
  scale_y_log10() +
  NULL

mod_original = list(ddt_model,
                pbde_model,
                pcb_model,
                hg_model,
                nit_model,
                phos_model,
                dha_model,
                epa_model)

posts_original_list = NULL

for(i in 1:length(mod_original)){
  posts_original_list[[i]] = mod_list[[i]]$data %>% 
    distinct(species) %>% 
    mutate(n = 1) %>% 
    add_epred_draws(mod_original[[i]], re_formula = NA) %>% 
    mutate(chemical = cont_list[i],
           model = "original")
}


fig_s10_data = bind_rows(posts_original_list) %>% 
  group_by(species, chemical) %>% 
  reframe(median_original = median(.epred),
          lower_original = quantile(.epred, probs = 0.025),
          upper_original = quantile(.epred, probs = 0.975)) %>% 
  left_join(bind_rows(posts_list) %>% 
              group_by(species, chemical) %>% 
              reframe(median_weighted = median(conc),
                      lower_weighted = quantile(conc, probs = 0.025),
                      upper_weighted = quantile(conc, probs = 0.975))) %>%
  mutate(panel = case_when(chemical == "nit" ~ "N",
                           chemical == "phos" ~ "P",
                           chemical == "dha" ~ "DHA",
                           chemical == "epa" ~ "EPA",
                           chemical == "hg" ~ "Hg",
                           chemical == "pcb" ~ "PCBs",
                           chemical == "ddt" ~ "DDTs",
                           chemical == "pbde" ~ "PBDEs")) %>% 
  mutate(panel = as.factor(panel),
         panel = fct_relevel(panel, "N", "P", "DHA", "EPA", "Hg", "PCBs", "DDTs"))


write_csv(fig_s10_data, file = "plots/fig_s10_data.csv")

theme_salmon = function () { 
  theme_default(base_family = "sans") +
    theme(axis.text=element_text(color="black", size = 5),
          axis.ticks = element_line(color="black"),
          text = element_text(size = 7),
          axis.title = element_text(size = 7),
          legend.text = element_text(size = 6),
          legend.key.size = unit(0.4, "line"))
}

theme_set(theme_salmon())


