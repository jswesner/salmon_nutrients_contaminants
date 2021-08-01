library(sensobol)
library(tidyverse)

# Define settings
N <- 1000; params <- paste("X", 1:2, sep = ""); R <- 10

# Create sample matrix
mat <- sobol_matrices(N = 1000, params = params)
matrix(c(ddt_test$year, ddt_test$species))
mat_test <- matrix(ddt_test)

Y = mat


# Compute Ishigami function
Y <- mean(mat[,1]*mat[,2])

# Compute and bootstrap Sobol' indices
ind <- sobol_indices(Y = Y, N = N, params = params, boot = TRUE, R = R)




# try with my data
n_iter = 300
flux_test <- gam_salmon_posts %>% filter(iter <= n_iter) %>% 
  group_by(location, species, year) %>% 
  right_join(region_concentrations_withchum %>%  filter(iter <= n_iter)) %>% 
  mutate(g_flux = region_conc*kg) 

ddt_test <- flux_test %>% 
  filter(chemical == "DDT") %>%
  distinct(iter, kg, region_conc, g_flux) %>% 
  ungroup() %>% 
  mutate(region_conc_s = (region_conc - mean(region_conc)/sd(region_conc)),
         kg_s = (kg - mean(kg)/sd(kg)),
         g_flux_s = (g_flux - mean(g_flux)/sd(g_flux)))




Y <- ddt_test$kg*ddt_test$region_conc

test <- sobol_indices(Y = ddt_test$g_flux, N = 2000,
                      params = c("kg", "region_conc"))
as_tibble(test$results)


flux_test %>%
  filter(chemical == "DDT") %>% 
  ggplot(aes(x = year, y = g_flux, color = species)) +
  geom_point(size = 0.1) +
  # geom_smooth(method = "lm") +
  facet_wrap(~location)

ddt_test %>% 
  unite("sp_l_y", species:year) %>% 
  select(-iter) %>% 
  mutate(id = 1:nrow(.)) %>% 
  pivot_longer(cols = c(-sp_l_y, -id)) %>% 
  pivot_wider(names_from = sp_l_y, values_from = value)


flux_test %>% 
  filter(chemical == "DDT") %>%
  distinct(iter, kg, region_conc, g_flux) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(kg, region_conc, g_flux)) %>% 
  pivot_wider(names_from = c(species,location, year), values_from = value)


test <- summary(lm(g_flux_s ~ kg_s + region_conc_s + year + location + species, data = ddt_test))
summary(lm(g_flux_s ~ kg_s + region_conc_s, data = ddt_test))

test

