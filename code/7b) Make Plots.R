library(tidyverse)
library(ggthemes)
library(ggrepel)
library(cowplot)
library(scales)
library(janitor)
library(ggridges)
library(viridis)
library(patchwork)
library(brms)
library(tidybayes)
library(rnaturalearth)
library(maps)
library(ggh4x)
library(ggview)
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

# This code loads the the raw data of the figures and recreates the plots

# Figure 1 ----------------------------------------------------------------
# 1) load data
fig1b_colors = readRDS(file = "plots/fig1b_colors.rds") %>% 
  mutate(species = case_when(species == "All" ~ "Total",
                             TRUE ~ species)) %>% 
  mutate(species = as.factor(species),
         species = fct_relevel(species, "Total", "Pink", "Sockeye", "Chum", "Chinook", "Coho"))

fig1b_data = read_csv(file = "plots/fig1b_data.csv") %>% 
  mutate(panel_new = str_trim(as.factor(panel_new)), 
         panel_new = fct_relevel(panel_new, "Total returns", "Total nutrients",
                                 "Total contaminants", "Species returns", "Species nutrients")) %>% 
  mutate(species = as.factor(species),
         species = fct_relevel(species, "Total", "Pink", "Sockeye", "Chum", "Chinook", "Coho")) %>% 
  separate(panel_new, into = c("group", "measure"), remove = F) %>% 
  mutate(group = as.factor(group),
         group = fct_relevel(group, "Total"),
         measure = as.factor(str_to_sentence(measure)),
         measure = fct_relevel(measure, "Returns", "Nutrients"))

fig1c_data = read_csv(file = "plots/fig_1c_data.csv") %>% clean_names() %>% 
  mutate(species = case_when(species == "Alewife_contemporary" ~ "Alewife", 
                             species == "Pacific salmon" ~ "Pacific salmon\n(this study)",
                             TRUE ~ species))

# load map data
world <- map_data("world") 
states <- map_data("state")
usa <- ne_countries(scale='medium', returnclass = 'sf') 


# 2) make fig1a

fig1a = usa %>% 
  filter(sovereignt == "United States of America") %>% 
  ggplot() + 
  # coord_sf() + 
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey70", color = "black") +
  geom_sf(color = "black", fill = "grey70") +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), color = "black", fill = "grey70")  +
  # theme_void() +
  coord_sf(ylim = c(40, 78), xlim = c(-190, -125)) +
  labs(subtitle = "b)") +
  # geom_curve(x = -180, xend = -155, y = 50, yend = 65, curvature = -0.4,
  #            arrow = arrow(length = unit(0.03, "npc"), type="closed"),
  #            linewidth = 1) +
  theme_void() +
  theme(axis.ticks = element_line(color="black"),
        text = element_text(size = 7, family = "sans")) +
  NULL


# 2) make plot
# make left column
top_1b = fig1b_data %>%
  filter(grepl("return", panel_new)) %>% 
  ggplot(aes(x = year, y = median, fill = species))  + 
  geom_ribbon(aes(ymin = low75, ymax = high75)) + 
  geom_line(aes(group = species), color = "black", linewidth = 0.1) +
  # facet_grid2(group ~ measure, scales = "free_y", axes = "all", remove_labels = "x") +
  facet_wrap(~panel_new, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  scale_fill_manual(values = c("grey50", "#251256FF", "#5F187FFF", "#972C80FF",
                               "#CF406FFF", "#F76F5CFF"),
                    labels = c("Total", "Pink", "Sockeye",
                               "Chum", "Chinook", 
                               "Coho")) +
  labs(y = expression("MT y"^-1),
       x = "Year",
       fill = "Species",
       subtitle = "a)") +
  theme_salmon() +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2),
        axis.title.x = element_blank(),
        panel.spacing = unit(0.2, "lines")) + 
  guides(fill = "none") +
  NULL

mid_1b = fig1b_data %>%
  filter(grepl("nutrients", panel_new)) %>% 
  ggplot(aes(x = year, y = median, fill = species))  + 
  geom_ribbon(aes(ymin = low75, ymax = high75)) + 
  geom_line(aes(group = species), color = "black", linewidth = 0.1) +
  # facet_grid2(group ~ measure, scales = "free_y", axes = "all", remove_labels = "x") +
  facet_wrap(~panel_new, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  scale_fill_manual(values = c("grey50", "#251256FF", "#5F187FFF", "#972C80FF",
                               "#CF406FFF", "#F76F5CFF"),
                    labels = c("Total", "Pink", "Sockeye",
                               "Chum", "Chinook", 
                               "Coho")) +
  labs(y = expression("MT y"^-1),
       x = "Year",
       fill = "Species") +
  theme_salmon() +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2),
        text = element_text(family = "sans"),
        axis.title.x = element_blank(),
        panel.spacing = unit(0.2, "lines")) + 
  guides(fill = "none") +
  NULL

bottom_1b = fig1b_data %>%
  filter(grepl("contaminants", panel_new)) %>% 
  ggplot(aes(x = year, y = median*1000, fill = species))  + 
  geom_ribbon(aes(ymin = low75*1000, ymax = high75*1000)) + 
  geom_line(aes(group = species), color = "black", linewidth = 0.1) +
  # facet_grid2(group ~ measure, scales = "free_y", axes = "all", remove_labels = "x") +
  facet_wrap(~panel_new, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  scale_fill_manual(values = c("grey50", "#251256FF", "#5F187FFF", "#972C80FF",
                               "#CF406FFF", "#F76F5CFF"),
                    labels = c("Total", "Pink", "Sockeye",
                               "Chum", "Chinook", 
                               "Coho")) +
  labs(y = expression("Kg y"^-1),
       # x = "Year",
       fill = "Species") +
  theme_salmon() +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2),
        text = element_text(family = "sans"),
        # axis.title.x = element_blank(),
        panel.spacing = unit(0.2, "lines"),
        axis.title.x = element_blank()
  ) + 
  guides(fill = "none") +
  NULL

temp_1b = fig1b_colors %>% 
  ggplot(aes(x = species, y = c(1, 2, 3, 4, 5, 6), fill = species)) + 
  geom_col() +
  scale_fill_manual(values = c("grey50", "#251256FF", "#5F187FFF", "#972C80FF",
                               "#CF406FFF", "#F76F5CFF"),
                    labels = c("Total", "Pink", "Sockeye",
                               "Chum", "Chinook", 
                               "Coho")) +
  # guides(fill = guide_legend(nrow = 1)) +
  labs(fill = "                    ") +
  theme_salmon() +
  theme(legend.position = "top",
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.4, "line"))

legend_1b = get_plot_component(temp_1b, 'guide-box-top', return_all = TRUE)


# make fig c
fig1c = ggplot(data = fig1c_data, aes(x = n_flux_annualkg, 
                                      y = p_flux_annualkg, 
                                      shape=mechanism, 
                                      label=species)) +
  # geom_text_repel(max.overlaps = Inf, size = 1.8) +
  # geom_text(size = 1.8, aes(x = n_flux_annualkg + 10*n_flux_annualkg)) +
  geom_point(color="blue")  +
  scale_shape_manual(values=c(1,4,5,19,17)) +
  scale_x_log10(labels=comma, limits = c(NA, 1e8)) +
  scale_y_log10(breaks=c(1,10,100,1000,10000,100000,1000000,10000000), labels=comma,
                limits = c(NA, 1e7)) +
  labs(y = expression("P flux in kg y"^-1),
       x = expression("N flux in kg y"^-1),
       shape = "Mechanism",
       subtitle = "c)") +
  theme_salmon() +
  theme(# legend.background = element_rect(fill="white",
    #                                  size=0.3, linetype="solid",
    #                                  colour ="black"),
    legend.position = c(0.2, 0.87),
    legend.text = element_text(size = 6),
    panel.grid = element_blank(),
    legend.key.size = unit(0.25, "line")) 

a_1b = plot_grid(top_1b, mid_1b, bottom_1b, ncol = 1, align = "v")
fig1b = plot_grid(a_1b, legend_1b, ncol = 1, rel_heights = c(0.95, 0.05))
fig1bc = plot_grid(fig1b, fig1c, ncol = 2, align = "v", rel_widths = c(0.4, 0.6))

ab = plot_grid(fig1a, fig1c, ncol = 1)
fig1abc = plot_grid(fig1b,ab, ncol = 2, align = "v")

ggview::ggview(fig1abc, width = 6.5, height = 5)

ggsave(fig1abc, width = 6.5, height = 5, 
       file = "plots/fig1_plot.jpg", dpi = 600)

saveRDS(fig1abc, file = "plots/fig1_plot.rds")

ggsave(fig1abc, width = 6.5, height = 5, 
       file = "plots/fig1_plot.pdf", dpi = 600)


# Figure 2 ----------------------------------------------------------------
# 1) load data
fig2_data = read_csv(file = "plots/fig2_data.csv") %>% 
  mutate(species = as.factor(species),
         species = fct_relevel(species, "Coho", "Chinook", "Chum", "Sockeye", "Pink")) %>% 
  mutate(chemical = as.factor(chemical),
         chemical = fct_relevel(chemical, "N", "P", "DHA", "EPA", "Hg", "PCBs", "DDTs", "PBDEs"))

# 2) make plot
fig2a_d = fig2_data %>%
  filter(panel_letter %in% letters[1:4]) %>% 
  ggplot(aes(x = year, y = median)) +
  geom_linerange(aes(ymin = low75, ymax = high75, color = species), alpha = 0.5,
                 linewidth = .2) +
  geom_point(size = 0.2, aes(color = species)) +
  scale_color_viridis_d(direction = 1, option = 'A', alpha = 1,
                        begin = 0.7, end = 0.15) +
  scale_fill_viridis_d(direction = 1, option = 'A', alpha = 1,
                       begin = 0.7, end = 0.15) +
  guides(color = "none",
         fill = "none") +
  labs(y =  expression("Total transport (kg y"^-1*")"),
       subtitle = "Nutrients") +
  facet_wrap(~ chemical, ncol = 4) +
  scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  theme_salmon() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2)) +
  NULL

fig2e_h =  fig2_data %>%
  filter(panel_letter %in% letters[5:8]) %>% 
  ggplot(aes(x = year, y = median)) + 
  geom_area(position = "fill", aes(fill = species)) +
  facet_wrap( ~ chemical, nrow = 1) +
  scale_fill_viridis_d(direction = -1, option = 'A', alpha = 1,
                       begin = 0.15, end = 0.7) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  coord_cartesian(ylim = c(0, 1.1)) +
  labs(y = "Proportional \ntransport",
       fill = "Species",
       x = "Year") +
  theme_salmon() +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2),
        axis.title.x = element_blank())

fig2i_l = fig2_data %>%
  filter(panel_letter %in% letters[9:12]) %>% 
  ggplot(aes(x = year, y = median)) +
  geom_linerange(aes(ymin = low75, ymax = high75, color = species), alpha = 0.5,
                 size = .2) +
  geom_point(size = 0.2, aes(color = species)) +
  scale_color_viridis_d(direction = 1, option = 'A', alpha = 1,
                        begin = 0.7, end = 0.15) +
  scale_fill_viridis_d(direction = 1, option = 'A', alpha = 1,
                       begin = 0.7, end = 0.15) +
  labs(y =  expression("Total transport (kg y"^-1*")"),
       subtitle = "Contaminants") +
  facet_wrap(~ chemical, ncol = 4) +
  scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  theme_salmon() +
  theme(axis.title.x = element_blank(),
        strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2)) +
  guides(color = "none",
         fill = "none") +
  NULL

fig2m_p = fig2_data %>%
  filter(panel_letter %in% letters[9:12]) %>% 
  ggplot(aes(x = year, y = median)) + 
  geom_area(position = "fill", aes(fill = species)) +
  facet_wrap( ~ chemical, nrow = 1) +
  scale_fill_viridis_d(direction = -1, option = 'A', alpha = 1,
                       begin = 0.15, end = 0.7) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  coord_cartesian(ylim = c(0, 1.1)) +
  labs(y = "Proportional \ntransport",
       fill = "Species",
       x = "Year") +
  theme_salmon() +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2),
        axis.title.x = element_blank())

fig2_legend = get_legend(fig2e_h)

# combine plots with patchwork
fig2_panels = (fig2a_d / (fig2e_h + guides(fill = "none")) / fig2i_l / fig2m_p + guides(fill = "none"))  

fig2_plot = plot_grid(fig2_panels, fig2_legend, rel_widths = c(1, 0.2))

saveRDS(fig2_plot, file = "plots/fig2_plot.rds")
ggview::ggview(fig2_plot, width = 6.5, height = 7)
ggsave(fig2_plot, file = "plots/fig2_plot.pdf", dpi = 600, width = 6.5, height = 7)
ggsave(fig2_plot, file = "plots/fig2_plot.jpg", dpi = 600, width = 6.5, height = 7)


# Figure 3 ----------------------------------------------------------------
# 1) load data
fig3_data = read_csv(file = "plots/fig3_data.csv") %>% 
  mutate(location = as.factor(location),
         location = fct_relevel(location, "BCWC", "SEAK", "CAK")) %>% 
  mutate(panel_old = panel) %>% 
  separate(panel, into = c("order", "panel")) %>% 
  mutate(panel = fct_relevel(panel, "N", "P", "DHA", "EPA", "Hg", "PCBs", "DDTs", "PBDEs"))

# 2) make plot
fig3_plot_a_d = fig3_data %>% 
  filter(type == "Nutrients") %>% 
  ggplot(aes(x = year, y = median/1000)) +
  # geom_linerange(aes(ymin = low75, ymax = high75), alpha = 1,
  #                size = .5) +
  geom_ribbon(aes(ymin = low50/1000, ymax = high50/1000, fill= location), alpha = 0.25) +
  geom_line(aes(color= location), linewidth = 0.5) +
  # scale_color_colorblind() +
  # scale_color_brewer(type = "qual", palette = 3) +
  scale_color_viridis_d(direction = 1, option = 'E') +
  scale_fill_viridis_d(direction = 1, option = 'E') +
  # guides(color = "none",
  #        fill = guide_legend(override.aes = list(size = 7))) +
  labs(y = expression("Total transport (MT y"^-1*")"),
       subtitle = "Nutrients",
       color = "Region",
       fill = "Region") +
  facet_wrap(~ panel, ncol = 4) +
  scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  theme_salmon() +
  theme(axis.title.x = element_blank(),
        strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2),
        text = element_text(family = "sans")) +
  NULL

# make second row of panels
fig3_plot_e_h = fig3_data %>% 
  filter(type != "Nutrients") %>% 
  ggplot(aes(x = year, y = median)) +
  geom_ribbon(aes(ymin = low50, ymax = high50, fill= location), alpha = 0.25) +
  geom_line(aes(color= location), linewidth = 0.6) +
  scale_color_viridis_d(direction = 1, option = "E") +
  scale_fill_viridis_d(direction = 1, option = "E") +
  # scale_color_brewer(type = "qual", palette = 3) +
  # guides(fill = guide_legend(override.aes = list(size = 7))) +
  labs(y = expression("Total transport (MT y"^-1*")"),
       subtitle = "Contaminants",
       color = "Region",
       fill = "Region") +
  facet_wrap(~ panel, ncol = 4) +
  scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  theme(axis.title.x = element_blank(),
        strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2)) +
  NULL

# combine panels using patchwork package
fig3_plot_temp = (fig3_plot_a_d + guides(color = "none", fill = "none"))/
  (fig3_plot_e_h + guides(color = "none", fill = "none"))

fig3_legend = get_legend(fig3_plot_a_d)

fig3_plot = plot_grid(fig3_plot_temp, fig3_legend, ncol = 2, rel_widths = c(0.9, 0.1))


# final plot
saveRDS(fig3_plot, file = "plots/fig3_plot.rds")
ggview(fig3_plot, width = 6.5, height = 3.5)
ggsave(fig3_plot, file = "plots/fig3_plot.jpg", dpi = 600, width = 8, height = 4.5)
ggsave(fig3_plot, file = "plots/fig3_plot.pdf", dpi = 600, width = 8, height = 4.5)


# Figure 4 ----------------------------------------------------------------
# 1) load data
fig4_data = read_csv(file = "plots/fig4_data.csv")

# 2) make plot
fig4_a = fig4_data %>% 
  filter(panel == "a)") %>% 
  ggplot(aes(x = ratio_kgmgperfish, y = reorder(species_tl,tl))) +
  geom_density_ridges(scale = 1) +
  geom_boxplot(outlier.shape = NA, width = 0.1, size = 0.25) +
  scale_color_viridis(option = "E",
                      direction = -1) +
  labs(color = "Year",
       x = expression("Ratio of nutrients (kg fish"^-1*") to contaminants (mg fish"^-1*")"),
       subtitle = "a)") +
  annotate("text", label = "More nutrients per contaminant", x = 2.3, y = 0.74,
           size = 2) +
  annotate("segment", x = 2.5, y = 0.6, xend = 4, yend = 0.6,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  theme_salmon() +
  theme(axis.title.y = element_blank()) +
  NULL

# get species means for Fig 4b
fig4_means = fig4_data %>% 
  filter(panel == "b)") %>% 
  group_by(species, .draw, species_tl, tl) %>% 
  reframe(mean_prop = mean(cont_minus_nut))

fig4_b = fig4_data  %>% 
  ggplot(aes(x = cont_minus_nut, y = reorder(species_tl,tl))) +
  geom_density_ridges(fill = NA, scale = 0.9,
                      aes(group = interaction(species, year),
                          color = year)) +
  geom_boxplot(data = fig4_means, aes(group = species, x = mean_prop), outlier.shape = NA,
               width = 0.1,
               size = 0.25) +
  scale_color_viridis(option = "E",
                      direction = -1, 
                      breaks = c(1976, 1985, 1995, 2005, 2015)) +
  geom_vline(xintercept = 0, linewidth = 0.2) + 
  labs(y = "", 
       color = "Year",
       x = "Relative contribution to continental biotransport",
       subtitle = "b)") +
  annotate("text", label = "Relatively more contaminants", x = -0.14, y = 0.6,
           size = 2.0) +
  annotate("text", label = "Relatively more nutrients", x = 0.14, y = 0.6,
           size = 2.0) +
  theme_salmon() +
  theme(legend.background = element_rect(fill = "white"),
        legend.position = c(0.8, 0.8),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.3, "cm")) +
  NULL

# combine plots with patchwork
fig4_plot = fig4_a + fig4_b

ggview::ggview(fig4_plot, width = 6.5, height = 3.5)

saveRDS(fig4_plot, file = "plots/fig4_plot.rds")
ggsave(fig4_plot, file = "plots/fig4_plot.jpg", dpi = 600, width = 6.5, height = 3.5)
ggsave(fig4_plot, file = "plots/fig4_plot.pdf", dpi = 600, width = 6.5, height = 3.5)


# Figure 5 ----------------------------------------------------------------

# 1) load data
fig5a_data = read_csv(file = "plots/fig5a_data.csv") %>% filter(!is.na(value)) %>% 
  mutate(group = rep(1:15, each = 3)) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  clean_names() %>% 
  pivot_longer(cols = c(total_hg_accumulation_mg_hg_per_bird, 
                        whole_body_hg_concentration_mg_kg_ww))

fig5b_data = read_csv(file = "plots/fig5_data.csv")


# 2) make plots
a5 = fig5a_data %>% 
  filter(name == "total_hg_accumulation_mg_hg_per_bird") %>% 
  ggplot(aes(x = -accumulation_proportion_of_hg_inputs, y = value)) + 
  geom_point(size = 1) +
  scale_y_log10() +
  geom_line(aes(group = name), linetype = "dashed") +
  labs(x = "",
       y = "Total Hg accumulation\n(mg Hg per bird)",
       subtitle = "a)") +
  theme(axis.title.x = element_blank()) 

b5 = fig5a_data %>% 
  filter(name != "total_hg_accumulation_mg_hg_per_bird") %>% 
  ggplot(aes(x = -accumulation_proportion_of_hg_inputs, y = value)) + 
  geom_point(size = 1) +
  scale_y_log10(breaks = c(0.1, 0.3, 1, 3, 10, 30), limits = c(0.1, 30)) +
  scale_x_continuous(labels = c("1", "0.75", "0.5", "0.25", "0")) +
  geom_line(aes(group = name), linetype = "dashed") +
  labs(x = "Accumulated proportion of annual Hg inputs",
       y = "Whole body Hg\n(mg kg\u207B\u00B9 ww)") 


fig5a = plot_grid(a5, b5, ncol = 1, align = "v")


fig5b_plot = fig5b_data %>%
  ggplot(aes(x = risk_quotient, y = reorder(species_tl,tl))) +
  geom_density_ridges(scale = 1, aes(fill = species)) +
  geom_boxplot(outlier.shape = NA, width = 0.1, size = 1, linewidth = 0.2) +
  scale_color_viridis(option = "E",
                      direction = -1) +
  labs(x = "Risk-Benefit Quotient",
       subtitle = "b)") +
  annotate("text", label = "More risk to consumers", x = 0.4, y = 0.8,
           size = 2) +
  annotate("segment", x = 0.1, y = 0.6, xend = 0.5, yend = 0.6,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  coord_cartesian(xlim = c(NA, 0.8)) +
  theme(axis.title.y = element_blank()) +
  guides(fill = "none") +
  scale_fill_manual(values = c("#CF406FFF", "#972C80FF", "#F76F5CFF",   
                               "#251256FF", "#5F187FFF"
                                
                               )) +
  NULL

fig5_plot = plot_grid(fig5a, fig5b_plot, ncol = 2, align = "v", rel_widths = c(0.5, 0.5))

ggview::ggview(fig5_plot, width = 6.5, height = 3.5)
saveRDS(fig5_plot, file = "plots/fig5_plot.rds")
ggsave(fig5_plot, file = "plots/fig5_plot.jpg", width = 6.5, height = 3.5, units = "in", dpi = 500 )
ggsave(fig5_plot, file = "plots/fig5_plot.pdf", width = 6.5, height = 3.5, units = "in", dpi = 500 )


# Figure ED1 --------------------------------------------------------------

fig_ed1_data = readRDS("plots/fig_ed1_data.rds")

fig_ed1a = fig_ed1_data[[1]] %>% 
  filter(type == "lines") %>% 
  ggplot(aes(x = year, y = metric_tons)) + 
  # geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.4) +
  # geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.8) +
  # geom_line(aes(y = med)) +
  stat_lineribbon(.width = 0.95, fill = "grey80") +
  facet_grid(location ~ panel, scales = "free_y") +
  labs(y = "Escapement: Metric tons per year") +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  scale_y_continuous(labels = comma) +
  # theme(text = element_text(size = 12),
  #       axis.title = element_text(size = 14)) +
  geom_point(data = fig_ed1_data[[1]] %>% 
               filter(type == "dots"),
             aes(y = metric_tons),
             size = 0.3) +
  scale_fill_colorblind() + 
  scale_color_colorblind() + 
  labs(y = "Escapement: Metric tons per year",
       ) +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2),
        axis.title.x = element_blank())


fig_ed1b = fig_ed1_data[[2]] %>% 
  filter(type == "lines") %>%
  ggplot(aes(x = year, fill = species)) + 
  stat_lineribbon(.width = 0.95, aes(fill = species_order, y = metric_tons),
                  alpha = 0.4) +
  facet_grid(location ~ panel, scales = "free_y") +
  labs(y = "Escapement: Metric tons per year",
       x = "Year") +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  scale_y_continuous(labels = comma) +
  # theme(text = element_text(size = 12),
  #       axis.title = element_text(size = 14)) +
  geom_point(data = fig_ed1_data[[2]] %>%
               filter(type == "dots"),
             aes(color = species_order, y = metric_tons),
             size = 0.3) + 
  labs(fill = "",
       color = "",
       y = "") + 
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2),
        axis.title.x = element_blank(),
        legend.position = "top") +
  guides(color = "none") +
  scale_color_viridis_d(direction = 1, option = 'A', alpha = 1,
                        begin = 0.15, end = 0.7) +
  scale_fill_viridis_d(direction = 1, option = 'A', alpha = 1,
                       begin = 0.15, end = 0.7) 

fig_ed1cd = fig_ed1_data[[3]] %>%
  filter(species != "All") %>% 
  filter(type == "lines") %>% 
  ggplot(aes(x = chem_order, y = .epred)) +
  geom_boxplot(aes(fill = species_order),
               alpha = 0.7,
               outlier.shape = NA,
               linewidth = 0.1) +
  scale_y_log10() +
  facet_wrap(~panel, ncol = 1, scales = "free") +
  geom_point(data = fig_ed1_data[[3]] %>%
               filter(species != "All") %>% 
               filter(type == "dots"),
             position = position_dodge(width = 0.75), 
             aes(y = mean_concentration_standardized, group = species_order),
             size = 0.1) +
  guides(fill = "none",
         color = "none") +
  labs(y = "Whole body concentrations (mg/kg ww)",
       x = "Chemical") +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2),
        legend.title = element_blank(),
        legend.position = "top") +
  scale_color_viridis_d(direction = 1, option = 'A', alpha = 1,
                        begin = 0.15, end = 0.7) +
  scale_fill_viridis_d(direction = 1, option = 'A', alpha = 1,
                       begin = 0.15, end = 0.7)
# 
# legend_escapement_conc <- get_plot_component(fig_ed1b, 'guide-box-top', return_all = TRUE)
# 
# temp =  plot_grid(fig_ed1a,
#                   fig_ed1b + guides(fill = "none") ,
#                   fig_ed1cd,
#                   ncol = 3,
#                   rel_widths = c(0.36, 0.43, 0.55))

fig_ed1_abcd = fig_ed1a + fig_ed1b + fig_ed1cd + plot_layout(guides = "collect") & theme(legend.position = 'top')


# fig_ed1_abcd <- 
#     plot_grid(legend_escapement_conc,
#               temp,
#               ncol = 1,
#               rel_heights = c(0.1, 0.9))

ggview::ggview(fig_ed1_abcd,
               dpi = 500, width = 6.5, height = 6, units = "in")


saveRDS(fig_ed1_abcd, file = "plots/fig_ed1.rds")
ggsave(fig_ed1_abcd, file = "plots/fig_ed1.pdf",
       dpi = 500, width = 6.5, height = 6, units = "in")
ggsave(fig_ed1_abcd, file = "plots/fig_ed1.jpg",
       dpi = 500, width = 6.5, height = 6, units = "in")

# Figure ED2 --------------------------------------------------------------
fig_ed2_data = read_csv(file = "plots/fig_ed2_data.csv") %>% 
  mutate(species = as.factor(species),
         species = fct_relevel(species, "Coho", "Chinook", "Chum", "Sockeye", "Pink"),
         location = fct_relevel(location, "BeringSea", "BCWC", "CentralAK"),
         chemical = fct_relevel(chemical, "N", "P", "DHA", "EPA", "Hg", "DDTs", "PBDEs"))

fig_ed2 = fig_ed2_data %>% 
    ggplot(aes(x = year, y = median)) + 
    geom_area(position = "fill", aes(fill = species)) +
    facet_grid(chemical ~ location) +
    scale_fill_viridis_d(direction = -1, option = 'A', alpha = 1,
                         begin = 0.15, end = 0.7) +
    scale_x_continuous(breaks = c(1980, 1995, 2010)) +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    coord_cartesian(ylim = c(0, 1.1)) +
    labs(y = "Median proportional contributions to\nnutrient and contaminant flux",
         fill = "Species",
         x = "") +
    theme(panel.spacing = unit(0.2, "lines")) 

saveRDS(fig_ed2, file = "plots/fig_ed2.rds")  
ggsave(fig_ed2, file = "plots/fig_ed2.jpg",
       dpi = 400, width = 7, height = 9)
ggsave(fig_ed2, file = "plots/fig_ed2.pdf",
       dpi = 400, width = 7, height = 9)



# Figure S1 ---------------------------------------------------------------

lmg = readRDS(file = "data/lmg.rds") %>% 
  filter(term %in% c("conc_c", "millions_c", "kg_ind_c")) %>% 
  group_by(chemical) %>% 
  mutate(order = max(estimate),
         term = case_when(term == "conc_c" ~ "Chemical concentration",
                          term == "kg_ind_c" ~ "Weight",
                          TRUE ~ "Escapement")) %>% 
  mutate(term = fct_relevel(term, "Escapement",
                            "Chemical concentration"))%>% 
  mutate(chemical = case_when(chemical == "PBDE" ~ "PBDEs",
                              chemical == "DDT" ~ "DDTs",
                              TRUE ~ chemical)) 

rel_importance = lmg %>%
  ggplot(aes(x = reorder(chemical, order), y = estimate, 
             color = term)) +
  geom_pointrange(aes(ymin = 0, 
                      ymax = estimate, 
                      y = estimate,
                      shape = term), position=position_dodge(width = 0.4)) + 
  coord_flip() + 
  labs(x = "Chemical",
       y = expression(paste("Explained variance (", "R"^2,")")),
       color = "Coefficient",
       shape = "Coefficient") +
  scale_color_grey() + 
  guides(colour = guide_legend(reverse=T),
         shape = guide_legend(reverse = T)) +
  # theme(legend.position = "top") +
  NULL

saveRDS(rel_importance, file = "plots/fig_s1_plot.rds")
ggsave(rel_importance, file = "plots/fig_s1_plot.jpg", width = 7, height = 3, dpi = 500)
ggsave(rel_importance, file = "plots/fig_s1_plot.pdf", width = 7, height = 3, dpi = 500)


# Figure S2 ---------------------------------------------------------------

fig_s2_data = readRDS(file = "plots/fig_s2_data.rds") %>% 
  mutate(analyte = as.factor(analyte),
         analyte = fct_relevel(analyte, "N", "P", "DHA", "EPA",
                               "Hg", "PCBs", "DDTs"))

prior_post_parameters = fig_s2_data %>% 
  group_by(analyte, model, name, parameter_type) %>% 
  median_qi(value) %>% 
  ggplot(aes(x = name, color = model, y = value)) + 
  geom_point(size = 0.4, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = .lower, ymax = .upper), 
                 linewidth = 0.1, 
                 position = position_dodge(width = 0.5)) + 
  facet_wrap(~analyte, ncol = 2) +
  coord_flip()  + 
  scale_color_manual(values = c("#000000", "#e69f00")) +
  labs(y = "Parameter value",
       x = "Parameter name",
       color = "") 

ggview::ggview(prior_post_parameters, width = 6.5, height = 8)
ggsave(prior_post_parameters, width = 6.5, height = 8,
       file = "plots/fig_s2_plot.jpg", dpi = 500)
ggsave(prior_post_parameters, width = 6.5, height = 8,
       file = "plots/fig_s2_plot.pdf", dpi = 500)
saveRDS(prior_post_parameters, file = "plots/fig_s2_plot.rds")


# Figure S3 ---------------------------------------------------------------
fig_s3_data = readRDS(file = "plots/fig_s3_data.rds") 

prior_post_parameters_escapement = fig_s3_data %>% 
  group_by(model, name) %>% 
  median_qi(value) %>%
  separate(name, into = c("parameter", "group"), extra = "merge") %>% 
  mutate(parameter = case_when(parameter == "b" ~ "bs",
                               TRUE ~ parameter)) %>% 
  mutate(group = str_remove(group, "syearspecies_location"),
         group = str_remove(group, "syear:species_location")) %>% 
  mutate(group = as.factor(group),
         group = fct_relevel(group, "Intercept"),
         model = as.factor(model),
         model = fct_relevel(model, "Prior")
  ) %>% 
  ggplot(aes(x = group, color = model, y = value)) + 
  facet_wrap(~parameter, scales = "free_x") +
  geom_point(size = 0.4, position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = .lower, ymax = .upper), 
                 linewidth = 0.1, 
                 position = position_dodge(width = 0.5)) + 
  coord_flip()  + 
  scale_color_manual(values = c("#000000", "#e69f00")) +
  labs(y = "Parameter value",
       x = "Parameter name",
       color = "")

ggview::ggview(prior_post_parameters_escapement, width = 6.5, height = 5)
ggsave(prior_post_parameters_escapement, width = 6.5, height = 6,
       file = "plots/fig_s3_plot.jpg", dpi = 500)
ggsave(prior_post_parameters_escapement, width = 6.5, height = 6,
       file = "plots/fig_s3_plot.pdf", dpi = 500)
saveRDS(prior_post_parameters_escapement, file = "plots/fig_s3_plot.rds")


# Figure S4 ---------------------------------------------------------------
fig_s4_data = readRDS(file = "plots/fig_s4_data.rds")

prior_post_escapement = fig_s4_data %>% 
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
       y = "Return biomass: metric tons per year") 

ggview::ggview(prior_post_escapement, width = 6.5, height = 6)

ggsave(prior_post_escapement, file = "plots/fig_s4_plot.jpg", 
       width = 6.5, height = 6, dpi = 500)
ggsave(prior_post_escapement, file = "plots/fig_s4_plot.pdf", 
       width = 6.5, height = 6, dpi = 500)
saveRDS(prior_post_escapement, file = "plots/fig_s4_plot.rds")


# Figure S5 ---------------------------------------------------------------

fig_s5_data = readRDS("plots/fig_s5_data.rds") %>% 
  mutate(analyte = as.factor(analyte),
         analyte = fct_relevel(analyte, "N", "DHA", "EPA",
                               "Hg", "PCBs", "DDTs"))

prior_post_analytes = fig_s5_data %>% 
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

ggsave(prior_post_analytes, file = "plots/fig_s5_plot.jpg", 
       width = 6.5, height = 8, dpi = 500)
ggsave(prior_post_analytes, file = "plots/fig_s5_plot.pdf", 
       width = 6.5, height = 8, dpi = 500)
saveRDS(prior_post_analytes, file = "plots/fig_s5_plot.rds")

