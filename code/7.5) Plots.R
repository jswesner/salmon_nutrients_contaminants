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
theme_set(theme_default())

# This code loads the the raw data of the figures and recreates the plots

# Figure 1 ----------------------------------------------------------------
# 1) load data
fig1_data = read_csv(file = "plots/fig1_data.csv")


# 2) make plot
# make left column
fig1_bdf = fig1_data %>%
  filter(!grepl("Species", panel)) %>% 
  ggplot(aes(x = year, y = median))  + 
  geom_ribbon(aes(ymin = low75, ymax = high75), fill = "grey50", alpha = 0.8) + 
  geom_line(aes(group = species), color = "black") +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  labs(y = expression("kg y"^-1),
       x = "Year",
       fill = "Species") +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2),
        text = element_text(family = "sans")) + 
  guides(fill = "none") +
  NULL

# make right column
fig1_ceg = fig1_data %>%
  filter(grepl("Species", panel)) %>% 
  mutate(species = as.factor(species),
         species = fct_relevel(species, "Pink", "Sockeye", "Chum", "Chinook", "Coho")) %>% 
  ggplot(aes(x = year, y = median))  + 
  geom_ribbon(aes(ymin = low75, ymax = high75, fill = species), alpha = 0.8) + 
  geom_line(aes(group = species), color = "black") +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) + 
  scale_fill_viridis_d(direction = -1, option = 'A', alpha = 1,
                       begin = 0.7, end = 0.15) +
  labs(y = expression("kg y"^-1),
       x = "Year",
       fill = "Species") +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2),
        text = element_text(family = "sans"),
        title = element_blank()) + 
  # guides(fill = "none") +
  NULL

# combine plots with patchwork

fig1_plot = fig1_bdf + fig1_ceg

saveRDS(fig1_plot, file = "plots/fig1_plot.rds")
ggsave(fig1_plot, file = "plots/fig1_plot.pdf", dpi = 600, width = 9, height = 9)
ggsave(fig1_plot, file = "plots/fig1_plot.jpg", dpi = 600, width = 9, height = 9)


# Figure 2 ----------------------------------------------------------------
# 1) load data
fig2_data = read_csv(file = "plots/fig2_data.csv") 

# 2) make plot
fig2a_d = fig2_data %>%
  filter(panel_letter %in% letters[1:4]) %>% 
  ggplot(aes(x = year, y = median)) +
  geom_linerange(aes(ymin = low75, ymax = high75, color = species), alpha = 0.5,
                 linewidth = .6) +
  geom_point(size = 0.2, aes(color = species)) +
  scale_color_viridis_d(direction = -1, option = 'A', alpha = 1,
                        begin = 0.7, end = 0.15) +
  scale_fill_viridis_d(direction = -1, option = 'A', alpha = 1,
                       begin = 0.7, end = 0.15) +
  guides(color = "none",
         fill = "none") +
  labs(y =  expression("Total Transport (kg y"^-1~")"),
       subtitle = "Nutrients") +
  facet_wrap(~ panel, ncol = 4) +
  scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  theme_default() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2)) +
  NULL

fig2e_h =  fig2_data %>%
  filter(panel_letter %in% letters[5:8]) %>% 
  ggplot(aes(x = year, y = median)) + 
  geom_area(position = "fill", aes(fill = species)) +
  facet_wrap( ~ panel, nrow = 1) +
  scale_fill_viridis_d(direction = -1, option = 'A', alpha = 1,
                       begin = 0.15, end = 0.7) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  coord_cartesian(ylim = c(0, 1.1)) +
  labs(y = "Proportional \nTransport",
       fill = "Species",
       x = "Year") +
  brms::theme_default()  + 
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

fig2i_l = fig2_data %>%
  filter(panel_letter %in% letters[9:12]) %>% 
  ggplot(aes(x = year, y = median)) +
  geom_linerange(aes(ymin = low75, ymax = high75, color = species), alpha = 0.5,
                 size = .6) +
  geom_point(size = 0.2, aes(color = species)) +
  scale_color_viridis_d(direction = -1, option = 'A', alpha = 1,
                        begin = 0.7, end = 0.15) +
  scale_fill_viridis_d(direction = -1, option = 'A', alpha = 1,
                       begin = 0.7, end = 0.15) +
  guides(color = "none",
         fill = "none") +
  labs(y =  expression("Total Transport (kg y"^-1~")"),
       subtitle = "Contaminants") +
  facet_wrap(~ panel, ncol = 4) +
  scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  theme_default() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2)) +
  NULL

fig2m_p = fig2_data %>%
  filter(panel_letter %in% letters[9:12]) %>% 
  ggplot(aes(x = year, y = median)) + 
  geom_area(position = "fill", aes(fill = species)) +
  facet_wrap( ~ panel, nrow = 1) +
  scale_fill_viridis_d(direction = -1, option = 'A', alpha = 1,
                       begin = 0.15, end = 0.7) +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  coord_cartesian(ylim = c(0, 1.1)) +
  labs(y = "Proportional \nTransport",
       fill = "Species",
       x = "Year") +
  brms::theme_default()  + 
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

fig2_legend = get_legend(fig2e_h)

# combine plots with patchwork
fig2_panels = (fig2a_d / (fig2e_h + guides(fill = "none")) / fig2i_l / fig2m_p + guides(fill = "none"))  

fig2_plot = plot_grid(fig2_panels, fig2_legend, rel_widths = c(1, 0.2))

saveRDS(fig2_plot, file = "plots/fig2_plot.rds")
ggsave(fig2_plot, file = "plots/fig2_plot.pdf", dpi = 600, width = 8, height = 8)
ggsave(fig2_plot, file = "plots/fig2_plot.jpg", dpi = 600, width = 8, height = 8)


# Figure 3 ----------------------------------------------------------------
# 1) load data
fig3_data = read_csv(file = "plots/fig3_data.csv")

# 2) make plot
fig3_plot_a_d = fig3_data %>% 
  filter(type == "Nutrients") %>% 
  ggplot(aes(x = year, y = median/1000)) +
  # geom_linerange(aes(ymin = low75, ymax = high75), alpha = 1,
  #                size = .5) +
  geom_ribbon(aes(ymin = low50/1000, ymax = high50/1000, fill= location), alpha = 0.25) +
  geom_line(aes(color= location), linewidth = 1.2) +
  # scale_color_colorblind() +
  # scale_color_brewer(type = "qual", palette = 3) +
  scale_color_viridis_d(direction = 1, option = 'E') +
  scale_fill_viridis_d(direction = 1, option = 'E') +
  # guides(color = "none",
  #        fill = guide_legend(override.aes = list(size = 7))) +
  labs(y = expression("Total Transport (MT y"^-1~")"),
       subtitle = "Nutrients",
       color = "Region",
       fill = "Region") +
  facet_wrap(~ panel, ncol = 4) +
  scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015),
                     limits = c(1975, 2030)) +
  theme_default() +
  theme(axis.title.x = element_blank(),
        strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2),
        text = element_text(family = "sans")) +
  NULL

# make second row of panels
fig3_plot_e_h = fig3_data %>% 
  filter(type != "Nutrients") %>% 
  ggplot(aes(x = year, y = median)) +
  geom_ribbon(aes(ymin = low50, ymax = high50, fill= location), alpha = 0.25) +
  geom_line(aes(color= location), size = 1.2) +
  scale_color_viridis_d(direction = 1, option = "E") +
  scale_fill_viridis_d(direction = 1, option = "E") +
  # scale_color_brewer(type = "qual", palette = 3) +
  # guides(fill = guide_legend(override.aes = list(size = 7))) +
  labs(y = expression("Total Transport (MT y"^-1~")"),
       subtitle = "Contaminants",
       color = "Region",
       fill = "Region") +
  facet_wrap(~ panel, ncol = 4) +
  scale_y_log10(labels = comma) +
  scale_x_continuous(breaks = c(1975, 1995, 2015),
                     limits = c(1975, 2030)) +
  theme_default() +
  theme(axis.title.x = element_blank(),
        strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2),
        text = element_text(family = "sans")) +
  NULL

# combine panels using patchwork package
fig3_plot_temp = (fig3_plot_a_d + guides(color = "none", fill = "none"))/
  (fig3_plot_e_h + guides(color = "none", fill = "none"))

fig3_legend = get_legend(fig3_plot_a_d)

fig3_plot = plot_grid(fig3_plot_temp, fig3_legend, ncol = 2, rel_widths = c(1, 0.2))


# final plot
saveRDS(fig3_plot, file = "plots/fig3_plot.rds")
ggsave(fig3_plot, file = "plots/fig3_plot.jpg", dpi = 600, width = 8, height = 4.5)
ggsave(fig3_plot, file = "plots/fig3_plot.pdf", dpi = 600, width = 8, height = 4.5)



# Figure 4 ----------------------------------------------------------------
# 1) load data
fig4_data = read_csv(file = "plots/fig4_data.csv")

# 2) make plot
fig4_a = fig4_data %>% 
  filter(panel == "a)") %>% 
  ggplot(aes(x = ratio_kgmgperfish, y = reorder(species_tl,tl))) +
  geom_density_ridges(scale = 1, size = 0.01) +
  geom_boxplot(outlier.shape = NA, width = 0.1, size = 0.25) +
  scale_color_viridis(option = "E",
                      direction = -1) +
  labs(color = "Year",
       x = "Ratio of nutrients (kg/fish) to\ncontaminants (mg/fish) for individual fish",
       subtitle = "a)") +
  annotate("text", label = "More nutrients per contaminant", x = 2.3, y = 0.74,
           size = 2) +
  annotate("segment", x = 2.5, y = 0.6, xend = 4, yend = 0.6,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9),
        plot.subtitle = element_text(size = 10)) +
  NULL

# get species means for Fig 4b
fig4_means = fig4_data %>% 
  filter(panel == "b)") %>% 
  group_by(species, .draw, species_tl, tl) %>% 
  reframe(mean_prop = mean(cont_minus_nut))

fig4_b = fig4_data  %>% 
  ggplot(aes(x = cont_minus_nut, y = reorder(species_tl,tl))) +
  geom_density_ridges(fill = NA, scale = 0.9,
                      size = 0.1,
                      aes(group = interaction(species, year),
                          color = year)) +
  geom_boxplot(data = fig4_means, aes(group = species, x = mean_prop), outlier.shape = NA,
               width = 0.1,
               size = 0.25) +
  scale_color_viridis(option = "E",
                      direction = -1) +
  geom_vline(xintercept = 0) + 
  labs(y = "", 
       color = "Year",
       x = "Relative contribution to continental biotransport",
       subtitle = "b)") +
  annotate("text", label = "Relatively more contaminants", x = -0.14, y = 0.6,
           size = 2.0) +
  annotate("text", label = "Relatively more nutrients", x = 0.14, y = 0.6,
           size = 2.0) +
  theme(legend.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 9),
        plot.subtitle = element_text(size = 10)) +
  NULL

# combine plots with patchwork
fig4_plot = fig4_a + fig4_b

saveRDS(fig4_plot, file = "plots/fig4_plot.rds")
ggsave(fig4_plot, file = "plots/fig4_plot.jpg", dpi = 600, width = 6.5, height = 3.5)
ggsave(fig4_plot, file = "plots/fig4_plot.pdf", dpi = 600, width = 6.5, height = 3.5)


# Figure 5 ----------------------------------------------------------------
# Note: Need to get data for Figure 5a.
# 1) load data
fig5_data = read_csv(file = "plots/fig5_data.csv")

# 2) make plot
fig5_plot = fig5_data %>%
  ggplot(aes(x = risk_quotient, y = reorder(species_tl,tl))) +
  geom_density_ridges(scale = 1) +
  geom_boxplot(outlier.shape = NA, width = 0.1, size = 1) +
  scale_color_viridis(option = "E",
                      direction = -1) +
  # geom_vline(xintercept = 1, linetype = "dashed") + 
  labs(x = "Risk-Benefit Quotient") +
  annotate("text", label = "More risk to consumers", x = 0.4, y = 0.8,
           size = 3) +
  annotate("segment", x = 0.1, y = 0.6, xend = 0.5, yend = 0.6,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  coord_cartesian(xlim = c(NA, 0.8)) +
  # labs(subtitle = "c) Risk-Benefit Quotient") +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9)) +
  # scale_x_log10() +
  NULL



saveRDS(fig5_plot, file = "plots/fig5_plot.rds")
ggsave(fig5_plot, file = "plots/fig5_plot.jpg", width = 5, height = 5, units = "in", dpi = 500 )
ggsave(fig5_plot, file = "plots/fig5_plot.pdf", width = 5, height = 5, units = "in", dpi = 500 )




# Figure ED1 --------------------------------------------------------------

total_esc_data = total_escapement$data %>% 
  mutate(panel = "a) Total Returns",
         data_type = "a_trends")

total_esc_raw = read_csv(file = "data/d_region_toplot.csv")  %>% 
  filter(species == "Total") %>% 
  mutate(panel = "a) Total Returns",
         data_type = "a_data") 

figed1_a_data = bind_rows(total_esc_data, 
                          total_esc_raw)

fig_ed1_data = bind_rows(figed1_a_data, fig_ed1_data)

write_csv(fig_ed1_data, file = "plots/fig_ed1_data.csv")

fig_ed1_data = read_csv(file = "plots/fig_ed1_data.csv")


fig_ed1a = fig_ed1_data %>%
  filter(data_type == "a_trends") %>%
  ggplot(aes(x = year)) + 
  geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.4) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.8) +
  geom_line(aes(y = med)) +
  facet_grid(location ~ ., scales = "free_y") +
  labs(y = "Escapement: Metric tons per year",
       x = "Year") +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  scale_y_continuous(labels = comma) +
  # theme(text = element_text(size = 12),
  #       axis.title = element_text(size = 14)) +
  geom_point(data = fig_ed1_data %>%
               filter(data_type == "a_data"),
             aes(y = value),
             size = 0.3) +
  scale_fill_colorblind() + 
  scale_color_colorblind() + 
  labs(y = "Escapement: Metric tons per year",
       subtitle = "a) Total Escapement") +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2),
        text = element_text(family = "sans"),
        plot.subtitle = element_text(size = 11))


fig_ed1b = fig_ed1_data %>%
  filter(data_type == "b_trends") %>%
  ggplot(aes(x = year, fill = species)) + 
  geom_ribbon(aes(ymin = low, ymax = upper), alpha = 0.4) +
  geom_ribbon(aes(ymin = low50, ymax = upper50), alpha = 0.4) +
  geom_line(aes(y = med)) +
  facet_grid(location ~ ., scales = "free_y") +
  labs(y = "Escapement: Metric tons per year",
       x = "Year") +
  scale_x_continuous(breaks = c(1975, 1995, 2015)) +
  scale_y_continuous(labels = comma) +
  # theme(text = element_text(size = 12),
  #       axis.title = element_text(size = 14)) +
  geom_point(data = fig_ed1_data %>%
               filter(data_type == "b_data"),
             aes(y = value, 
                 color = species),
             size = 0.3) + 
  labs(fill = "",
       color = "",
       y = "",
       subtitle = "b) Species Escapement") + 
  theme(legend.text=element_text(size=10),
        strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2),
        text = element_text(family = "sans"),
        plot.subtitle = element_text(size = 11)) +
  guides(fill = guide_legend(override.aes = 
                               list(alpha = .8))) +
  scale_color_viridis_d(direction = 1, option = 'A', alpha = 1,
                        begin = 0.15, end = 0.7) +
  scale_fill_viridis_d(direction = 1, option = 'A', alpha = 1,
                       begin = 0.15, end = 0.7) 

fig_ed1cd = fig_ed1_data %>% 
  filter(data_type == "cd_trends") %>% 
  ggplot(aes(x = chemical, y = .epred)) +
  geom_boxplot(aes(fill = species),
               alpha = 0.7,
               outlier.shape = NA) +
  scale_y_log10() +
  facet_wrap(~panel, ncol = 1, scales = "free") +
  geom_point(data = fig_ed1_data %>% 
               filter(data_type == "cd_data"),
             position = position_dodge(width = 0.75), 
             aes(y = mean_concentration_standardized, group = species),
             size = 0.4) +
  # guides(fill = "none") +
  labs(y = "Whole body concentrations (mg/kg ww)",
       x = "Chemical") +
  theme(strip.text.x = element_text(angle = 0, hjust = 0, vjust = -1.2),
        text = element_text(family = "sans"),
        plot.subtitle = element_text(size = 11),
        legend.title = element_blank()) +
  scale_color_viridis_d(direction = 1, option = 'A', alpha = 1,
                        begin = 0.15, end = 0.7) +
  scale_fill_viridis_d(direction = 1, option = 'A', alpha = 1,
                       begin = 0.15, end = 0.7)

legend_escapement_conc <- get_legend(fig_ed1_cd + theme(legend.position = "top"))

temp =  plot_grid(fig_ed1a,
                  fig_ed1b +
                    guides(color = "none", 
                           fill = "none"),
                  fig_ed1cd +
                    guides(color = "none", 
                           fill = "none"),
                  ncol = 3,
                  rel_widths = c(0.36, 0.43, 0.55))

(fig_ed1_abcd <- 
    plot_grid(legend_escapement_conc,
              temp,
              ncol = 1,
              rel_heights = c(0.1, 1)))

saveRDS(fig_ed1_abcd, file = "plots/ms_plots/fig_ed1_abcd.rds")
# ggview::ggview(fig_ed1_abcd,
#                dpi = 500, width = 6.5, height = 6, units = "in")
ggsave(fig_ed1_abcd, file = "plots/fig_ed1_abcd.pdf",
       dpi = 500, width = 6.5, height = 6, units = "in")
ggsave(fig_ed1_abcd, file = "plots/ms_plots/fig_ed1_abcd.jpg",
       dpi = 500, width = 6.5, height = 6, units = "in")

# Figure ED2 --------------------------------------------------------------
fig_ed2_data = read_csv(file = "plots/fig_ed2_data.csv")

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
         x = "Year") +
    theme(text = element_text(size = 11),
          axis.title = element_text(size = 11),
          legend.text = element_text(size = 11),
          # strip.text = element_blank(),
          # strip.background = element_blank(),
          panel.spacing = unit(0.2, "lines")) 



saveRDS(fig_ed2, file = "plots/ms_plots/fig_ed2.rds")  
ggsave(fig_ed2, file = "plots/ms_plots/fig_ed2.jpg",
       dpi = 400, width = 7, height = 9)
ggsave(fig_ed2, file = "plots/fig_ed2.jpg",
       dpi = 400, width = 7, height = 9)






