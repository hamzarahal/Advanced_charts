#############################Annotation##################################
##################################################################################

library(tidyverse)
library(RColorBrewer)
library(colorblindcheck)
library(colorBlindness)
library(recolorize)

## read slope graph with default colours
slope_graph <- readRDS("slope_04_01.rdata")

# slope_graph +
#   scale_colour_manual(name = "",
#     values = c("blue", "lightblue", "darkblue", "purple")
#   )
# 
# cvd_grid(
#   slope_graph +
#     scale_colour_manual(name = "",
#       values = c("blue", "lightblue", "darkblue", "purple")
#     )
# )
## viridis colour palette
slope_graph +
  scale_colour_viridis_d(name = "", option = "magma", alpha = 0.5)

cvd_grid(slope_graph +
           scale_colour_viridis_d(name = "", option = "magma",
                                  alpha = 0.8))


## we select one of this palette and run again the code
# slope_graph +
#   scale_colour_brewer()

## use R colours brewer to find colour blind palette
display.brewer.all(colorblindFriendly = TRUE)

slope_graph +
  scale_colour_brewer(name = "",
                      palette = "Set2")

## brand colours

brand <- c("red", "darkgreen", "blue", "purple")
palette_plot(brand)
cvd_grid(palette_plot(brand, label_size = 4))


brand_rgb <- t(col2rgb(brand)/ 255 )

brand_sat <- adjust_color(brand_rgb, which_colors = c(2, 3),
                          saturation = 0.3, brightness = 0.9,
                          plotting = TRUE)
brand_hex <- rgb(brand_sat)

cvd_grid(palette_plot(brand_hex))

###Add charts on the same figure

library(tidyverse)
library(cowplot)
library(circlize)
source("circos_function.R")
source("col_grid.R")

## read the data and create the matrix
matr <- read_csv("renewable_data_65.csv",
                 show_col_types = FALSE) %>%
  column_to_rownames("name") %>%
  as.matrix()

## define colour grid for the chord diagram
col_grid <- col_grid()

## chord diagram
## formula format for circlize charts
cd <- ~circos_function(matr, col_grid, small_gap = 4)

circos.clear()

## Read sparklines
sp <- readRDS("sparkline.rdata")

plot_col <- plot_grid(sp, cd,
                      labels = c("Share of renewable energy ",
                                 "Renewable source of energy in 2021"),
                      ncol = 2,
                      hjust = 0.1,
                      label_x = 0.1,
                      label_y = 1.5,
                      rel_widths = c(1.1, 2),
                      rel_heights = c(0.5, 2)) +
  theme(plot.margin = margin(20, 3, 4, 8))

circos.clear()

## Add a title and subtitle. We need to create a ggplot element
title_gg <- ggplot() +
  labs(title = "Use of renewable energies per country",
       subtitle = "Share of renewable energy greater than 65% in 2021") +
  theme(
    plot.title = element_text(size = 24,
                              hjust = 0.5,
                              face = "bold"),
    plot.subtitle = element_text(size = 16,
                                 hjust = 0.5),
    plot.margin = margin(18, 0, 12, 0),
    plot.background = element_rect(
      fill = "grey92", colour = NA)
  )


panel <- plot_grid(title_gg,
                   plot_col,
                   ncol = 1, rel_heights = c(0.1, 1),
                   align = "v") +
  theme(
    plot.background = element_rect(
      fill = "grey92",
      colour = NA)
  )
panel
circos.clear()

###Background

plot_col <- plot_grid(sp, cd,
                      labels = c("Share of renewable energy ",
                                 "Renewable source of energy in 2021"),
                      ncol = 2,
                      hjust = 0.1,
                      label_x = 0.1,
                      label_y = 1.5,
                      rel_widths = c(1.1, 2),
                      rel_heights = c(0.5, 2)) +
  theme(plot.margin = margin(20, 3, 4, 8))

circos.clear()

## Add a title and subtitle. We need to create a ggplot element
title_gg <- ggplot() +
  labs(title = "Use of renewable energies per country",
       subtitle = "Share of renewable energy greater than 65% in 2021") +
  theme(
    plot.title = element_text(size = 24,
                              hjust = 0.5,
                              face = "bold"),
    plot.subtitle = element_text(size = 16,
                                 hjust = 0.5),
    plot.margin = margin(18, 0, 12, 0),
    plot.background = element_rect(
      fill = "grey92", colour = NA)
  )

panel <- plot_grid(title_gg,
                   plot_col,
                   ncol = 1, rel_heights = c(0.1, 1),
                   align = "v") +
  theme(
    plot.background = element_rect(
      fill = "grey92",
      colour = NA)
  )
panel
circos.clear()

panel_footnote <- add_sub(panel,
                          label = "Source: Our world in data",
                          fontface = "bold",
                          size = 10,
                          hjust = -1.2)

txt <- "'other' in the chord diagram refers to\n renewable sources including:\n geothermal, biomass, waste, wave and tidal."
ggdraw(panel_footnote) +
  geom_text(
    aes(x = 0.55, y = 0.2, label = txt),
    hjust = 0.5, vjust = 0.5,
    size = 12/.pt,
  )