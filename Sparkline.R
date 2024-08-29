#############################Sparkline and others##################################
##################################################################################

library(tidyverse)

data <- read_csv("renewable_65_perc.csv",
                 show_col_types = FALSE)

## I want to compare The share of renewable energy per countries.
## We calculate the max and min for each countries

min_df <- data %>%
  group_by(territory) %>%
  slice(which.min(share_electricity))

max_df <- data %>%
  group_by(territory) %>%
  slice(which.max(share_electricity))

col <- c("min" = "#2c7bb6", "max" = "#d7191c")

data %>%
  ggplot(aes(x = year, y = share_electricity)) +
  facet_wrap(territory ~ .,
             scales = "free_y",
             ncol = 3
  ) +
  geom_line(linewidth = 0.3) +
  geom_point(data = min_df, aes(colour = "min")) +
  geom_point(data = max_df, aes(colour = "max")) +
  geom_text(data = min_df, aes(label = round(share_electricity, 1)),
            size = 5,
            vjust = -1) +
  geom_text(data = max_df, aes(label = round(share_electricity, 1)),
            size = 5,
            vjust = 1.5, hjust = 0.5) +
  theme_minimal() +
  ylab("") + xlab("") +
  coord_cartesian(clip = "off") +
  scale_x_continuous(breaks = c(2000, 2021)) +
  scale_colour_manual(
    name = "",
    breaks = c("min", "max"),
    values = col) +
  theme(
    axis.text.x =element_text(size = 12),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 14),
    panel.spacing = unit(2, "lines"),
    legend.position = "bottom"
  )

###create a lollipop plot

data <- read_csv("renewable_continents.csv",
                 show_col_types = FALSE)


## select the top countries that use solar energy.
top_10 <- data %>%
  filter(year == 2021) %>%
  slice_max(solar, n = 10)

ggdotchart(
  top_10,
  y = "solar",
  x = "territory",
  color = "Continent",
  palette = "jco",
  legend.title = "Continents",
  sorting = "descending",
  rotate = TRUE,
  add = "segments",
  add.params = list(color = "black", size = 0.5),
  dot.size = 8,
  label = round(top_10$solar),
  font.label = list(color = "grey95",
                    size = 10,
                    vjust = 0.5,
                    face = "bold"),
  ggtheme = theme_void()
) +
  ggtitle("Solar energy in 2021 in TWh") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 22)
  )

###Create a slop graph

library(ggrepel)
library(ggpp)

data <- read_csv("renewable_03_04.csv",
                 show_col_types = FALSE)

ggs <- data %>%
  ggplot(
    aes(
      x = year, y = share_electricity,
      group = territory,
      colour = Continent
    )) +
  geom_line(linewidth = 2, alpha = 0.5) +
  geom_point(size = 3) +
  scale_colour_manual(name = "Continents",
                      values = ggsci::pal_jco("default")(4)) +
  scale_x_continuous(breaks = c(2000, 2021)) +
  theme_void() +
  theme(
    axis.text.x = element_text(size = 16),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 30, face = "bold",
                              hjust = 0.5,
                              margin = margin(10,0,10,0)),
    plot.margin = unit(c(1,8,1,8), "lines")
  )

## add labels
ggs +
  geom_text_repel(
    data = data,
    aes(x = year, y = share_electricity,
        label = paste0(territory, " - ",
                       round(share_electricity), "%")),
    force =  20,
    show.legend = FALSE,
    size = 5,
    position = position_nudge_to(x = c(2000-13, 2021 + 13))) +
  ggtitle("Share of renewable energy for the \ntop 10 countries that use solar energy")
