###################################Chords Diagrams######################################
#######################################################################################


## Load circlize
library(circlize)
library(tidyverse)

## sector names
sectors <- c("Norway", "New Zealand", "Luxembourg", "Vietnam",
             "United Kingdom", "Slovenia", "Greece", "Kazakhstan")

## initialise layout
circos.initialize(sectors, xlim = c(2000,2021))

## plotting region
circos.track(sectors, ylim = c(15, 100),
             panel.fun = function(x, y) {
               circos.text(CELL_META$xcenter,
                           CELL_META$cell.ylim[2] + mm_y(6),
                           CELL_META$sector.index)
               circos.axis(labels.cex = 0.6)
             })
## clear the environment
circos.clear()

## read the data from renewable energy
data <- read_csv("renewable_continents.csv",
                 show_col_types = FALSE)

## I select the variable I will use to create the chord diagram
## Remove all the data with primary energy share = 0%
data_0 <- data %>%
  filter(share_electricity > 0,
         year == 2021)

## summary statistics
summary(data_0)


## we select all the data with share of primary energy > 35,
## which is the mean in 2021

data_35 <- data_0 %>%
  filter(share_electricity > 35)

## data for the matrix
renewable_wider <- data_35 %>%
  select(-c("country_code", "year",
            "share_electricity", "Continent")) %>%
  ## remove punctuations from countries names.
  mutate(territory = str_remove(territory, "[[:punct:]]")) %>%
  column_to_rownames("territory") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from=rowname, values_from=value)

##Chord diagram

## read the matrix
matr <- read_csv("renewable_data_35.csv",
                 show_col_types = FALSE) %>%
  column_to_rownames("name") %>%
  as.matrix()

## Introduce the chorDiagram function
## Create a very basic chord diagram
chordDiagram(matr)
## get information about the diagram
circos.info()
circos.clear()

circos.par(start.degree = -90)
chordDiagram(
  matr,
  big.gap = 70 ## increase the gap between the upper and lower sectors.
)
circos.clear()


circos.par(start.degree = -90)
chordDiagram(
  matr,
  big.gap = 70, ## increase the gap between the upper and lower sectors.
  small.gap = 3, ## gap between sectors
  annotationTrack = "grid", ## show only the track and the sectors, no names
  annotationTrackHeight = c(0.03, 0.01), ## height of annotation track
  preAllocateTracks = list(
    track.height = max(strwidth(unlist(dimnames(matr))))
  ) ## number of pre-allocated empty tracks this allow an easy customisation
)
circos.track(
  track.index = 1,
  panel.fun = function(x, y) {
    circos.text(
      CELL_META$xcenter,
      CELL_META$ylim[1],
      CELL_META$sector.index,
      facing = "clockwise",
      niceFacing = TRUE, ## Make sure that the label on the left fit human eyes. Anticlockwise
      adj = c(0, 0.5)    ## offset of the text to make sure that the text doesn't overlap with the track.
    )
  },
  bg.border = NA ## colour for the border of the plotting region
)
circos.clear()

##Customize chord diagram

## Create colour palette for the chord diagram
source_palette <- c("#1B9E77", "#D95F02", "#7570B3", "#A6761D") ## colour blind friendly

## create structure to assign to each renewable source a specific color

source_colour <- structure(source_palette, names = rownames(matr))

country_colour <- structure(rep("#1F78B4",
                                length(colnames(matr))),
                            names = colnames(matr))
## create a colour grid for the chord diagram
col_grid <- c(source_colour, country_colour)

circos.par(start.degree = -90)
chordDiagram(
  matr,
  big.gap = 70, ## increase the gap between the upper and lower sectors.
  small.gap = 3, ## gap between sectors
  grid.col = col_grid,  ## to change the colours
  annotationTrack = "grid", ## show only the track and the sectors, no names
  annotationTrackHeight = c(0.03, 0.01), ## height of annotation track
  preAllocateTracks = list(
    track.height = max(strwidth(unlist(dimnames(matr))))
  ) ## number of pre-allocated empty tracks to allow an easy customisation
)
circos.track(
  track.index = 1,
  panel.fun = function(x, y) {
    circos.text(
      CELL_META$xcenter,
      CELL_META$ylim[1],
      CELL_META$sector.index,
      facing = "clockwise",
      niceFacing = TRUE, ## Make sure that the label on the left fit human eyes. Anticlockwise
      adj = c(0, 0.5)    ## offset of the text to male sure that the text doesn't overlap with the track.
    )
  },
  bg.border = NA, ## colour for the border of the plotting region
  
)
# add title
title("Renewable source of energy per countries", cex = 10)
## add footnote
text(-0.6, -1, "*other include geothermal, biomass and other sources")
## add data source as foot note.
text(0.8, -1, pos = 4, "Source: `Our world in data`")
circos.clear()

##Customization options

## Create colour palette for the chord diagram
source_palette <- c("#1B9E77", "#D95F02", "#7570B3", "#A6761D") ## colour blind friendly

## create structure to assign to each renewable source a specific color

source_colour <- structure(source_palette, names = rownames(matr))

country_colour <- structure(rep("#1F78B4",
                                length(colnames(matr))),
                            names = colnames(matr))
## create a colour grid for the chord diagram
col_grid <- c(source_colour, country_colour)

circos.par(start.degree = -90)
chordDiagram(
  matr,
  big.gap = 70, ## increase the gap between the upper and lower sectors.
  small.gap = 3, ## gap between sectors
  ## add colours
  grid.col = col_grid,
  annotationTrack = "grid", ## show only the track and the sectors, no names
  annotationTrackHeight = c(0.03, 0.01), ## height of annotation track
  preAllocateTracks = list(
    track.height = max(strwidth(unlist(dimnames(matr))))
  ) ## number of pre-allocated empty tracks this allow an easy customisation
)
circos.track(
  track.index = 1,
  panel.fun = function(x, y) {
    circos.text(
      CELL_META$xcenter,
      CELL_META$ylim[1],
      CELL_META$sector.index,
      facing = "clockwise",
      niceFacing = TRUE, ## Make sure that the label on the left fit human eyes. Anticlockwise
      adj = c(0, 0.5)    ## offset of the text to male sure that the text doesn't overlap with the track.
    )
  },
  bg.border = NA ## colour for the border of the plotting region
)

## define continents

asia <- colnames(matr)[1:3]
europe <- colnames(matr)[4:23]
north_america <- colnames(matr)[24]
oceania <- colnames(matr)[25]
south_america <- colnames(matr)[26:31]

## highlight sectors

highlight.sector(c("hydro", "wind", "solar", "other"), col = NA,
                 border = "grey95",
                 lwd = 2,
                 text = "Source of renewable\n energy",
                 facing = "clockwise",
                 niceFacing = TRUE,
                 text.vjust = "18mm",
                 cex = 1.2
)

highlight.sector(asia, col = NA,
                 border = "grey95",
                 lwd = 2,
                 padding = c(0.01, 0.15, 0, 0.1),
                 text = "Asia",
                 facing = "clockwise",
                 text.vjust = "18mm",
                 cex = 1.2
)
highlight.sector(europe, col = NA,
                 border = "grey95",
                 lwd = 2,
                 padding = c(0.01, 0.02, 0, 0.01),
                 text = "Europe",
                 facing = "clockwise",
                 text.vjust = "20mm",
                 cex = 1.2
)
highlight.sector(north_america, col = NA,
                 border = "grey95",
                 lwd = 2,
                 padding = c(0, 0.01, 0, 0),
                 text = "North America",
                 facing = "clockwise",
                 text.vjust = "22mm",
                 cex = 1.2
)

highlight.sector(oceania, col = NA,
                 border = "grey95",
                 lwd = 2,
                 padding = c(0.01, 1, 0, 1.5),
                 text = "Oceania",
                 facing = "clockwise",
                 text.vjust = "18mm",
                 cex = 1.2
)
highlight.sector(south_america, col = NA,
                 border = "grey95",
                 lwd = 2,
                 padding = c(0.01, 0.01, 0, 0.04),
                 text = "South America",
                 facing = "clockwise",
                 text.vjust = "18mm",
                 cex = 1.2
)

circos.clear()

##Create and call a function

source("circos_function.R")

## read the matrix
matr <- read_csv("renewable_data_35.csv",
                 show_col_types = FALSE) %>%
  column_to_rownames("name") %>%
  as.matrix()

## Create colour palette for the chord diagram
source_palette <- c("#1B9E77", "#D95F02", "#7570B3", "#A6761D")

## create structure to assign to each renewable source a specific color

source_colour <- structure(source_palette, names = rownames(matr))

country_colour <- structure(rep("#1F78B4",
                                length(colnames(matr))),
                            names = colnames(matr))
## create a colour grid for the chord diagram
col_grid <- c(source_colour, country_colour)

circos.par(start.degree = -90)
chordDiagram(
  matr,
  big.gap = 70, ## increase the gap between the upper and lower sectors.
  small.gap = 3, ## gap between sectors
  grid.col = col_grid,  ## to change the colours
  annotationTrack = "grid", ## show only the track and the sectors, no names
  annotationTrackHeight = c(0.03, 0.01), ## height of annotation track
  preAllocateTracks = list(
    track.height = max(strwidth(unlist(dimnames(matr))))
  ) ## number of pre-allocated empty tracks this allow an easy customisation
)
circos.track(
  track.index = 1,
  panel.fun = function(x, y) {
    circos.text(
      CELL_META$xcenter,
      CELL_META$ylim[1],
      CELL_META$sector.index,
      facing = "clockwise",
      niceFacing = TRUE, ## Make sure that the label on the left fit human eyes. Anticlockwise
      adj = c(0, 0.5)    ## offset of the text to male sure that the text doesn't overlap with the track.
    )
  },
  bg.border = NA, ## colour for the border of the plotting region
  circos.clear()
)
# add title
title("Renewable source of energy per countries", cex = 10)
## add footnote
text(-0.6, -1, "*other include geothermal, biomass and other sources")
## add data source as foot note.
text(0.8, -1, pos = 4, "Source: `Our world in data`")
