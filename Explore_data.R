##########################Explore the Data###################################
#############################################################################

## Visual data exploration
library(tidyverse)


## read data from continents
ren <- read_csv("renewable_continents.csv",
                show_col_types = FALSE)

View(ren)
## create histogram
ren %>% 
  filter(year %in% 2000:2021) %>% 
  ggplot() +
  geom_histogram(aes(share_electricity), binwidth = 1)

## create a frequency polygons

ren %>% 
  filter(year %in% 2000:2021) %>% 
  ggplot() +
  geom_freqpoly(aes(share_electricity), colour = Continent)

## Countries that use more than 90% of renewable energy

ren %>% 
  filter(year %in% 2000:2021,
         Continent=="Europe",
         share_electricity >= 90) %>% 
  ggplot(aes(year, share_electricity, colour = territory)) +
  geom_line()