library(tidyverse)
library(rnaturalearth)
library(sf)
library(ggalt)

# Get the Data

individuals <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv")

locations <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv")

locations
individuals

# Much more femals
individuals %>%
  count(sex)

locations %>%
  count(animal_id)

# Herd --------------------------------------------------------------------

set.seed(1)

locations %>%
  sample_n(1500) %>%
  ggplot(aes(x = longitude, y = latitude, color = study_site)) +
  geom_point() +
  geom_encircle(s_shape = 0, expand = 0.02, show.legend = FALSE)
