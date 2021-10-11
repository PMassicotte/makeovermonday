library(tidyverse)
library(httr)
library(readxl)
library(waffle) # install.packages("waffle", repos = "https://cinc.rud.is")
library(geofacet)
library(USAboundaries)

GET(
  "https://query.data.world/s/gfdbclhvli6fguiprusawb4sbkmfk7",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

df <- read_excel(tf, skip = 1) %>%
  janitor::clean_names()

us_states <- us_states() %>%
  as_tibble() %>%
  select(state_name, state_abbr)

df_viz <- df %>%
  select(-difference) %>%
  pivot_longer(c("full_coverage", "minimum_coverage")) %>%
  filter(state %in% c("Michigan", "Florida")) %>%
  left_join(us_states, by = c("state" = "state_name"))

df_viz %>%
  distinct(state_abbr, .keep_all = TRUE)

df_viz %>%
  ggplot(aes(fill = name, values = value)) +
  geom_waffle(
    make_proportional = TRUE,
    flip = TRUE,
    radius = unit(1, "mm"),
    size = 0.1
  ) +
  facet_geo(~state_abbr)
