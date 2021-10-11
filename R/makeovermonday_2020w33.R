library(tidyverse)
library(rnaturalearth)
library(sf)
library(httr)
library(readxl)
library(ggpmthemes)

theme_set(theme_poppins())

GET(
  "https://query.data.world/s/orueho2bpxgkwfuf5szdgzulhn66ch",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

df <- read_excel(tf) %>%
  janitor::clean_names()

new_names <- tibble::tribble(
  ~new_name, ~name_en,
  "Highlands & Islands", "Highland",
  "Argyll and Renfrewshire", "Argyll and Bute",
  "Glasgow", "Glasgow",
  "Ayrshire", "North Ayrshire",
  "Lanarkshire", "North Lanarkshire",
  "Stirlingshire & Clackmannanshire", "Clackmannanshire",
  "Midlothian", "Midlothian",
  "Borders", "Scottish Borders",
  "Fife", "Fife",
  "Angus", "Angus",
  "Aberdeenshire", "Aberdeenshire"
)

scotland <- ne_states("united kingdom",
  returnclass = "sf"
)

scotland_company <- scotland %>%
  inner_join(new_names) %>%
  inner_join(df, by = c("new_name" = "region"))

scotland_company_centers <- scotland_company %>%
  st_centroid() %>%
  as_tibble() %>%
  select(name_en, companies, geometry) %>%
  mutate(coords = map(geometry, function(x) {
    st_coordinates(x) %>%
      as_tibble() %>%
      set_names(c("longitude", "latitude"))
  })) %>%
  unnest(coords)

p <- scotland %>%
  filter_if(is.character, any_vars(str_detect(., fixed(
    "scotland",
    ignore_case = TRUE
  )))) %>%
  ggplot() +
  geom_sf(fill = "gray85", size = 0.25) +
  geom_sf(data = scotland_company, aes(fill = name_en), size = 0.25) +
  # geom_sf(data = scotland_company_centers) +
  geom_text(
    data = scotland_company_centers,
    aes(
      x = longitude,
      y = latitude,
      label = name_en
    ),
    label.family = c("Poppins"),
    size = 2
  ) +
  coord_sf(xlim = c(-7.5, -1.3), ylim = c(54.7, 59.2)) +
  theme_void() +
  theme(
    legend.position = "none"
  )
# +
#   geofacet::facet_geo(~name_en, grid = "scotland_local_authority_grid1")

ggsave(
  here::here("graphs/makeovermonday_2020w33.pdf"),
  width = 7,
  height = 7,
  device = cairo_pdf
)
