library(tidyverse)
library(ggpmthemes)
library(pins)
library(sf)
library(glue)

theme_set(theme_exo())

theme_update(
  rect = element_rect(fill = "#2b2d2f"),
  text = element_text(colour = "white"),
  axis.text = element_text(colour = "white"),
  axis.ticks = element_line(colour = "white")
)

df <- read_csv("https://query.data.world/s/wcntg2ynqmimnjd7tl4d4ybjwf2nv6") %>%
  janitor::clean_names()

df <- df %>%
  mutate(file_date = lubridate::mdy(file_date)) %>%
  mutate(year = lubridate::year(file_date)) %>%
  extract(location, into = c("longitude", "latitude"), regex = "POINT \\((-?\\d+\\.\\d+) (\\d+\\.\\d+)\\)", convert = TRUE) %>%
  pivot_longer(c(non_payment:good_samaritan_ends), names_to = "reason", values_to = "value") %>%
  filter(value) %>%
  drop_na(longitude, latitude, year)

# df %>%
#   group_by(eviction_id) %>%
#   # filter(value) %>%
#   ungroup() %>%
#   count(eviction_id, sort = T)
#
df %>%
  count(neighborhoods_analysis_boundaries, sort = TRUE)

caption <- df %>%
   count(reason, sort = TRUE) %>%
   top_n(10) %>%
   mutate(reason = str_replace_all(reason, "_", " ")) %>%
   mutate(reason = str_to_title(reason)) %>%
   mutate(txt = glue("{reason} (n = {n})")) %>%
   pull(txt)

caption <- str_c("Number of eviction between 1997 and 2019: ", str_c(caption, collapse = ", "))

sf_shapefile <- curl::curl_download(
  "https://data.sfgov.org/api/geospatial/pty2-tcw4?method=export&format=Shapefile",
  destfile = tempfile(fileext = ".zip")
)

td <- tempdir()
sf_shapefile <- unzip(sf_shapefile, exdir = td)

sf <- st_read(td) %>%
  mutate(name = as.character(name))

df2 <- df %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(st_crs(sf)) %>%
  st_join(sf, .)

df2 %>%
  filter(year %in% c(1997, 2004, 2011, 2018)) %>%
  group_by(name, year) %>%
  summarise(sum_eviction = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot() +
  geom_sf(data = sf, fill = "#3c3c3c", size = 0.1, color = "white") +
  geom_sf(aes(fill = sum_eviction), size = 0.1, color = "white") +
  facet_wrap(~year, drop = TRUE, ncol = 4, strip.position = "bottom") +
  coord_sf() +
  scale_fill_viridis_c(trans = "log10", option = "plasma") +
  labs(
    fill = "Number of\nevictions",
    title = str_wrap("Total number of San Francisco eviction notices in 1997, 2004, 2011 and 2018", 50),
    subtitle = str_wrap(caption, width = 130),
    caption = "Makeover Monday (2019/W39) | Data Source: DataSF | Visualization: @philmassicotte"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.border = element_blank(),
    strip.background = element_rect(fill = "#2b2d2f"),
    strip.text = element_text(face = 2, size = 24, color = "white"),
    panel.background = element_rect(fill = "#2b2d2f"),
    plot.caption = element_text(size = 8, color = "white", hjust = 0),
    plot.subtitle = element_text(size = 8)
  )

# Save plot ---------------------------------------------------------------

destfile <- here::here("graphs", "makeovermonday_2019w39.pdf")

ggsave(
  destfile,
  device = cairo_pdf,
  width = 5.52 * 1.35,
  height = 5 * 1.35
)

knitr::plot_crop(destfile)

bitmap <- pdftools::pdf_render_page(destfile, dpi = 600)
destfile <- here::here("graphs", "makeovermonday_2019w39.png")
png::writePNG(bitmap, destfile)
