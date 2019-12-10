library(tidyverse)
library(httr)
library(readxl)
library(sf)
library(ggpmthemes)
library(ggtext)

theme_set(theme_maven())

GET(
  "https://query.data.world/s/u2gxpsonhnnm6o7llvzrint6medcvd",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)
df <- read_excel(tf) %>%
  janitor::clean_names()

df

us_county <-
  st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
us_state <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

us_outline <- us_state %>%
  st_simplify() %>%
  st_union() %>%
  st_buffer(dist = 0.25)

us_outline %>%
  ggplot() +
  geom_sf()

file <- pins::pin("https://raw.githubusercontent.com/gavinr/usa-mcdonalds-locations/master/mcdonalds.geojson")
mcdo <- sf::st_read(file) %>%
  st_transform(st_crs(us_county))

sb <- jsonlite::fromJSON(pins::pin("https://raw.githubusercontent.com/mmcloughlin/starbucks/master/locations.json")) %>%
  filter(country == "US") %>%
  as_tibble() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(st_crs(us_county))

sb <- st_join(us_county, sb) %>%
  group_by(ID) %>%
  summarise(n = n()) %>%
  mutate(name = "sb")

mcdo <- st_join(us_county, mcdo) %>%
  group_by(ID) %>%
  summarise(n = n()) %>%
  mutate(name = "mcdo")

most_popular <- rbind(sb, mcdo) %>%
  group_by(ID) %>%
  filter(n == max(n))


# Plot --------------------------------------------------------------------

subtitle <- "<b style='color:#FFC300'>McDonald's</b> and <b style='color:#00704A'>Starbucks</b> are the two most abundant and lucrative fast-food chains in the<br>USA. In 2017, <b style='color:#FFC300'>McDonald's</b> and <b style='color:#00704A'>Starbucks</b> generated $37,500,000,000 and $13,200,000,000<br>sales revenues respectively. At the county level, McDonald's are mostly present on the east<br>coast whereas Starbucks dominates on the west coast. "

p <- most_popular %>%
  ggplot() +
  geom_sf(aes(fill = name), size = 0.1, color = "#3c3c3c") +
  geom_sf(
    data = us_state,
    fill = NA,
    color = "#3c3c3c",
    size = 0.5
  ) +
  geom_sf(
    data = us_outline,
    fill = NA,
    size = 0.5,
    color = "white"
  ) +
  scale_fill_manual(
    values = c("mcdo" = "#FFC300", "sb" = "#00704A"),
    labels = c("McDonald's\nmostly present", "Starbucks\nmostly present"),
    guide = guide_legend(
      label.position = "top",
      label.theme = element_text(color = "white", size = 6, hjust = 0),
      keyheight = unit(0.15, "cm"),
      nrow = 1
    )
  ) +
  labs(
    title = "Fast-foods in the USA: the battle of the titans",
    subtitle = subtitle,
    caption = "**MakeoverMonday 2019W50 | Visualization: @philmassicotte<br>****Data sources:**<br>(1) *Fast-food sales*: Visual Capitalist<br>(2) <i style='color:#FFC300'>McDonald's</i>: raw.githubusercontent.com/gavinr/usa-mcdonalds-locations/master/mcdonalds.geojson<br>(3) <i style='color:#00704A'>Starbucks</i>: raw.githubusercontent.com/mmcloughlin/starbucks/master/locations.json"
  ) +
  coord_sf(crs = 2163) +
  theme(
    legend.justification = c(1, 1),
    legend.position =  c(0.3, 0.25),
    legend.key = element_rect(fill = NA),
    legend.background = element_blank(),
    legend.title = element_blank(),
    panel.border = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#333333"),
    panel.background = element_rect(fill = "#333333"),
    plot.title = element_text(color = "white", hjust = 0.5, size = 18),
    plot.subtitle = element_markdown(color = "gray85", size = 8, hjust = 0.5),
    plot.caption = element_markdown(color = "gray75", size = 5, hjust = 0)
  )

destfile <- here::here("graphs", "makeovermonday_2019w50.pdf")

ggsave(
  destfile,
  device = cairo_pdf,
  height = 6,
  width = 7
)

knitr::plot_crop(destfile)

bitmap <- pdftools::pdf_render_page(destfile, dpi = 600)
destfile <- here::here("graphs", "makeovermonday_2019w50.png")
png::writePNG(bitmap, destfile)
