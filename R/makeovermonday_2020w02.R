library(tidyverse)
library(waffle)
library(ggpmthemes)
library(glue)

theme_set(theme_poppins())

# https://static-content.springer.com/esm/art%3A10.1186%2Fs12940-019-0488-0/MediaObjects/12940_2019_488_MOESM3_ESM.xlsx

tmpfile <- tempfile(fileext = ".xlsx")

tmpfile <-
  curl::curl_download(
    "https://static-content.springer.com/esm/art%3A10.1186%2Fs12940-019-0488-0/MediaObjects/12940_2019_488_MOESM3_ESM.xlsx",
    destfile = tmpfile
  )

df <- readxl::read_excel(tmpfile,
  sheet = "S10",
  range = "A3:F511"
) %>%
  janitor::clean_names() %>%
  select(-list) %>%
  mutate_at(vars(eu, usa), parse_number) %>%
  pivot_longer(-pesticide, names_to = "place", values_to = "status_id")

status_desc <- c(
  "0" = "Not in database/unknown",
  "1" = "Banned",
  "2" = "In process of phase out",
  "3" = "Approved",
  "4" = "Not approved/ vountarily withdrawn"
)

df <- df %>%
  mutate(status_desc = status_desc[as.character(status_id)])

df

df_viz <- df %>%
  mutate(
    place = case_when(
      place == "chn" ~ "China",
      place == "usa" ~ "USA",
      place == "bra" ~ "Brazil",
      place == "eu" ~ "Europe"
    )
  ) %>%
  count(place, status_desc, sort = TRUE)


# Plot --------------------------------------------------------------------

banned <- df_viz %>%
  filter(status_desc == "Banned")

subtitle <- str_wrap(
  glue(
    "The USA and China are lagging behind among the largest agricultural producers and users of pesticides in the world. Only {banned$n[banned$place == 'USA']} pesticides are banned in the USA and in China compared to Europe (n = {banned$n[banned$place == 'Europe']}) and Brazil (n = {banned$n[banned$place == 'Brazil']})."
  ),
  60
)

p <- df_viz %>%
  ggplot(aes(fill = str_wrap(status_desc, 15), values = n)) +
  geom_waffle(
    n_rows = 25,
    size = 0.5,
    colour = "#173f50",
    flip = TRUE
  ) +
  facet_wrap(~place) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  paletteer::scale_fill_paletteer_d("ghibli::MononokeLight", direction = 1) +
  labs(
    title = str_wrap("Pesticide status for the USA, EU, China and Brazil", 30),
    subtitle = subtitle,
    caption = "MakeoverMonday 2020w02\nData Source: https://ehjournal.biomedcentral.com/articles/10.1186/s12940-019-0488-0\nVisualization: @philmassicotte"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.width = unit(2.8, "cm"),
    legend.key.height = unit(0.25, "cm"),
    plot.background = element_rect(fill = "#173f50"),
    panel.background = element_rect(fill = "#173f50"),
    legend.background = element_blank(),
    legend.key = element_rect(fill = NA),
    text = element_text(color = "white"),
    strip.background = element_blank(),
    strip.text = element_text(
      color = "gray85",
      size = 24,
      face = "bold",
      family = "Cabin"
    ),
    panel.border = element_blank(),
    plot.title = element_text(
      family = "Roboto",
      color = "white",
      hjust = 0.5,
      face = "bold",
      size = 28
    ),
    plot.subtitle = element_text(
      family = "Poppins Light",
      color = "white",
      hjust = 0.5,
      face = "bold",
      size = 12
    ),
    legend.text = element_text(
      size = 10,
      color = "white",
      family = "Open Sans",
      face = "bold"
    ),
    plot.caption = element_text(size = 8)
  ) +
  guides(fill = guide_legend(
    label.position = "bottom",
    nrow = 1,
    override.aes = list(color = NA)
  )) +
  coord_equal()

pdf_file <- here::here("graphs", "makeovermonday_2020w02.pdf")
png_file <- here::here("graphs", "makeovermonday_2020w02.png")

ggsave(pdf_file,
  device = cairo_pdf,
  height = 9,
  width = 8
)

knitr::plot_crop(pdf_file)

bitmap <- pdftools::pdf_render_page(pdf_file, dpi = 600)
png::writePNG(bitmap, png_file)
