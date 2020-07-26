library(tidyverse)
library(ggpmthemes)

theme_set(theme_poppins())

df <-
  read_csv("https://query.data.world/s/wamstg4pj4mtcp3dakatcdwafqpkj3") %>%
  janitor::clean_names()

df


# G20 ---------------------------------------------------------------------

g20 <- tibble::tribble(
  ~country_code,
  "IND",
  "DEU",
  "RUS",
  "BRA",
  "FRA",
  "USA",
  "ZAF",
  "ITA",
  "AUS",
  "MEX",
  "GBR",
  "IDN",
  "CHN",
  "CAN",
  "JPN",
  "KOR",
  "TUR",
  "ARG",
  "SAU"
)

# Prepare data ------------------------------------------------------------

df_viz <- df %>%
  semi_join(g20) %>%
  filter(country_name != "World") %>%
  group_by(country_name, country_code) %>%
  drop_na() %>%
  # filter(country_name == "Canada") %>%
  summarise(
    difference =
      proportion_of_seats_held_by_women_in_national_parliaments_percent[year == max(year)] -
        proportion_of_seats_held_by_women_in_national_parliaments_percent[year == min(year)]
  ) %>%
  ungroup() %>%
  filter(difference > 0) %>%
  arrange(difference) %>%
  mutate(country_name = fct_reorder(country_name, difference))


# Labels ------------------------------------------------------------------


label_data <- df_viz %>%
  rowid_to_column(var = "id")

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-
  90 - 360 * (label_data$id - 0.5) / number_of_bar # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust <- ifelse(angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle <- ifelse(angle < -90, angle + 180, angle)

p <- df_viz %>%
  ggplot(aes(x = country_name, y = difference)) +
  geom_col(aes(fill = difference), show.legend = FALSE) +
  coord_polar() +
  paletteer::scale_fill_paletteer_c("grDevices::Sunset") +
  paletteer::scale_color_paletteer_c("grDevices::Sunset") +
  geom_text(
    data = label_data,
    aes(
      x = id,
      y = difference + 0.01,
      label = glue::glue("{country_name} (+{round(difference, digits = 2) * 100}%)"),
      hjust = hjust,
      color = difference
    ),
    size = 6,
    angle = label_data$angle,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  scale_y_continuous(limits = c(-0.5, NA), expand = expansion(mult = c(0, 0.15))) +
  scale_x_discrete(expand = c(0, 0)) +
  annotate(
    "text",
    x = 0,
    y = -0.5,
    label = str_wrap(
      "Countries of the G20 that increased the number of seats held by women in their parliaments between 1997 and 2019",
      20
    ),
    color = "white",
    size = 9,
    family = "Varela Round",
    fontface = "bold"
  ) +
  labs(
    caption = "MakeoverMonday 2020W30 | Visualization: @philmassicotte"
  ) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#2E3440"),
    panel.background = element_rect(fill = "#2E3440"),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    plot.caption = element_text(color = "#5C6270", size = 14, family = "Cairo")
  )

file <- here::here("graphs", "makeovermonday_2020w30.pdf")
ggsave(file,
  device = cairo_pdf,
  width = 12,
  height = 12
)

knitr::plot_crop("~/Desktop/test.pdf")
pdftools::pdf_convert(
  file,
  format = "png",
  filenames = here::here("graphs", "makeovermonday_2020w30.png"),
  dpi = 600
)
