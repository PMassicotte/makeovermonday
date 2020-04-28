library(httr)
library(readxl)
library(tidyverse)
library(ggpmthemes)
library(emojifont)

rm(list = ls())

load.fontawesome()

theme_set(theme_light_modified(base_family = "Baloo"))

# Gas price ---------------------------------------------------------------

GET(
  "https://query.data.world/s/osn5evkuxowuquchuwppithzywmrji",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

gas_price <- read_excel(tf) %>%
  janitor::clean_names() %>%
  drop_na() %>%
  mutate(date = parse_number(date)) %>%
  mutate(date = janitor::excel_numeric_to_date(date)) %>%
  pivot_longer(-date, names_to = "type", values_to = "price")

gas_price

# Boring graph...
gas_price %>%
  ggplot(aes(x = date, y = price, color = type)) +
  geom_line()

gas_price_viz <- gas_price %>%
  group_by(year = lubridate::year(date), type) %>%
  summarise(mean_price = mean(price)) %>%
  ungroup() %>%
  filter(type == "petrol_usd")

gas_price_viz %>%
  ggplot(aes(x = mean_price, y = factor(year))) +
  geom_col()

# Fuel consumption --------------------------------------------------------

# https://open.canada.ca/data/en/dataset/98f1a129-f628-4ce4-b24d-6f16bf24dd64

files <- fs::dir_ls("data/raw/2020_week_17/")

read_fuel_consumption <- function(file) {
  df <- data.table::fread(
    file,
    select = c(1, 4, 5, 6, 11),
    skip = 2,
    col.names = c("year", "class", "engine_size", "cylinders", "l_100_km")
  ) %>%
    as_tibble() %>%
    filter(str_detect(year, "^\\d{4}$")) %>%
    type_convert() %>%
    mutate(class = str_to_lower(class)) %>%
    mutate(class = str_replace(class, ": ", " - "))


  return(df)
}

fuel_consumption <- map_df(files, read_fuel_consumption)

fuel_consumption %>%
  count(class, year, sort = TRUE) %>%
  arrange(class, year) %>%
  select(-n) %>%
  count(class) %>%
  arrange(desc(n))

# Select "pickup truck - standard" and "compact" because they have 18 years of
# data

fuel_consumption_viz <- fuel_consumption %>%
  filter(class %in% c("pickup truck - standard", "compact")) %>%
  group_by(year, class) %>%
  summarise(mean_consumption_l_100_km = mean(l_100_km), n = n())

fuel_consumption_viz %>%
  ggplot(aes(x = mean_consumption_l_100_km, y = factor(year), fill = class)) +
  geom_col(position = "dodge")

df <- inner_join(gas_price_viz, fuel_consumption_viz, by = "year")

df

# Calculate average price for 20 000 km per year --------------------------

df_viz <- df %>%
  mutate(liter_per_year = mean_consumption_l_100_km * 20000 / 100) %>%
  mutate(price_per_year = liter_per_year * mean_price / 100)

df_viz

# Add icons ---------------------------------------------------------------

df_viz <- df_viz %>%
  mutate(
    label = case_when(
      class == "compact" ~ fontawesome("fa-car"),
      class == "pickup truck - standard" ~ fontawesome("fa-truck")
    )
  )

bg_color <- "#DADFD7"

p <- df_viz %>%
  mutate(consumption_label = round(mean_consumption_l_100_km, digits = 1)) %>%
  mutate(consumption_label = paste0(consumption_label, " L/100km")) %>%
  ggplot(aes(x = year, y = price_per_year, color = class)) +
  geom_line(size = 3) +
  geom_point(size = 15, color = bg_color) +
  # geom_point(size = 14.5, color = "#3c3c3c") +
  # geom_point(size = 14) +
  geom_text(
    aes(label = label),
    family = "fontawesome-webfont",
    size = 5,
    color = "#3c3c3c",
    vjust = 0
  ) +
  geom_text(
    aes(label = consumption_label, y = price_per_year + 110),
    size = 2,
    color = "#3c3c3c",
    fontface = "italic",
    parse = FALSE
  ) +
  geom_text(
    aes(label = scales::label_dollar()(round(
      price_per_year,
      digits = 0
    ))),
    size = 2,
    color = "#3c3c3c",
    vjust = 3,
    fontface = "bold"
  ) +
  scale_x_continuous(breaks = 2000:2030) +
  scale_color_manual(values = c(
    "compact" = "#E0B9AA",
    "pickup truck - standard" = "#9CB290"
  )) +
  labs(
    title = str_wrap("Pump prices over time: how much does it cost to drive 20 000 km per year?", 40),
    subtitle = str_wrap("Fuel consumptions for each car type are averaged based on fuel consumption statistics using between 52 and 186 car models. The prices calculated are based on 20,000 km driven per year.", 130),
    caption = "MakeoverMonday 2020W17\nVisualization: @philmassicotte\nFuel prices: Department for Business, Energy & Industrial Strategy\nCar fuel consumption ratings: https://open.canada.ca/data/en/dataset/98f1a129-f628-4ce4-b24d-6f16bf24dd64"
  ) +
  geom_fontawesome(
    alias = "fa-truck",
    color = "#3c3c3c",
    size = 10,
    x = 2004,
    y = 4000
  ) +
  geom_fontawesome(
    alias = "fa-car",
    color = "#3c3c3c",
    size = 10,
    x = 2004,
    y = 3700
  ) +
  annotate(
    "text",
    x = 2004.5,
    y = 3700,
    label = "Compact cars",
    hjust = 0,
    size = 5,
    fontface = "bold",
    family = "Roboto"
  ) +
  annotate(
    "text",
    x = 2004.5,
    y = 4000,
    label = "Pickup trucks",
    hjust = 0,
    size = 5,
    fontface = "bold"
  ) +
  theme(
    legend.text = element_text(family = "fontawesome-webfont"),
    legend.position = "none",
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_line(size = 0.25, color = "gray75"),
    panel.background = element_rect(fill = bg_color),
    plot.background = element_rect(fill = bg_color),
    plot.title = element_text(hjust = 0.5, face = "bold", color = "#3c3c3c", size = 28),
    plot.caption = element_text(size = 8, colour = "gray35"),
    plot.subtitle = element_text(color = "gray35", hjust = 0)
  )

ggsave(
  "graphs/makeovermonday_2020w17.pdf",
  device = cairo_pdf,
  width = 10,
  height = 8
)

bitmap <- pdftools::pdf_render_page("graphs/makeovermonday_2020w17.pdf", dpi = 600)
destfile <- "graphs/makeovermonday_2020w17.png"
png::writePNG(bitmap, destfile)
