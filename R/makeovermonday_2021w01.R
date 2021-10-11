library(tidyverse)
library(httr)
library(readxl)
library(ggpmthemes)
library(ggforce)

theme_set(theme_exo())

GET(
  "https://query.data.world/s/cuwpbvcchugiiyiyfl6qrvb5ovd3xf",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

df <- read_excel(tf, sheet = "Total Count data (31 counters)") %>%
  janitor::clean_names() %>%
  select(-week_of, -change_2019_2020)

df

df <- df %>%
  pivot_longer(starts_with("x")) %>%
  rename(week = timeframe) %>%
  mutate(week = parse_number(week)) %>%
  extract(name,
    into = "year",
    regex = "^x(\\d{4})",
    convert = TRUE
  )

df

range(df$week)

df <- df %>%
  mutate(date = as.Date(paste(year, week, 1), "%Y %U %u"))

df

df <- df %>%
  mutate(month = lubridate::month(date, label = TRUE, abbr = FALSE)) %>%
  group_by(year, month) %>%
  summarise(value = sum(value))

# https://stackoverflow.com/questions/58423666/attaching-half-circles-to-a-bar-plot

df %>%
  ungroup() %>%
  mutate(year = year - 2018) %>%
  ggplot(aes(x = year, y = value / 1e5)) +
  geom_col(aes(fill = factor(year)), position = "dodge") +
  geom_arc_bar(
    aes(
      x0 = 0,
      y0 = 0,
      # center
      r = year + 0.45,
      r0 = year - 0.45,
      # radii
      start = 0.5 * pi,
      end = 1.5 * pi,
      fill = factor(year)
    ),
    inherit.aes = FALSE,
    colour = NA
  ) +
  scale_y_continuous(labels = scales::label_number_si()) +
  facet_wrap(~month) +
  theme_void()

ggsave(
  "~/Desktop/test.pdf",
  device = cairo_pdf,
  width = 5,
  height = 5
)
