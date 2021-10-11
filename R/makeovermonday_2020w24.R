library(tidyverse)
library(httr)
library(readxl)
library(geofacet)
library(ggpmthemes)

theme_set(theme_light_modified(base_family = "Oxanium"))

GET("https://query.data.world/s/rxkbxztblyj4w4h57jhbkoaohnd5yv", write_disk(tf <- tempfile(fileext = ".xlsx")))

df <- read_excel(tf) %>%
  janitor::clean_names()

df

df %>%
  count(sport, sort = TRUE)

df %>%
  pivot_longer(-c(year, state, sport)) %>%
  drop_na() %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~name, scales = "free") +
  scale_x_log10()

df_viz <- df %>%
  pivot_longer(-c(year, state, sport)) %>%
  separate(name, into = c("sex", "type")) %>%
  separate(
    year,
    into = c("year_start", "year_end"),
    convert = TRUE,
    remove = FALSE
  ) %>%
  drop_na()

df_viz

df_viz <- df_viz %>%
  filter(str_detect(type, fixed("participation", ignore_case = TRUE))) %>%
  filter(str_detect(sport, fixed("soccer", ignore_case = TRUE))) %>%
  group_by(year_start, year_end, state, sex) %>%
  summarise(value = sum(value, na.rm = TRUE))

df_viz %>%
  group_by(state, sex) %>%
  # filter(state == "AK" & sex == "boys") %>%
  mutate(v = (value - value[year_start == 2002]) / value[year_start == 2002]) %>%
  ggplot(aes(x = year_start, y = v, color = sex)) +
  geom_line() +
  facet_geo(~state, scales = "free")

# Plot --------------------------------------------------------------------

df_viz <- df_viz %>%
  ungroup() %>%
  mutate(state2 = state.name[match(state, state.abb)])

p <- df_viz %>%
  group_by(state, sex) %>%
  mutate(v = value / max(value)) %>%
  ggplot(aes(x = year_start, y = v, color = sex)) +
  geom_line(size = 1) +
  facet_geo(~state, scales = "free_y") +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 4),
    labels = scales::label_percent()
  ) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 6)) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 6),
    panel.grid.major.y = element_line(size = 0.1, color = "gray75")
  )

ggsave(
  here::here("graphs/", "makeovermonday_2020w24.png"),
  dpi = 600,
  width = 18,
  height = 10
)
