library(tidyverse)
library(ggpmthemes)
library(santoku)

theme_set(theme_exo2(base_size = 12))

df <- read_csv("https://query.data.world/s/wh62hpfc3bmhcltwkjlkmrul2f3f4t") %>%
  janitor::clean_names()

df %>%
  count(year, age) %>%
  pull(n) %>%
  range()

population <- c(
  "https://www.ons.gov.uk/generator?format=csv&uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/timeseries/enpop/pop",
  "https://www.ons.gov.uk/generator?format=csv&uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/timeseries/wapop/pop"
) %>%
  map_df(read_csv, skip = 8, col_names = c("year", "population")) %>%
  group_by(year) %>%
  summarise(population = sum(population))

p <- df %>%
  mutate(class_age = chop_width(age, 5)) %>%
  group_by(year, class_age) %>%
  summarise(suicides = sum(suicides)) %>%
  ggplot(aes(x = year, y = suicides, fill = class_age)) +
  geom_area() +
  scale_fill_viridis_d(option = "B") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1970, 2100, by = 5)) +
  xlab(NULL) +
  ylab("Number of suicide") +
  theme(
    plot.background = element_rect(fill = "#3c3c3c"),
    panel.background = element_rect(fill = "#3c3c3c"),
    legend.background = element_rect(fill = "#3c3c3c"),
    legend.key = element_rect(color = NA, fill = NA),
    text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    panel.grid = element_line(color = "gray50"),
    panel.border = element_rect(color = "gray50"),
    plot.caption = element_text(color = "gray75")
  ) +
  labs(
    title = str_wrap("Trend by age group at which most people are dying by suicide England and Wales between 1981 and 2017", 50),
    subtitle = str_wrap("There was a slight decrease between 1981 and 2008. However, the number of suicide seems to be increasing since the last decade.", 90),
    caption = "MakeoverMonday 2019W43 | Data source: Office for National Statistics | Visualization: @philmassicotte",
    fill = "Age"
  )

ggsave(
  here::here("graphs", "makeovermonday_2019w43.png"),
  device = "png",
  type = "cairo",
  height = 8,
  width = 9
)

df %>%
  ggplot(aes(x = year, y = age, fill = suicides)) +
  geom_tile() +
  scale_fill_viridis_c(option = "B") +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, by = 5)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1970, 2100, by = 5))

df %>%
  mutate(class_age = chop_width(age, 10)) %>%
  group_by(year, class_age) %>%
  summarise(suicides = sum(suicides)) %>%
  left_join(population) %>%
  mutate(relative_suicide = suicides / population) %>%
  ggplot(aes(x = year, y = relative_suicide, fill = class_age)) +
  geom_area() +
  scale_fill_viridis_d(option = "B") +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))
