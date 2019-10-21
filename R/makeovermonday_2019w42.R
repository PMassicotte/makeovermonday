library(tidyverse)
library(ggpmthemes)
library(glue)
library(ggalt)
library(ggtext)
library(chron)
library(httr)
library(readxl)

theme_set(theme_exo())

GET("https://query.data.world/s/w2ac457otr52o2baz5mbwrucrcnp2d", write_disk(tf <- tempfile(fileext = ".xlsx")))

df <- read_excel(tf) %>%
  janitor::clean_names() %>%
  mutate(time = hms::as_hms(time))

df_viz <- df %>%
  select(-athlete, -country) %>%
  pivot_wider(names_from = gender, values_from = time) %>%
  mutate(year = case_when(
    year == "1982 (Oct)" ~ "1982",
    year == "1982 (Feb)" ~ "1982",
    TRUE ~ year
  )) %>%
  mutate(year = parse_number(year)) %>%
  drop_na() %>%
  group_by(year, place) %>%
  summarise_at(vars(Male, Female), ~as.character(mean(times(.)))) %>%
  filter(place == "Gold") %>%
  mutate_at(vars(Male, Female), hms::parse_hms)

df_viz %>%
  pivot_longer(c(Male, Female), names_to = "gender", values_to = "time") %>%
  filter(year %in% c(1979, 2019)) %>%
  pivot_wider(names_from = year, values_from = time) %>%
  mutate(diff_time = difftime(`2019`, `1979`))

df_viz

p <- df_viz %>%
  ggplot(aes(x = Male, y = year, xend = Female)) +
  geom_dumbbell(
    size_x = 3,
    size_xend = 3,
    colour_x = "#39ccff",
    colour_xend = "#ff79fc",
    color = "white",
    size = 2
  ) +
  scale_y_reverse() +
  scale_x_time(breaks = hms::parse_hms(times("00:00:00") + (0:23)/24)) +
  xlab("Racing time") +
  ylab(NULL) +
  theme(
    plot.background = element_rect(fill = "#3c3c3c"),
    panel.background = element_rect(fill = "#3c3c3c"),
    text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    plot.subtitle = element_markdown(lineheight = 1.1),
    panel.grid = element_line(color = "gray50"),
    panel.border = element_rect(color = "gray50"),
    plot.caption = element_text(color = "gray50")
  ) +
  labs(
    title = str_wrap("Evolution of 40 years of ironman world championship racing time", 40),
    subtitle = str_wrap("Between 1979 and 2019, the racing time decreased steadily. <i style='color:#39ccff'>Men's</i> and <i style='color:#ff79fc'>women's</i> <br> racing time decreased by 3.4 and 4.3 hours respectively.", 60),
    caption = "MakeoverMonday 2019W42 | Data source: Wikipedia | Visualization: @philmassicotte"
  )

ggsave(
  here::here("graphs", "makeovermonday_2019w42.png"),
  device = "png",
  type = "cairo",
  height = 8,
  width = 7
)

