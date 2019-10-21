library(tidyverse)
library(ggpmthemes)
library(ggchicklet)
library(httr)
library(readxl)

theme_set(theme_exo())

theme_update(
  rect = element_rect(fill = "#173f50"),
  text = element_text(colour = "white"),
  axis.text = element_text(colour = "white"),
  axis.ticks = element_line(colour = "white")
)

GET("https://query.data.world/s/43baodomwmwjakyfzhysyo4avzsxog", write_disk(tf <- tempfile(fileext = ".xlsx")))

video_game_revenus <- read_excel(tf) %>%
  janitor::clean_names()

video_game_revenus <- video_game_revenus %>%
  mutate(platform = fct_relevel(platform, "Console Games", "Mobile Games", "PC Games")) %>%
  arrange(year, platform) %>%
  group_by(year) %>%
  mutate(cum_percent = cumsum(percent_of_revenue)) %>%
  mutate(abs_percent = cum_percent - percent_of_revenue)


video_game_revenus %>%
  ggplot(aes(x = year, y = percent_of_revenue, fill = platform)) +
  geom_chicklet(
    width = 0.75,
    radius = unit(0.1, "cm"),
    size = 0.25
  ) +
  geom_text(
    aes(
      y = abs_percent,
      label = scales::percent(percent_of_revenue, accuracy = 1)
    ),
    vjust = -1,
    color = "white",
    family = "Exo",
    size = 2.5
  ) +
  scale_x_continuous(breaks = seq(1990, 2030, by = 1)) +
  scale_y_continuous(labels = scales::percent) +
  xlab(NULL) +
  paletteer::scale_fill_paletteer_d(ghibli, MononokeLight) +
  # paletteer::scale_fill_paletteer_d(nord, aurora) +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "#173f50"),
    legend.key = element_rect(fill = "#173f50"),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 8, color = "gray75")
  ) +
  labs(
    title = "The past and the future of gaming",
    subtitle = str_wrap(
      "In 2018, mobile games represented more than 50% of the total gaming revenues. It is expected to reach 59% by 2021. The console and PC games both decreased.",
      100
    ),
    caption = "Makeover Monday (2019/W35) | Data Source: Statista | Visualization: @philmassicotte"
  )

ggsave(here::here("graphs/", "makeovermonday_2019w35.png"),
  dpi = 600,
  dev = "png",
  width = 6, type = "cairo"
)
