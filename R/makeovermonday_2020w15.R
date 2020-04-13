library(tidyverse)
library(httr)
library(readxl)
library(ggpmthemes)
library(ggtext)

# TODO: Fonts and remove the legend and use colors in the titles

theme_set(theme_light_modified(base_family = "Roboto Condensed"))

GET(
  "https://query.data.world/s/mxoyyowr25xcrglaiqsc66p6op3goi",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

df <- read_excel(tf) %>%
  janitor::clean_names()

df

df_viz <- df %>%
  arrange(season) %>%
  mutate(season_start = str_extract(season, "\\d{4}")) %>%
  mutate(season_start = parse_number(season_start)) %>%
  mutate(season = fct_reorder(season, season_start))

df_bar <- df_viz %>%
  select(season, player, liga_goals) %>%
  pivot_wider(names_from = player, values_from = liga_goals) %>%
  janitor::clean_names()

df_viz %>%
  ggplot(aes(x = season, y = liga_goals, color = player)) +
  geom_segment(
    data = df_bar,
    aes(
      x = season,
      xend = season,
      y = messi,
      yend = ronaldo
    ),
    inherit.aes = FALSE,
    size = 2,
    color = "#FFF8EC"
  ) +
  geom_point(size = 9, color = "#FFF8EC") +
  geom_point(size = 8) +
  geom_text(
    aes(label = liga_goals),
    size = 3.5,
    color = "#504538",
    family = "FIFA Welcome"
  ) +
  labs(
    y = NULL,
    x = NULL,
    title = "<span style = 'color:#FFAE00'>Messi </span><span style = 'font-size:18pt; color:#FFF8EC'>vs </span><span style = 'color:#C25450'>Ronaldo</span>",
    caption = "MakeoverMonday 2020W15\nVisualization: @philmassicotte\nData Source: Data Source: www.transfermarkt.com",
    subtitle = glue::glue("In *La Liga football league* between 2009 and 2018, <span style = 'color:#FFAE00'>**Messi**</span> and <span style = 'color:#C25450'>**Ronaldo**</span> scored respectively <span style = 'color:#FFAE00'>**{sum(df_bar$messi)}**</span> and <span style = 'color:#C25450'>**{sum(df_bar$ronaldo)}**</span> goals.<br>In their best season, <span style = 'color:#FFAE00'>**Messi**</span> scored <span style = 'color:#FFAE00'>**{max(df_bar$messi)}**</span> goals whereas <span style = 'color:#C25450'>**Ronaldo**</span> scored <span style = 'color:#C25450'>**{max(df_bar$ronaldo)}**</span> goals.")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "#4E8397"),
    panel.background = element_rect(fill = "#4E8397"),
    legend.background = element_rect(fill = "#4E8397"),
    legend.position = "none",
    axis.text.y = element_blank(),
    text = element_text(color = "#FFF8EC"),
    plot.title = element_markdown(
      family = "FIFA Welcome",
      size = 28,
      hjust = 0.5
    ),
    plot.subtitle = element_markdown(
      family = "UnGraphic",
      size = 10,
      color = "#FFF8EC",
      hjust = 0,
      lineheight = 1.5
    ),
    plot.title.position = "plot",
    plot.caption = element_text(
      family = "Roboto Condensed Light",
      color = "#FFF8EC",
      size = 6
    ),
    axis.text = element_text(
      color = "#FFF8EC",
      family = "FIFA Welcome",
      face = "bold"
    ),
    axis.title = element_text(
      color = "#FFF8EC",
      family = "FIFA Welcome",
      face = "bold"
    )
  ) +
  scale_color_manual(
    values = c(
      "Messi" = "#FFAE00",
      "Ronaldo" = "#C25450"
    ),
    guide = guide_legend(label.position = "top")
  )

ggsave(
  here::here("graphs", "makeovermonday_2020w15.png"),
  type = "cairo",
  dpi = 600,
  width = 7.2,
  height = 5.79
)
