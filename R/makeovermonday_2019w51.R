library(tidyverse)
library(ggpmthemes)
library(tidytext)
library(ggtext)
library(ggforce)

theme_set(theme_poppins())

df <-
  read_csv("https://query.data.world/s/zzslefolfv64ufd5m66nw724aa5tjw") %>%
  janitor::clean_names()

df

df %>%
  group_by(season) %>%
  top_n(5, def_rtg) %>%
  ungroup() %>%
  mutate(season = fct_inorder(season)) %>%
  mutate(team = reorder_within(team, def_rtg, season)) %>%
  ggplot(aes(x = team, y = def_rtg)) +
  geom_col() +
  facet_wrap(~season, scales = "free_y") +
  coord_flip() +
  scale_x_reordered()

df <- df %>%
  separate(season,
    into = c("season_start", "season_end"),
    convert = TRUE
  ) %>%
  mutate(season_end = season_start + 1) %>%
  mutate(season = glue::glue("{season_start}\n{season_end}")) %>%
  mutate(season = fct_inorder(season))

spurs <- df %>%
  filter(str_detect(team, pattern = regex("spurs", ignore_case = TRUE)))

nba <- df %>%
  group_by_at(vars(contains("season"))) %>%
  summarise(def_rtg = mean(def_rtg)) %>%
  mutate(team = "NBA")

# Plot --------------------------------------------------------------------

explaination <- tibble(
  desc = "The defensive rating is the number of points allowed per 100 possessions by a team. *Source: stats.nba.com/help/glossary/*",
  x = 1996,
  y = 90
)

subtitle <- tibble(
  desc = glue::glue(
    "<i style='color:#E8406C'>San Antonion Spurs'</i> defensive rating is currently the highest in its<br> history with **{spurs$def_rtg[spurs$season_start == 2019]} points**. It is **{spurs$def_rtg[spurs$season_start == 2019] - nba$def_rtg[spurs$season_start == 2019]} points above** the <i style='color:#4393C3FF'>NBA average</i>."
  ),
  x = 2004,
  y = 94
)

df %>%
  ggplot(aes(x = season_start, y = def_rtg, group = team)) +
  geom_line(
    color = "gray45",
    size = 0.1,
    lineend = "round"
  ) +
  geom_line(
    data = spurs,
    color = "#E8406C",
    size = 1,
    lineend = "round"
  ) +
  geom_line(
    data = nba,
    aes(x = season_start, y = def_rtg),
    color = "#4393C3FF",
    size = 1,
    lineend = "round",
    inherit.aes = FALSE
  ) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(n = 10),
    labels = function(x) {
      glue::glue("{x}\n{x + 1}")
    },
    limits = c(1996, 2019),
    expand = expand_scale(mult = c(0.06, 0.02))
  ) +
  scale_y_continuous(breaks = scales::breaks_pretty()) +
  xlab(NULL) +
  ylab("Defensive Rating (DEFRTG)") +
  labs(
    title = "San Antonio Spurs' defensive rating",
    caption = "MakeoverMonday 2019W49 | Data Source: Bureau of Labor Statistics | Visualization: @philmassicotte"
  ) +
  geom_richtext(
    data = explaination,
    aes(x = x, y = y, label = desc),
    inherit.aes = FALSE,
    label.color = NA,
    hjust = 0,
    family = "Exo",
    size = 2,
    fill = NA,
    color = "gray75"
  ) +
  geom_richtext(
    data = subtitle,
    aes(x = x, y = y, label = desc),
    inherit.aes = FALSE,
    label.color = NA,
    hjust = 0,
    family = "Exo",
    size = 3,
    fill = NA,
    color = "gray75"
  ) +
  geom_mark_circle(
    data = spurs,
    aes(
      label = team,
      filter = season_start == 2019,
      description = def_rtg
    ),
    label.fill = NA,
    label.colour = "#E8406C",
    con.colour = "#E8406C",
    expand = unit(1, "mm"),
    color = "#E8406C",
    label.buffer = unit(15, "mm"),
    label.fontsize = 8
  ) +
  geom_mark_circle(
    data = nba,
    aes(
      label = team,
      filter = season_start == 2019,
      description = def_rtg
    ),
    label.fill = NA,
    label.colour = "#4393C3FF",
    con.colour = "#4393C3FF",
    expand = unit(1, "mm"),
    color = "#4393C3FF",
    label.buffer = unit(30, "mm"),
    label.fontsize = 8
  ) +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#3c3c3c"),
    plot.background = element_rect(fill = "#3c3c3c"),
    text = element_text(color = "gray75"),
    axis.text = element_text(color = "gray75"),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, color = "gray90"),
    plot.caption = element_text(size = 6, color = "gray75")
  )

# Save plot ---------------------------------------------------------------

destfile <- here::here("graphs", "makeovermonday_2019w51.pdf")

ggsave(destfile,
  device = cairo_pdf,
  height = 5,
  width = 7
)

ggsave(
  here::here("graphs", "makeovermonday_2019w51.png"),
  device = "png",
  type = "cairo",
  dpi = 600,
  height = 5,
  width = 7
)
