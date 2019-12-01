library(tidyverse)
library(ggpmthemes)
library(httr)
library(readxl)
library(ggtext)

theme_set(theme_maven(base_family = "Paytone One"))

GET(
  "https://query.data.world/s/n27qgd4hbhmzgwihn7q2lj5lf4skjf",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)
df <- read_excel(tf) %>%
  janitor::clean_names()

df

df %>%
  count(union, industry, occupation, sex, race, ethnic_origin, age)

df_viz <- df %>%
  filter(sex %in% c("Men", "Women") &
    union %in% c("Members of unions", "Non-union"))

# Plot --------------------------------------------------------------------

subtitle <- "Although there are benefits to be part of a union, there is still a **clear difference between**<br>*men* and *women* weekly earning."

df_viz %>%
  ggplot(aes(x = year, y = median_usual_weekly_earnings)) +
  geom_line(aes(color = union), size = 2) +
  xlab(NULL) +
  ylab("Median usual weekly earning") +
  scale_y_continuous(labels = scales::label_dollar()) +
  facet_wrap(~sex) +
  paletteer::scale_color_paletteer_d(
    ggsci,
    alternating_igv,
    guide = guide_legend(
      label.position = "bottom",
      keywidth = unit(6, "cm"),
      override.aes = list(size = 4)
    )
  ) +
  labs(
    title = str_wrap("It pays off to be part of a union in the USA", 30),
    subtitle = subtitle,
    caption = "MakeoverMonday 2019W49 | Data Source: Bureau of Labor Statistics | Visualization: @philmassicotte"
  ) +
  theme(
    legend.position = "bottom",
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    plot.background = element_rect(fill = "#3c3c3c"),
    panel.background = element_rect(fill = "#3c3c3c"),
    legend.background = element_rect(fill = "#3c3c3c"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    strip.background = element_blank(),
    strip.text = element_text(
      color = "gray75",
      size = 20,
      face = "bold"
    ),
    panel.grid = element_line(size = 0.1, color = "gray50"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 28, face = "bold"),
    plot.caption = element_text(
      hjust = 1,
      size = 8,
      color = "gray75",
      family = "Maven Pro"
    ),
    plot.subtitle = element_markdown(
      size = 12,
      color = "gray75",
      family = "Maven Pro"
    )
  )

ggsave(
  here::here("graphs", "makeovermonday_2019w49.png"),
  device = "png",
  type = "cairo",
  height = 5,
  width = 8
)

