library(tidyverse)
library(ggpmthemes)

theme_set(theme_light_modified(base_family = "Roboto Condensed"))

url <- "https://query.data.world/s/djyy4zmg6sgjtbqyau47k5pznw5k67"
url2 <- pins::pin(url)

df <- read_csv(url2)

df <- df %>%
  janitor::clean_names() %>%
  filter(taught == TRUE)

df %>%
  count(area, sort = TRUE)

df %>%
  count(gen_area)

# Plot --------------------------------------------------------------------

df_viz <- df %>%
  count(year, gen_area) %>%
  group_by(year) %>%
  mutate(n = n / sum(n))

df_viz %>%
  group_by(year) %>%
  summarise(total = sum(n))

df_viz %>%
  ggplot(aes(x = year, y = n, color = gen_area)) +
  geom_line() +
  facet_wrap(~gen_area) +
  labs(
    x = NULL,
    y = "Percent of classes taught",
    title = str_wrap(
      "Evolution of the classes taught in different areas at University of California between 1900 and 2011", 50
    ),
    caption = "MakeoverMonday 2020W12\nVisualization: @philmassicotte\nData Source: UC ClioMetric History Project"
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
  scale_y_continuous(labels = scales::label_percent()) +
  paletteer::scale_color_paletteer_d("RColorBrewer::Set2") +
  theme(
    legend.position = "none",
    strip.text.x = element_text(hjust = 0, size = 14, family = "Roboto Condensed Bold Italic"),
    strip.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.2),
    panel.spacing.y = unit(2, "lines"),
    axis.text = element_text(color = "gray50"),
    axis.title = element_text(color = "gray40"),
    plot.title.position = "plot",
    plot.title = element_text(
      hjust = 0.5,
      margin = margin(b = unit(10, "lines"))
    ),
    plot.caption = element_text(
      family = "Roboto Condensed Light",
      color = "gray50",
      size = 8
    )
  )

ggsave(
  "graphs/makeovermonday_2020w11.png",
  type = "cairo",
  dpi = 600,
  width = 7,
  height = 5
)
