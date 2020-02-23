library(tidyverse)
library(httr)
library(readxl)
library(ggpmthemes)
library(ggforce)

theme_set(theme_poppins())

GET(
  "https://query.data.world/s/wscte7ftrehwdhdsfcktkvgktpvwuu",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

df <- read_excel(tf) %>%
  janitor::clean_names()

df

labels <- df %>%
  filter(percent_of_your_life_the_us_has_been_at_war == 1) %>%
  filter(birth_year == min(birth_year)) %>%
  mutate(description = sprintf("If you are born after %s, the US has been always at war.", birth_year))

# Make more data points so the line color looks smoother
df_viz <- df %>%
  group_nest() %>%
  mutate(af = map(data, approxfun)) %>%
  mutate(new_data = map(af, function(af) {
    tibble(
      birth_year = seq(1905, 2019, length.out = 1000),
      percent_of_your_life_the_us_has_been_at_war = af(birth_year)
    )
  })) %>%
  unnest(new_data)

df_viz %>%
  ggplot(
    aes(
      x = birth_year,
      y = percent_of_your_life_the_us_has_been_at_war,
      color = percent_of_your_life_the_us_has_been_at_war
    )
  ) +
  geom_path(size = 2, lineend = "round") +
  # geom_point() +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = seq(1900, 2020, by = 15)) +
  labs(
    y = "Percentage of your life the US has been at war",
    x = "Birth year",
    title = "How much of your life\n the US has been at war?",
    caption = "MakeoverMonday 2020W6\nVisualization: @philmassicotte\nData Source: Washington Post (shorturl.at/jGMSY)"
  ) +
  scale_color_viridis_c(option = "plasma") +
  geom_mark_hull(
    data = labels,
    aes(description = description),
    con.colour = "white",
    expand = unit(3, "mm"),
    label.colour = "white",
    label.fill = "transparent",
    label.fontsize = 10,
    con.size = 0.25,
    label.family = "Londrina Solid Light",
    label.hjust = 0,
    label.buffer = unit(5, "mm")
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = "#2E3440"),
    plot.background = element_rect(fill = "#2E3440"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(
      size = 0.1,
      color = "gray50",
      linetype = "dashed"
    ),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(hjust = 0.5, family = "Mitr"),
    plot.caption = element_text(size = 6, color = "gray75"),
    plot.title.position = "plot"
  )

destfile <- here::here("graphs", "makeovermonday_2020w06.pdf")
ggsave(destfile,
  device = cairo_pdf,
  width = 5.45,
  height = 4.68
)

destfile <- here::here("graphs", "makeovermonday_2020w06.png")
ggsave(destfile,
       type = "cairo",
       dpi = 600,
       width = 5.45,
       height = 4.68
)
