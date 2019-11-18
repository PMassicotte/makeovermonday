library(tidyverse)
library(httr)
library(readxl)
library(ggpmthemes)

theme_set(theme_maven())

# Get the data ------------------------------------------------------------

GET("https://query.data.world/s/pou3ohuohfa6q7yspsbnsst3vzgrof", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf) %>%
  janitor::clean_names()

# Explore -----------------------------------------------------------------

df %>%
  ggplot(aes(x = age, y = ownership, fill = factor(year))) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_x_continuous(breaks = unique(df$age), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent, expand = expand_scale(mult = c(0, 0.2))) +
  xlab("Age of owners") +
  ylab("Proportion of ownership") +
  paletteer::scale_fill_paletteer_d(wesanderson, Chevalier1) +
  labs(
    title = str_wrap("Proportion of smartphone ownership between 2015 and 2019 among youth", 45),
    caption = "MakeoverMonday 2019W47 | Data Source: https://data.world/makeovermonday/2019w47 | Visualization: @philmassicotte"
  ) +
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.9, 0.1),
    legend.title = element_blank(),
    plot.background = element_rect(fill = "#3c3c3c"),
    panel.background = element_rect(fill = "#3c3c3c"),
    text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.caption = element_text(color = "gray75", size = 6),
    axis.ticks = element_blank(),
    legend.background = element_rect(fill = "#3c3c3c"),
    legend.key = element_blank(),
    plot.title = element_text(size = 14),
    legend.direction = "horizontal"
  ) +
  guides(fill = guide_legend(label.position = "top"))

ggsave(
  here::here("graphs", "makeovermonday_2019w47.png"),
  device = "png",
  type = "cairo",
  dpi = 600
)

