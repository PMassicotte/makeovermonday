library(tidyverse)
library(ggpmthemes)
library(httr)
library(readxl)
library(tidytext)
library(patchwork) # devtools::install_github("thomasp85/patchwork#106")

theme_set(theme_exo2(base_size = 12))

GET("https://query.data.world/s/grfxn7os5xwnxmr2n43tagk2vnroie", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf)

df <- df %>%
  pivot_longer(
    -c("Continent", "Country", "City", "Year"),
    names_to = "month", values_to = "sunshine_duration"
  ) %>%
  janitor::clean_names() %>%
  select(-year) %>%
  distinct()

df

df %>%
  count(continent, country, sort = TRUE)

p1 <- df %>%
  group_by(continent, country, city) %>%
  mutate(yearly_sunshine_duration = sum(sunshine_duration)) %>%
  ungroup() %>%
  mutate(month = fct_relevel(month, month.abb)) %>%
  mutate(continent = str_replace_all(continent, " ", "\n")) %>%
  ggplot(aes(x = month, y = sunshine_duration, group = city, color = yearly_sunshine_duration)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~continent, ncol = 1) +
  paletteer::scale_color_paletteer_c(pals, ocean.oxy) +
  ylab("Number of hours of sunshine per month") +
  xlab(NULL) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(position = "left") +
  theme(
    plot.background = element_rect(fill = "#3c3c3c"),
    panel.background = element_rect(fill = "#3c3c3c"),
    legend.background = element_rect(fill = "#3c3c3c"),
    legend.key = element_rect(color = NA, fill = NA),
    text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.caption = element_text(color = "gray75"),
    strip.background = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )

p2 <- df %>%
  group_by(continent, country, city) %>%
  summarise(yearly_sunshine_duration = sum(sunshine_duration)) %>%
  ungroup() %>%
  group_by(continent) %>%
  top_n(5, yearly_sunshine_duration) %>%
  ungroup() %>%
  mutate(city = reorder_within(city, yearly_sunshine_duration, continent, max)) %>%
  mutate(continent = str_replace_all(continent, " ", "\n")) %>%
  ggplot(aes(x = city, y = yearly_sunshine_duration)) +
  geom_col(aes(fill = yearly_sunshine_duration)) +
  facet_wrap(~continent, scales = "free_y", ncol = 1, strip.position = "right") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0, 0)) +
  ylab("Total hours of sunshine per year") +
  xlab(NULL) +
  paletteer::scale_fill_paletteer_c(pals, ocean.oxy) +
  theme(
    plot.background = element_rect(fill = "#3c3c3c"),
    panel.background = element_rect(fill = "#3c3c3c"),
    legend.position = "none",
    text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.caption = element_text(color = "gray75"),
    strip.background = element_rect(fill = "#3c3c3c"),
    strip.text = element_text(color = "white", size = 14, face = "bold"),
    axis.ticks = element_blank()
  )

p <- p1 + p2 +
  plot_theme(background = "#3c3c3c", padding = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  plot_annotation(
    title = str_wrap("World Cities Ranked by Average Annual Sunshine Hours", 100),
    subtitle = str_wrap("Left: Number of hours of sunshine per month per city. Right: Top five cities with the most number of sunshine hours per continent.", 100),
    caption = "MakeoverMonday 2019W44 | Data Source: Wikipedia | Visualization: @philmassicotte",
    theme = theme(
      plot.title = element_text(color = "white"),
      plot.subtitle = element_text(color = "white"),
      plot.caption = element_text(color = "gray75")
    )
  )

ggsave(
  here::here("graphs", "makeovermonday_2019w44.png"),
  device = "png",
  type = "cairo",
  height = 8,
  width = 9
)
