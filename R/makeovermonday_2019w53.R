library(tidyverse)
library(tidytext)
library(ggpmthemes)
library(glue)

theme_set(theme_exo2())

df <-
  read_csv("https://query.data.world/s/l75aum4chp6stjxxcn6txx2dw4jo6w") %>%
  janitor::clean_names() %>%
  mutate(date = as.Date(date, "%d/%m/%Y")) %>%
  mutate(year = lubridate::year(date))

df

df %>%
  filter(lubridate::year(date) >= 2018) %>%
  ggplot(aes(x = date, y = downloads)) +
  geom_line() +
  geom_point()

words <- df %>%
  unnest_tokens("word", "subject") %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

df %>%
  group_by(year = lubridate::year(date)) %>%
  summarise(n = sum(downloads)) %>%
  ggplot(aes(x = year, y = n)) +
  geom_col(fill = "#ABB7B7") +
  coord_flip()

df_viz <- words %>%
  filter(str_detect(word, "\\d{4}", negate = TRUE)) %>%
  # group_by(year) %>%
  # top_n(16, n) %>%
  filter(n >= 2) %>%
  # mutate(word = str_to_title(word)) %>%
  mutate(word = fct_reorder(word, n))

subtitle <-
  glue("There were a total of {nrow(df)} challenges between 2016 and 2019.")

df_viz %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "#ABB7B7") +
  coord_flip() +
  geom_text(
    aes(label = n),
    hjust = 1.2,
    color = "#3c3c3c",
    size = 3,
    fontface = "bold"
  ) +
  xlab(NULL) +
  scale_y_continuous("Frequency",
    breaks = seq(0, 10, by = 1),
    expand = expand_scale(mult = c(0, 0.02))
  ) +
  labs(title = str_wrap(
    "Most frequent words of #MakeoverMonday topics between 2016 and 2019",
    50
  ),
  caption = "MakeoverMonday 2019W53 | Data Source: data.world | Visualization: @philmassicotte") +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      family = "Paytone One",
      face = "bold"
    ),
    plot.background = element_rect(fill = "#3c3c3c", colour = NA),
    panel.background = element_rect(fill = "#3c3c3c"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white", face = "bold"),
    panel.grid = element_blank(),
    axis.title = element_text(color = "white", face = "bold"),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.caption = element_text(color = "gray75")
  )

ggsave(
  here::here("graphs", "makeovermonday_2019w53.png"),
  device = "png",
  type = "cairo",
  dpi = 600,
  height = 9,
  width = 9
)
