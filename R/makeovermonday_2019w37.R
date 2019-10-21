library(tidyverse)
library(tidytext)
library(glue)
library(ggpmthemes)

theme_set(theme_exo())
theme_update(
  rect = element_rect(fill = "#173f50"),
  text = element_text(colour = "white"),
  axis.text = element_text(colour = "white"),
  axis.ticks = element_line(colour = "white")
)

df <- read_csv(url("https://query.data.world/s/37mfbbbovsgbk3gg3ngckdrjdwo3ai"), col_types = cols(
  `Material Type` = col_character(),
  Title = col_character(),
  Series = col_character(),
  `Checkout Year` = col_double(),
  `Checkout Month` = col_double(),
  Checkouts = col_double(),
  Subjects = col_character(),
  Publisher = col_character(),
  `Publication Year` = col_character()
)) %>%
  janitor::clean_names() %>%
  mutate(publication_year = str_sub(publication_year, 1, 6)) %>%
  mutate(publication_year = parse_number(publication_year))

df %>%
  distinct(series)

df %>%
  count(checkout_year)

df %>%
  drop_na(series) %>%
  mutate(title = glue("{title} ({publication_year})")) %>%
  group_by(series, title) %>%
  summarise(total_checkout = sum(checkouts)) %>%
  top_n(10, total_checkout) %>%
  ungroup() %>%
  add_count(series) %>%
  filter(n >= 10) %>%
  mutate(title = reorder_within(title, total_checkout, within = series)) %>%
  mutate(series = fct_reorder(series, -total_checkout, .fun = sum)) %>%
  ggplot(aes(x = title, y = total_checkout)) +
  geom_col(fill = paletteer::paletteer_d(yarrr, google)[3]) +
  coord_flip() +
  facet_wrap(~series, scales = "free_y", ncol = 2) +
  scale_x_reordered() +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.2))) +
  ylab("Number of checkouts") +
  xlab(NULL) +
  theme(
    panel.background = element_rect(fill = "#173f50"),
    strip.background = element_rect(fill = "#173f50"),
    strip.text = element_text(size = 18, color = "white", face = 2),
    legend.key = element_rect(fill = "#173f50"),
    panel.grid = element_blank(),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 10, color = "gray75"),
    panel.border = element_blank()
  ) +
  labs(
    title = "James Patterson Book Checkouts at Seattle Libraries",
    subtitle = "Showing the total number of checkouts for the book series containing at least 10 books.",
    caption = "Makeover Monday (2019/W37) | Data Source: Seattle Open Data | Visualization: @philmassicotte"
  )

ggsave(
  here::here("graphs/makeovermonday_2019w37.png"),
  dpi = 300,
  device = "png",
  type = "cairo",
  width = 12, height = 8
)

df %>%
  group_by(title) %>%
  mutate(total_checkouts = sum(checkouts)) %>%
  ungroup() %>%
  filter(dense_rank(desc(total_checkouts)) <= 10) %>%
  group_by(title, checkout_year) %>%
  summarise(total_checkouts = sum(checkouts)) %>%
  ungroup() %>%
  ggplot(aes(x = checkout_year, y = total_checkouts, color = title)) +
  geom_line()
