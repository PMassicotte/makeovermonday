library(tidyverse)
library(ggpmthemes)
library(tidytext)
library(ggchicklet)

theme_set(theme_light_modified(base_family = "Exo"))

df <-
  read_csv("https://query.data.world/s/usukdxwok4d36cqxo74t6ftxwlrbpr") %>%
  janitor::clean_names() %>%
  select(-x9) %>%
  pivot_longer(-food_product) %>%
  group_by(food_product) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  mutate(food_product = fct_reorder(food_product, total)) %>%
  filter(dense_rank(desc(total)) <= 10) %>%
  group_by(name) %>%
  mutate(total2 = sum(value)) %>%
  ungroup() %>%
  mutate(name = fct_reorder(name, total2))

df %>%
  mutate(name = reorder_within(name, value, food_product)) %>%
  ggplot(aes(x = value, y = name)) +
  geom_col() +
  facet_wrap(~food_product, scales = "free_y") +
  scale_y_reordered()


# Plot --------------------------------------------------------------------

# https://mycolor.space/?hex=%2376C3E6&sub=1

p <- df %>%
  mutate(name = str_replace_all(name, "_", " ")) %>%
  mutate(name = str_to_title(name)) %>%
  mutate(name = fct_reorder(name, value, .fun = max, .desc = TRUE)) %>%
  mutate(food_product = reorder_within(food_product, value, name)) %>%
  ggplot(aes(y = value, x = food_product)) +
  coord_flip() +
  geom_chicklet(
    radius = grid::unit(2, "mm"),
    fill = "#3A8DAE",
    color = "#354A54",
    size = 0.35
  ) +
  facet_wrap(~name, scales = "free") +
  scale_x_reordered() +
  # geom_point(size = 7.5, color = "white") +
  # geom_point(size = 6.5, color = "gray60") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(
      family = "Baloo",
      hjust = 0,
      size = 20
    ),
    strip.background = element_blank(),
    plot.background = element_rect(fill = "#FCFCD4"),
    panel.background = element_rect(fill = "#FCFCD4"),
    text = element_text(color = "#954A00"),
    axis.text.y = element_text(color = "#954A00", face = "bold"),
    axis.text.x = element_text(color = "#3c3c3c")
  )

ggsave(
  "graphs/makeovermonday_2020w16.png",
  type = "cairo",
  dpi = 600,
  width = 15,
  height = 10
)
