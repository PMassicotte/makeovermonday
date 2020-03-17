library(tidyverse)
library(httr)
library(readxl)
library(ggpmthemes)
library(ggtext)

theme_set(theme_exo())

GET(
  "https://query.data.world/s/qc5qabsvvouwhlkkk2cbv4yk55ked3",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

df <- read_excel(tf) %>%
  janitor::clean_names()

df_viz <- df %>%
  mutate(hours_deficit = hours_needed - hours_averaged) %>%
  mutate(grade = fct_reorder(grade, hours_needed)) %>%
  pivot_longer(-c(grade, hours_needed),
    names_to = "type",
    values_to = "hours"
  ) %>%
  mutate(type = factor(type, levels = c("hours_deficit", "hours_averaged"))) %>%
  mutate(label = paste(round(hours, digits = 2), "h"))

lab <- df_viz %>%
  filter(type == "hours_averaged") %>%
  group_by(grade) %>%
  summarise(percent_deficit = hours / hours_needed) %>%
  filter(percent_deficit == min(percent_deficit)) %>%
  mutate(label = glue::glue("{round(percent_deficit * 100, digits = 0)}%"))

lab

# Plot --------------------------------------------------------------------

df_viz %>%
  ggplot(aes(
    x = grade,
    y = hours,
    fill = type
  )) +
  geom_col() +
  geom_text(
    data = filter(df_viz, type == "hours_averaged"),
    aes(label = label),
    hjust = 1.3,
    color = "white",
    family = "Acme",
    size = 3
  ) +
  geom_text(
    data = filter(df_viz, type == "hours_deficit"),
    aes(y = hours_needed, label = label),
    hjust = 1.3,
    color = "white",
    family = "Acme",
    size = 3
  ) +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 20, by = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_manual(values = c(
    "hours_averaged" = "#238994",
    "hours_deficit" = "#942e23"
  )) +
  labs(
    x = NULL,
    title = "Average<span style = 'color:#238994;'> hours slept</span><br>and <span style = 'color:#942e23;'>hours deficit</span> by grade",
    subtitle = glue::glue(
      "The **{lab$grade}** is the group presenting the highest sleeping deficit with **{lab$label}**"),
      caption = "MakeoverMonday 2020W9\nVisualization: @philmassicotte\nData Source: https://savvysleeper.org/costing-kids-sleep/"
  ) +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "#293039"),
    panel.background = element_rect(fill = "#293039"),
    legend.background = element_rect(fill = "#293039"),
    legend.key = element_rect(color = "transparent", fill = "transparent"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_markdown(hjust = 0.5),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(
      hjust = 0.5,
      family = "Oswald",
      color = "gray75"
    ),
    plot.caption = element_text(size = 6, color = "gray75")
  )

df_viz

ggsave(
  "graphs/makeovermonday_2020w09.png",
  type = "cairo",
  dpi = 600,
  width = 7,
  height = 5
)
