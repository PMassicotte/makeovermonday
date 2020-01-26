library(tidyverse)
library(httr)
library(readxl)
library(ggpmthemes)

theme_set(theme_exo())

GET(
  "https://query.data.world/s/mb2km4dczipjlrbeponimyuee5lqay",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)
df <- read_excel(tf)

df <- df %>%
  pivot_longer(-1,
    names_to = "year",
    values_to = "free_sugars_intake_of_total_energy"
  ) %>%
  rename(age_group = 1) %>%
  mutate(free_sugars_intake_of_total_energy = free_sugars_intake_of_total_energy / 100) %>%
  mutate(year = str_remove_all(year, "\\(|\\)| ")) %>%
  mutate(age = str_match(age_group, "(\\d+.*)\\s+")[, 2])

df %>%
  mutate(age = fct_inorder(age)) %>%
  ggplot(aes(x = age, y = free_sugars_intake_of_total_energy, fill = year)) +
  geom_col(position = position_dodge()) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_percent()) +
  facet_wrap(~year)

df %>%
  mutate(year = fct_inorder(year)) %>%
  ggplot(
    aes(
      x = year,
      y = free_sugars_intake_of_total_energy,
      color = age_group,
      group = age_group
    )
  ) +
  geom_line()

df %>%
  extract(age_group, into = c("type", "age"), regex = "(\\S+)(.*)") %>%
  filter(type %in% c("Adults", "Children")) %>%
  mutate(year = str_replace_all(year, "/.*/", "-")) %>%
  ggplot(aes(x = year, y = age, fill = free_sugars_intake_of_total_energy)) +
  geom_tile(color = "#232C33", size = 0.5) +
  geom_text(aes(label = glue::glue(
    "{free_sugars_intake_of_total_energy * 100}%"
  ))) +
  facet_wrap(~type, scales = "free") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Blue",
    label = scales::label_percent(),
    breaks = scales::breaks_pretty(n = 8),
    limits = c(0.1, 0.18),
    oob = scales::squish,
    guide = guide_legend(
      nrow = 1,
      title.position = "top",
      label.position = "bottom",
      keywidth = unit(1, "cm"),
      keyheight = unit(0.25, "cm")
    )
  ) +
  labs(
    title = "Percentage of total calories from free sugars",
    subtitle = str_wrap(
      "Intakes of free sugars should not be more than 5% of total energy. None of the age groups reported by the British National Diet and Nutrition Survey meet this recommendation.",
      75
    ),
    caption = "MakeoverMonday 2020W3\nVisualization: @philmassicotte\nData Source: https://www.nutrition.org.uk/nutritioninthenews/new-reports/ndnsyears7and8.html"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    aspect.ratio = 1,
    strip.background = element_blank(),
    strip.text = element_text(
      family = "Maven Pro",
      face = "bold",
      size = 16,
      color = "white"
    ),
    plot.background = element_rect(fill = "#232C33"),
    panel.background = element_rect(fill = "#232C33"),
    legend.background = element_rect(fill = "#232C33"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    legend.key = element_rect(fill = NA),
    plot.title = element_text(
      family = "Gugi",
      size = 18,
      hjust = 0.5
    ),
    plot.subtitle = element_text(family = "Handlee", hjust = 0.5),
    plot.caption = element_text(size = 8, color = "gray75")
  )

destfile <- here::here("graphs", "makeovermonday_2020w03.pdf")

ggsave(destfile,
  device = cairo_pdf,
  height = 7,
  width = 7
)

knitr::plot_crop(destfile)

bitmap <- pdftools::pdf_render_page(destfile, dpi = 600)
destfile <- here::here("graphs", "makeovermonday_2020w03.png")
png::writePNG(bitmap, destfile)
