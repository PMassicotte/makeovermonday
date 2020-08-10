library(tidyverse)
library(ggforce)
library(ggfittext)
library(ggpattern)
library(ggtext)
library(httr)
library(readxl)

# circleFun <- function(center = c(0, 0), diameter = 1, npoints = 100, start = 0, end = 2) {
#   tt <- seq(start * pi, end * pi, length.out = npoints)
#   tibble(
#     x = center[1] + diameter / 2 * cos(tt),
#     y = center[2] + diameter / 2 * sin(tt)
#   )
# }
#
# c1 <- circleFun(center = c(1, 1), npoints = 1000, diameter = 2) %>%
#   as_tibble() %>%
#   mutate(
#     colour1 = "black",
#     colour2 = NA,
#     orient = "radial"
#   )

GET(
  "https://query.data.world/s/qxplwqhb32ds5dshjrljxjrupjxtkd",
  write_disk(tf <- tempfile(fileext = ".xlsx"))
)

df <- read_excel(tf) %>%
  janitor::clean_names()

df

colors <- c(
  "#6FAFAE",
  "#5D5E60",
  "#C7C6B4",
  "#E3C9A6",
  "#DD7873",
  "#956F5D"
)

p <- df %>%
  mutate(benefit_of_remote_work = fct_reorder(benefit_of_remote_work, percentage)) %>%
  ggplot(aes(label = glue::glue(
    "{benefit_of_remote_work} ({percentage}%)"
  ))) +
  # geom_polygon_pattern(
  #   data = c1,
  #   aes(
  #     x = x, y = y,
  #     pattern_fill = I(colour1),
  #     pattern_fill2 = I(colour2),
  #     pattern_orientation = I(orient)
  #   ),
  #   pattern = "gradient",
  #   colour = NA,
  #   pattern_density = 0.1,
  #   fill = NA,
  #   inherit.aes = FALSE
  # ) +
  geom_link(
    aes(
      x = benefit_of_remote_work,
      y = 0,
      xend = benefit_of_remote_work,
      yend = percentage,
      color = benefit_of_remote_work,
      alpha = -..index..
    ),
    size = 30,
    lineend = "round",
    n = 500
  ) +
  geom_bar_text(
    aes(x = benefit_of_remote_work, y = percentage),
    # angle = 90,
    position = "dodge",
    grow = TRUE,
    reflow = FALSE,
    place = "left",
    color = "white",
    family = "Oxanium",
    fontface = "bold"
  ) +
  scale_y_continuous(expand = expansion(c(0, 0.1))) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_color_manual(values = colors) +
  theme(
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.border = element_blank(),
    legend.position = "none",
    plot.title = element_text(
      color = "#3c3c3c",
      size = 50,
      family = "Nova Flat",
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 25, t = 15)
    ),
    panel.background = element_rect(fill = "#fffeea", color = NA),
    plot.background = element_rect(fill = "#fffeea", colour = NA),
    plot.caption = element_text(color = "gray60"),
    plot.subtitle = element_markdown(
      color = "gray40",
      family = "NRT",
      margin = margin(b = 25)
    )
  ) +
  coord_flip() +
  labs(
    title = "Benefits of Remote Work",
    caption = "MakeoverMonday 2020W32\nData Source: https://lp.buffer.com/state-of-remote-work-2020\n@philmassicotte",
    subtitle = "These results are based on a survey completed by over **3500 remote workers** from around the world that shared their<br>experiences and feelings about being a remote worker. *Note: The data was collected prior to the COVID-19 pandemic.*"
  )

struggle <- tibble(
  label = c(
    "Collaboration and communication",
    "Loneliness",
    "Not being able to unplug",
    "Distraction at home",
    "Being in a different time zone than teammates",
    "Staying motivated",
    "Taking vacation time",
    "Findind reliable WiFi",
    "Other"
  ),
  percentage = c(0.2, 0.2, 0.18, 0.12, 0.1, 0.07, 0.05, 0.03, 0.05)
)

p2 <- ggplotGrob(
  struggle %>%
    mutate(label = fct_reorder(label, percentage)) %>%
    arrange(desc(percentage)) %>%
    mutate(lab.ypos = cumsum(percentage) - 0.5 * percentage) %>%
    ggplot(aes(
      x = 2, y = percentage, fill = label
    )) +
    geom_bar(
      width = 1,
      stat = "identity",
      color = "white"
    ) +
    coord_polar(theta = "y", start = 0) +
    xlim(-0.5, 2.5) +
    geom_text(
      aes(y = lab.ypos, label = glue::glue("{percentage * 100}%")),
      color = "#3c3c3c",
      fontface = "bold",
      family = "Oxanium",
      size = 2.5
    ) +
    theme_void() +
    scale_fill_manual(
      values = paletteer::paletteer_c("ggthemes::Classic Area Red", n = 9),
      guide = guide_legend(
        reverse = TRUE,
        title = NULL,
        ncol = 1
      )
    ) +
    theme(
      legend.text = element_text(size = 8, color = "gray40"),
      legend.key.size = unit(0.5, "cm")
    ) +
    annotate(
      "text",
      x = -0.5,
      y = 1,
      label = str_wrap("There are also some drawbacks to working at home!", 20),
      color = "gray40",
      hjust = 0.5,
      vjust = 0.5
    )
)

p +
  annotation_custom(
    grob = p2,
    xmin = 0,
    xmax = 4,
    ymin = 15,
    ymax = 35
  )

ggsave(
  here::here("graphs/makeovermonday_2020w32.pdf"),
  device = cairo_pdf,
  width = 10,
  height = 8
)

pdftools::pdf_convert(
  pdf = here::here("graphs/makeovermonday_2020w32.pdf"),
  format = "png",
  filenames = here::here("graphs/makeovermonday_2020w32.png"),
  dpi = 600
)
