library(tidyverse)
library(ggpmthemes)
library(ggforce)

theme_set(theme_light_modified(base_family = "Poppins"))

url <- pins::pin("https://www.ons.gov.uk/generator?format=csv&uri=/peoplepopulationandcommunity/leisureandtourism/timeseries/gmax/ott")

df <- read_csv(url, skip = 8, col_names = c("year", "visits"))

df <- df %>%
  filter(str_detect(year, "Q")) %>%
  separate(year, into = c("year", "quarter"), convert = TRUE)

df %>%
  ggplot(aes(x = year, y = visits, color = quarter)) +
  geom_line() +
  geom_point()

df %>%
  arrange(year, quarter) %>%
  group_by(year) %>%
  mutate(cum_visits = cumsum(visits)) %>%
  ggplot(aes(x = year, y = cum_visits, color = quarter)) +
  geom_line() +
  geom_point()

df %>%
  group_by(year) %>%
  summarise(total_visits = sum(visits) * 1000)


# test --------------------------------------------------------------------

p <- df %>%
  # filter(year %in% c(2011, 2012)) %>%
  mutate(quarter = parse_number(quarter)) %>%
  # group_by(year) %>%
  # mutate(id = c(0, 3, 6, 9)) %>%
  # complete(year, id = seq(0, 0.9, by = 0.1), fill = list(visits = 1000)) %>%
  # fill(quarter) %>%
  ggplot(aes(x0 = quarter, y0 = year, fill = visits)) +
  geom_ellipse(aes(a = 1/3, b = 1/3, angle = 0, m2 = 6, m1 = 1), size = 0.1) +
  coord_fixed(ratio = 1/5) +
  scale_fill_viridis_b(option = "C", n.breaks = 10) +
  scale_x_continuous(labels = function(x) paste0("Q", x)) +
  scale_y_continuous(breaks = seq(1900, 2020), expand = expansion(mult = c(0, 0))) +
  labs(
    x = NULL,
    y = NULL,
    title = "Sfsdf"
  ) +
  theme(
    plot.margin = margin(t = 10),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    axis.text = element_text(size = 6, family = "Roboto"),
    legend.text = element_text(size = 2),
    legend.title = element_blank(),
    plot.title = element_text(size = 10, family = "Exo")
  )

ggsave(
  "~/Desktop/test.pdf",
  device = cairo_pdf
)

knitr::plot_crop("~/Desktop/test.pdf")
