library(tidyverse)

library(httr)
GET("https://query.data.world/s/u66fdcph5274egcd4qma3nwroh6rkb", write_disk(tf <- tempfile(fileext = ".xlsx")))

df <- readxl::read_excel(tf) %>%
  janitor::clean_names()

df

skimr::skim(df)

df %>%
  count(format)

df %>%
  count(metric)

df %>%
  filter(str_detect(metric, "Adjusted")) %>%
  group_by(year) %>%
  summarise(total = sum(value_actual, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year >= 1977) %>%
  ggplot(aes(x = year, y = total)) +
  geom_area()
