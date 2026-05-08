library(tidyverse)
source("functions.R")

consumption <- update_octopus_data()


consumption |>
  mutate(date = date(interval_start)) |>
  summarise(
    consumption = sum(consumption, na.rm = TRUE),
    .by = c(fuel, date)
  ) |>
  # filter(fuel == "gas") |>
  ggplot(aes(x = date, y = consumption, colour = fuel)) +
  geom_line()
