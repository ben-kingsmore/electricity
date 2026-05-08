library(tidyverse)

agile_historic <- read_csv(
  "G:/My Drive/battery_calcs/csv_agile_C_London.csv",
  col_names = c("time.cet", "time.uk", "area.code", "area.name", "price.unit")
)


agile_historic |>
  mutate(year = year(time.cet)) |>
  ggplot(aes(x = price.unit)) +
  geom_histogram() +
  geom_vline(xintercept = 25, colour = "red") +
  facet_wrap(vars(year), scales = "free_y")


agile_historic |>
  mutate(year = year(time.cet)) |>
  filter(year >= 2024) |>
  summarise(
    mean = mean(price.unit),
    pct.10 = quantile(price.unit, 0.1),
    pct.25 = quantile(price.unit, 0.25),
    pct.75 = quantile(price.unit, 0.75),
    pct.90 = quantile(price.unit, 0.9),
    .by = time.uk
  ) |>
  ggplot(aes(x = time.uk)) +
  geom_ribbon(
    aes(ymin = pct.10, ymax = pct.90),
    fill = "dodgerblue",
    alpha = 0.5
  ) +
  geom_ribbon(
    aes(ymin = pct.25, ymax = pct.75),
    fill = "dodgerblue4",
    alpha = 0.5
  ) +
  geom_line(aes(y = mean)) +
  theme(text = element_text(size = 16)) +
  xlab("Time of day") +
  ylab("Pence per kWh")
