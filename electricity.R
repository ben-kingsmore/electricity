library(tidyverse)

usage <- read_csv("C:/Users/benki/Desktop/battery_calcs/electricity_usage.csv")
names(usage) <- c("consumption", "cost", "from", "to")

agile_prices <- read_csv(
  "C:/Users/benki/Desktop/battery_calcs/agile-half-hour-actual-rates-01-01-2024_25-09-2025.csv"
)
names(agile_prices) <- c("from", "to", "import", "export")

agile_prices <- agile_prices %>%
  mutate(
    from = dmy_hm(from),
    to = dmy_hm(to),
    import = as.numeric(import),
    export = as.numeric(export)
  )


df <- usage %>%
  select(from, to, consumption) %>%
  left_join(
    agile_prices %>%
      select(from, import, export),
    by = "from"
  ) %>%
  mutate(
    date = date(from),
    month = month(date, label = TRUE),
    season = case_when(
      month %in% c("Dec", "Jan", "Feb") ~ "Winter",
      month %in% c("Mar", "Apr", "May") ~ "Spring",
      month %in% c("Jun", "Jul", "Aug") ~ "Summer",
      month %in% c("Sep", "Oct", "Nov") ~ "Autumn"
    ),
    time = hm(str_c(hour(from), ":", minute(from))),
    time = as_date("2025-01-01") + time
  ) %>%
  pivot_longer(c(consumption, import, export), names_to = "measure")


sample_dates <- sample(unique(df$date), 3)

df %>%
  filter(date %in% sample_dates, measure == "import") %>%
  mutate(price.centile = ecdf(value)(value), .by = date) %>%
  mutate(price.bucket = cut(price.centile, c(0, 0.25, 0.5, 0.75, 1))) %>%
  mutate(date = as.character(date)) %>%
  ggplot(aes(x = time, y = value, colour = price.bucket, group = date)) +
  geom_line()


df %>%
  filter(measure != "export", !is.na(value), value > 0) %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  mutate(price.centile = ecdf(import)(import), .by = date) %>%
  mutate(
    price.bucket = cut(
      price.centile,
      c(0, 0.25, 0.5, 0.75, 1),
      include.lowest = TRUE
    )
  ) %>%
  summarise(
    total.cons = sum(consumption),
    .by = c(date, month, season, price.bucket)
  ) %>%
  filter(price.bucket %in% c("(0.5,0.75]", "(0.75,1]")) %>%
  mutate(above.5 = if_else(total.cons > 5, TRUE, FALSE)) %>%
  summarise(total.cons = sum(total.cons), .by = c(date, above.5))
summarise(share.above.5 = sum(above.5) / n(), .by = month)
ggplot(aes(
  x = month,
  y = mean.kwh,
  colour = price.bucket,
  group = price.bucket
)) +
  geom_line() +
  geom_point()

df %>%
  filter(is.na(value))
