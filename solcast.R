library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

api_key <- Sys.getenv("SOLCAST_API_KEY")

forecast_dttm <- Sys.time()

response <- httr::GET(
  url = "https://api.solcast.com.au/rooftop_sites/c3c9-7418-7691-43be/forecasts",
  query = list(format = "json"),
  httr::add_headers(Authorization = paste("Bearer", api_key))
)

httr::stop_for_status(response)

forecasts <- httr::content(response, as = "parsed", type = "application/json")

forecasts <- map_dfr(forecasts$forecasts, bind_rows) |>
  mutate(
    vintage = lubridate::as_datetime(forecast_dttm),
    period_end = lubridate::ymd_hms(period_end),
    period_end_local = lubridate::with_tz(period_end, "Europe/London")
  ) |>
  select(
    vintage,
    period_end,
    period_end_local,
    pv_estimate,
    pv_estimate10,
    pv_estimate90
  )

# Need to save and append to existing data on google drive
# Might need to follow approach in octopus function

forecasts |>
  select(!c(period, period_end)) |>
  pivot_longer(!c(period_end_local), names_to = "pctile") |>
  ggplot(aes(x = period_end_local, y = value, colour = pctile)) +
  geom_line()
