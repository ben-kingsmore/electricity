library(httr2)
library(dplyr)
library(lubridate)
library(ggplot2)

# ── Configuration ──────────────────────────────────────────────────────────────
product_code <- "AGILE-24-10-01"
gsp <- "C" # C = London; change for your region (see below)
tariff_code <- paste0("E-1R-", product_code, "-", gsp)

# gsp letters: A=Eastern, B=East Midlands, C=London, D=Merseyside & N.Wales,
#              E=Midlands, F=North Eastern, G=North Western, H=Southern,
#              J=South Eastern, K=South Wales, L=South Western, M=Yorkshire,
#              N=S. Scotland, P=N. Scotland

# ── Date window: today 23:00 UTC → tomorrow 23:00 UTC (= one UK "price day") ──
today <- Sys.Date()
period_from <- format(
  as.POSIXct(paste(today, "23:00:00"), tz = "UTC"),
  "%Y-%m-%dT%H:%M:%SZ"
)
period_to <- format(
  as.POSIXct(paste(today + 1, "23:00:00"), tz = "UTC"),
  "%Y-%m-%dT%H:%M:%SZ"
)

# ── API request ───────────────────────────────────────────────────────────────
url <- paste0(
  "https://api.octopus.energy/v1/products/",
  product_code,
  "/electricity-tariffs/",
  tariff_code,
  "/standard-unit-rates/"
)

resp <- request(url) |>
  req_url_query(
    period_from = period_from,
    period_to = period_to,
    page_size = 100
  ) |>
  req_perform() |>
  resp_body_json()

# ── Parse into a tidy data frame ──────────────────────────────────────────────
agile_prices <- resp$results |>
  lapply(\(x) {
    data.frame(
      valid_from = as.POSIXct(
        x$valid_from,
        format = "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      ),
      valid_to = as.POSIXct(
        x$valid_to,
        format = "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      ),
      p_per_kwh_inc = x$value_inc_vat, # pence/kWh inc. VAT
      p_per_kwh_exc = x$value_exc_vat # pence/kWh exc. VAT
    )
  }) |>
  bind_rows() |>
  mutate(
    valid_from_local = with_tz(valid_from, "Europe/London"),
    valid_to_local = with_tz(valid_to, "Europe/London")
  ) |>
  arrange(valid_from)

price_date <- date(agile_prices$valid_from[3])

plotly::ggplotly(
  agile_prices |>
    ggplot(aes(x = valid_from_local, y = p_per_kwh_inc, fill = p_per_kwh_inc)) +
    geom_col() +
    scale_fill_gradient(guide = "none", low = "green", high = "red") +
    theme(text = element_text(size = 14)) +
    xlab("Time") +
    ylab("Pence per kWh") +
    ggtitle(glue::glue("Agile prices for {format(price_date, '%d %b %Y')}"))
)
