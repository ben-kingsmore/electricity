library(httr)
library(digest)
library(jsonlite)

# ── Configuration ────────────────────────────────────────────────────────────

FOX_API_KEY <- Sys.getenv("FOXESS_API_KEY")
FOX_BASE_URL <- "https://www.foxesscloud.com"
DEVICE_SN <- "605H37205ADC047"

# ── Auth header builder ───────────────────────────────────────────────────────

fox_headers <- function(path) {
  timestamp <- as.character(round(as.numeric(Sys.time()) * 1000))
  sig_string <- paste0(path, "\\r\\n", FOX_API_KEY, "\\r\\n", timestamp)
  signature <- digest(sig_string, algo = "md5", serialize = FALSE)

  add_headers(
    token = FOX_API_KEY,
    timestamp = timestamp,
    signature = signature,
    lang = "en",
    `User-Agent` = "Mozilla/5.0"
  )
}

# ── Generic request helpers ───────────────────────────────────────────────────

fox_get <- function(path, query = list()) {
  resp <- GET(
    url = paste0(FOX_BASE_URL, path),
    config = fox_headers(path),
    query = query
  )
  stop_for_status(resp)
  content(resp, as = "parsed", type = "application/json")
}

fox_post <- function(path, body = list()) {
  resp <- POST(
    url = paste0(FOX_BASE_URL, path),
    config = fox_headers(path),
    body = toJSON(body, auto_unbox = TRUE),
    encode = "raw",
    content_type_json()
  )
  stop_for_status(resp)
  content(resp, as = "parsed", type = "application/json")
}

# ── API calls ─────────────────────────────────────────────────────────────────

# List all devices on the account
fox_device_list <- function(page = 1, page_size = 10) {
  fox_post("/op/v0/device/list", list(currentPage = page, pageSize = page_size))
}

# Get device detail (includes battery info as of API v1.1.2)
fox_device_detail <- function(sn = DEVICE_SN) {
  fox_get("/op/v0/device/detail", query = list(sn = sn))
}

# Get real-time data — pass an empty variables list to get everything
fox_realtime <- function(sn = DEVICE_SN, variables = list()) {
  fox_post("/op/v0/device/real/query", list(sn = sn, variables = variables))
}

# Get history data between two POSIXct timestamps
fox_history <- function(
  sn = DEVICE_SN,
  from = Sys.time() - 86400,
  to = Sys.time(),
  variables = list()
) {
  fox_post(
    "/op/v0/device/history/query",
    list(
      sn = sn,
      variables = variables,
      begin = round(as.numeric(from) * 1000),
      end = round(as.numeric(to) * 1000)
    )
  )
}

# Get battery SoC limits (min SoC settings)
fox_battery_soc_get <- function(sn = DEVICE_SN) {
  fox_get("/op/v0/device/battery/soc/get", query = list(sn = sn))
}

# Get current scheduler (time segment charge/discharge plan)
fox_scheduler_get <- function(sn = DEVICE_SN) {
  fox_post("/op/v0/device/scheduler/get", list(deviceSN = sn))
}

# Check how many API calls you've used today
fox_usage <- function() {
  fox_get("/op/v0/user/getAccessCount")
}


# Start here — find your device serial number
devices <- fox_device_list()
str(devices)

# Then pull real-time data (all variables)
rt <- fox_realtime()
str(rt)

# Pull the last 24h of history for key variables
hist_data <- fox_history(
  variables = list(
    "pvPower",
    "loadsPower",
    "batChargePower",
    "batDischargePower",
    "SoC",
    "feedinPower",
    "gridConsumptionPower"
  )
)

# Flatten the history response into a data frame
# The response structure is: result -> data -> list of {variable, unit, data -> [{time, value}]}
parse_fox_history <- function(resp) {
  vars <- resp$result$data
  dfs <- lapply(vars, function(v) {
    df <- do.call(rbind, lapply(v$data, as.data.frame))
    df$variable <- v$variable
    df$unit <- v$unit
    df
  })
  result <- do.call(rbind, dfs)
  result$time <- as.POSIXct(
    result$time / 1000,
    origin = "1970-01-01",
    tz = "Europe/London"
  )
  result
}

history_df <- parse_fox_history(hist_data)
head(history_df)


results <- vector("list", length(hist_data$result[[1]]$datas))

for (i in seq_along(results)) {
  unit <- hist_data$result[[1]]$datas[[i]]$unit
  name <- hist_data$result[[1]]$datas[[i]]$name
  variable <- hist_data$result[[1]]$datas[[i]]$variable

  data <- purrr::map_dfr(
    hist_data$result[[1]]$datas[[i]]$data,
    \(x) tibble::tibble(time = x[["time"]], value = x[["value"]])
  )
  data <- data |>
    dplyr::mutate(name = name, unit = unit, variable = variable)

  results[[i]] <- data
}


library(tidyverse)

results |>
  bind_rows() |>
  filter(name != "SoC") |>
  mutate(time = as_datetime(time)) |>
  ggplot(aes(x = time, y = value, colour = name)) +
  geom_line()
