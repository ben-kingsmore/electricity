update_octopus_data <- function() {
  # Read existing data from google drive
  cons_files <- googledrive::drive_ls("Energy usage data")
  cons_files <- cons_files$name[str_detect(
    cons_files$name,
    "energy_consumption_"
  )]
  cons_latest <- sort(cons_files, decreasing = TRUE)[1]

  googledrive::drive_download(
    glue::glue("Energy usage data/{cons_latest}"),
    "consumption_old.csv",
    overwrite = TRUE
  )

  cons_old <- read_csv("consumption_old.csv")

  # Read latest data from Octopus
  elec_new <- octopusR::get_consumption(
    "electricity",
    period_from = today() - (365 * 2),
    period_to = today()
  )

  gas_new <- octopusR::get_consumption(
    "gas",
    period_from = today() - (365 * 2),
    period_to = today()
  )

  cons_new <- bind_rows(elec_new, gas_new, .id = "fuel") |>
    mutate(
      fuel = case_when(
        fuel == "1" ~ "electricity",
        fuel == "2" ~ "gas"
      )
    ) |>
    mutate(across(contains("interval"), as_datetime))

  # Keep any old consumption data not in the new set
  to_keep <- cons_old |>
    filter(!interval_start %in% cons_new$interval_start)

  consumption <- bind_rows(to_keep, cons_new) |>
    arrange(fuel, interval_start)

  write_csv(consumption, "consumption.csv")

  # Save updated data to google drive
  googledrive::drive_upload(
    "consumption.csv",
    glue::glue("Energy usage data/energy_consumption_{today()}.csv"),
    overwrite = TRUE
  )

  new_data <- consumption$interval_start[
    !consumption$interval_start %in% cons_old$interval_start
  ]

  cli::cli_alert_success("Added {length(new_data)} data points")

  return(consumption)
}


# Probs remove function above given future Octopus data less crucial
# Incorp this as workhorse
update_drive_csv <- function(drive_csv, new_data, dup_col = NULL) {
  existing_data <- drive_read_string(drive_csv) |>
    readr::read_csv()

  if (
    !all.equal(
      names(existing_data),
      names(new_data)
    )
  ) {
    cli::cli_abort("Columns in new data do not match existing")
  }

  dup_vals <- new_data[[dup_col]]

  if (!is.null(dup_col)) {
    existing_data <- existing_data |>
      filter(.data[[dup_col]] %in% dup_vals)
  }

  updated_data <- dplyr::bind_rows(
    existing_data,
    new_data
  ) |>
    distinct()

  write_csv(updated_data, "temp_output.csv")
  drive_put("temp_output.csv", drive_csv)
  file.remove("temp_output.csv")

  return(updated_data)
}
