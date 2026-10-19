# Data Utils -----------------------------------------------------
# Helper functions for normalizing data types from gsheets
normalize_numeric <- function(x) {
  as.numeric(unlist(x))
}

normalize_character <- function(x) {
  vapply(
    x,
    function(value) {
      if (is.null(value)) {
        NA_character_
      } else {
        as.character(value)
      }
    },
    character(1)
  )
}

# Read data from gsheets
read_drinks <- function(sheet_url) {

  drinks <- googlesheets4::read_sheet(sheet_url)

  drinks$date <- as.Date(drinks$date)
  drinks$time <- as.character(drinks$time)
  drinks$drink <- as.character(drinks$drink)
  drinks$brand <- as.character(drinks$brand)
  drinks$style <- as.character(drinks$style)

  drinks$abv <- normalize_numeric(drinks$abv)
  drinks$volume <- normalize_numeric(drinks$volume)
  drinks$notes <- normalize_character(drinks$notes)

  validate_drinks(drinks)  # validate data read

  drinks
}

# Validate data reads. Enforce column names.
validate_drinks <- function(drinks) {

  required_columns <- c(
    "date", "time", "drink", "brand",
    "style", "abv", "volume", "notes"
  )

  missing_columns <- setdiff(required_columns, names(drinks))

  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  if (any(is.na(drinks$date))) {
    stop("Missing or invalid values found in 'date'.")
  }

  if (any(is.na(drinks$drink))) {
    stop("Missing values found in 'drink'.")
  }

  if (any(is.na(drinks$abv))) {
    stop("Missing or invalid values found in 'abv'.")
  }

  if (any(is.na(drinks$volume))) {
    stop("Missing or invalid values found in 'volume'.")
  }

  invisible(TRUE)
}

# Write data to gsheets
append_drink <- function(sheet_url, drink) {

  # stop("Simulated Google Sheets failure")

  required_columns <- c(
    "date", "time", "drink", "brand",
    "style", "abv", "volume", "notes"
  )

  if (!all(required_columns %in% names(drink))) {
    stop("Drink record does not contain all required columns.")
  }

  drink <- drink[, required_columns]

  googlesheets4::sheet_append(
    ss = sheet_url,
    data = drink
  )

  invisible(TRUE)
}

# BAC utils -----------------------------------------------------------
calculate_alcohol <- function(abv, volume) {
  (abv / 100) * (volume * 29.574)  # oz to ml
}

calculate_bac <- function(alcohol, weight_g, r, elapsed_hours) {
  bac <- (alcohol / (weight_g * r) * 100) - (elapsed_hours * 0.015)

  max(bac, 0)  # Clamp bac to >= 0
}

# Sobriety calculations
calculate_sober_time <- function(last_drink, peak_bac) {
  last_drink + (peak_bac / 0.015) * 3600
}

# UI / formatting utils ------------------------------------------------------------
# Format tSober and tToSober for UI
format_duration <- function(hours) {
  # TODO: improve plurality (1 year vs 2 years, etc.)
  # TODO: Handle edge case where minutes = 0
  # TODO: Handle edge cases where trailing value = 0, e.g. 1 hour 0 minutes -> 1 hour, etc.
  if(hours >= 8760) {
    years <- floor(hours / 8760)
    days <- floor((hours %% 8760) / 24)

    paste(years, 'years', days, 'days')
  } else if (hours >= 24) {
    wholeDays <- floor(hours / 24)
    remainingHours <- floor(hours %% 24)
    
    paste(wholeDays, 'days', remainingHours, 'hours')
  } else if (hours >= 1) {
    wholeHours <- floor(hours)
    minutes <- floor((hours - wholeHours) * 60)

    paste(wholeHours, 'hours', minutes, 'minutes')
  } else {
    minutes <- floor(hours * 60)

    paste(minutes, 'minutes')
  }
}

# Authentication utils ------------------------------------------------------
authenticate_google <- function() {
  credentials <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_B64")

  if(!nzchar(credentials)) {
    stop("Google service-account credentials are not configured.")
  }

  key_file <- tempfile(fileext = '.json')
  json_raw <- openssl::base64_decode(credentials)
  writeBin(json_raw, key_file)

  googlesheets4::gs4_auth(
    path = key_file
  )

  invisible(TRUE)
}
