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