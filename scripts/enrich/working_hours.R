# scripts/clean/working_hours.R
# Purpose: Normalize gym working hours to long and wide formats (MVP-ready)

rm(list = ls())

# ---- Load libraries ----
library(tidyverse)
library(jsonlite)
library(lubridate)
library(here)
library(glue)

# ---- Step 1: Load Data ----
input_file <- here("data/interim", "places_lima_google_combined_20250714.csv")
output_date <- format(Sys.Date(), "%Y%m%d")
output_long <- here("data/exports/long", glue("working_hours_long_lima_google_{output_date}.csv"))
output_wide <- here("data/exports/wide", glue("working_hours_wide_lima_google_{output_date}.csv"))

df <- read_csv(input_file, show_col_types = FALSE) %>%
  select(place_id, working_hours)

stopifnot(nrow(df) == 190)

days_of_week <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# ---- Step 2: Parse JSON into Raw Daily Rows ----
working_hours_long <- df %>%
  mutate(json_parsed = map(working_hours, ~ tryCatch(fromJSON(.x), error = function(e) NULL))) %>%
  rowwise() %>%
  mutate(day_data = list({
    data <- json_parsed
    tibble(
      day = days_of_week,
      raw = map_chr(days_of_week, ~ if (!is.null(data) && .x %in% names(data)) data[[.x]] else NA_character_)
    )
  })) %>%
  ungroup() %>%
  select(place_id, day_data) %>%
  unnest(day_data)

stopifnot(nrow(working_hours_long) == 190 * 7)

# ---- Step 3: Helpers to Parse Times ----
round_to_nearest_half_hour <- function(t) {
  if (is.na(t)) return(NA_character_)
  minutes <- hour(t) * 60 + minute(t)
  rounded <- round(minutes / 30) * 30
  sprintf("%02d:%02d", floor(rounded / 60), rounded %% 60)
}

parse_single_time <- function(str) {
  parse_date_time(str_trim(str), orders = c("I:Mp", "I p", "IMp", "H:M", "H"), quiet = TRUE)
}

parse_open_close_range <- function(time_range) {
  times <- str_split(time_range, "-")[[1]]
  if (length(times) != 2) return(c(NA, NA))
  open <- round_to_nearest_half_hour(parse_single_time(times[1]))
  close <- round_to_nearest_half_hour(parse_single_time(times[2]))
  return(c(open, close))
}

parse_open_close_mvp <- function(raw) {
  if (is.na(raw)) {
    return(tibble(open = NA, close = NA, closed = TRUE, pattern_flag = "missing"))
  }
  
  raw <- str_trim(str_to_lower(raw))
  
  if (raw %in% c("closed", "cerrado")) {
    return(tibble(open = NA, close = NA, closed = TRUE, pattern_flag = "closed"))
  }
  
  if (str_detect(raw, "24 hours")) {
    return(tibble(open = "00:00", close = "23:59", closed = FALSE, pattern_flag = "24hrs"))
  }
  
  if (str_detect(raw, "all day")) {
    return(tibble(open = NA, close = NA, closed = FALSE, pattern_flag = "ambiguous"))
  }
  
  intervals <- str_split(raw, ",")[[1]]
  parsed_ranges <- map(intervals, parse_open_close_range)
  parsed_ranges <- discard(parsed_ranges, ~ any(is.na(.x)))
  
  if (length(parsed_ranges) == 0) {
    return(tibble(open = NA, close = NA, closed = FALSE, pattern_flag = "unreadable"))
  }
  
  opens <- map_chr(parsed_ranges, 1)
  closes <- map_chr(parsed_ranges, 2)
  
  opens_dt <- suppressWarnings(as.POSIXct(opens, format = "%H:%M", tz = "UTC"))
  closes_dt <- suppressWarnings(as.POSIXct(closes, format = "%H:%M", tz = "UTC"))
  
  tibble(
    open = format(min(opens_dt, na.rm = TRUE), "%H:%M"),
    close = format(max(closes_dt, na.rm = TRUE), "%H:%M"),
    closed = FALSE,
    pattern_flag = "standard"
  )
}

# ---- Step 4: Apply Parsing Logic ----
working_hours_clean <- working_hours_long %>%
  mutate(parsed = map(raw, parse_open_close_mvp)) %>%
  unnest(parsed) %>%
  select(place_id, day, raw, open, close, closed, pattern_flag) %>%
  arrange(place_id, day)

# ---- Step 4.5: Validate Long Format ----
stopifnot(nrow(working_hours_clean) == 190 * 7)
stopifnot(n_distinct(working_hours_clean$place_id) == 190)

# ---- Step 5: Create Wide Format ----
working_hours_wide <- working_hours_clean %>%
  select(place_id, day, open, close, closed, pattern_flag) %>%
  pivot_wider(
    names_from = day,
    values_from = c(open, close, closed, pattern_flag),
    names_glue = "{day}_{.value}"
  )

# ---- Step 5.5: Validate Wide Format ----
stopifnot(nrow(working_hours_wide) == 190)
stopifnot(n_distinct(working_hours_wide$place_id) == 190)

# ---- Step 6: Export ----
write_csv(working_hours_clean, output_long)
write_csv(working_hours_wide, output_wide)
message("✅ Working hours long and wide formats exported successfully.")
