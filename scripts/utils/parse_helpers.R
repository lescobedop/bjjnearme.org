# parse_helpers.R — Time & JSON parsing utilities for BJJNearMe
rm(list = ls())

library(jsonlite)
library(stringr)
library(lubridate)
library(tibble)

# ---- Safe JSON Parsing ----
safe_parse_json <- function(text) {
  tryCatch(fromJSON(text), error = function(e) NULL)
}

# ---- Time Parsing Utilities ----

# Parse a single time string into HH:MM 24-hour format
parse_single_time <- function(time_str) {
  if (is.na(time_str) || str_trim(time_str) == "") return(NA_character_)
  
  time_clean <- str_replace_all(time_str, "\\.", ":") %>%
    str_replace_all("([0-9])([ap]m)", "\\1 \\2") %>%
    str_trim()
  
  parsed <- parse_date_time(
    time_clean,
    orders = c("I:M p", "HMS", "HM", "H p", "I p", "H", "I"),
    tz = "UTC"
  )
  
  if (is.na(parsed)) return(NA_character_)
  
  format(parsed, "%H:%M")
}

# Parse a raw time range like "6–10PM" into open/close format
parse_open_close_range <- function(raw_string) {
  if (is.na(raw_string) || str_trim(raw_string) == "") {
    return(tibble(open = NA, close = NA, closed = TRUE, pattern_flag = "missing"))
  }
  
  raw_lower <- str_to_lower(raw_string)
  
  if (str_detect(raw_lower, "closed|cerrado")) {
    return(tibble(open = NA, close = NA, closed = TRUE, pattern_flag = "closed"))
  }
  
  if (str_detect(raw_lower, "24\\s?hours|abierto.*24")) {
    return(tibble(open = "00:00", close = "23:59", closed = FALSE, pattern_flag = "24hrs"))
  }
  
  if (!str_detect(raw_string, "-")) {
    return(tibble(open = NA, close = NA, closed = FALSE, pattern_flag = "no-dash"))
  }
  
  parts <- str_split(raw_string, "-", simplify = TRUE)
  if (ncol(parts) != 2) {
    return(tibble(open = NA, close = NA, closed = FALSE, pattern_flag = "unparsed"))
  }
  
  open_time <- parse_single_time(parts[1])
  close_time <- parse_single_time(parts[2])
  
  if (is.na(open_time) || is.na(close_time)) {
    return(tibble(open = NA, close = NA, closed = FALSE, pattern_flag = "unparsed"))
  }
  
  tibble(open = open_time, close = close_time, closed = FALSE, pattern_flag = "standard")
}

# Reduce multiple ranges like "6-10AM,5-10PM" to the earliest open and latest close
reduce_multiple_ranges <- function(raw_string) {
  if (is.na(raw_string)) return(tibble(open = NA, close = NA, closed = TRUE, pattern_flag = "missing"))
  
  if (str_detect(raw_string, "closed|cerrado")) {
    return(tibble(open = NA, close = NA, closed = TRUE, pattern_flag = "closed"))
  }
  
  if (str_detect(str_to_lower(raw_string), "24\\s?hours|abierto.*24")) {
    return(tibble(open = "00:00", close = "23:59", closed = FALSE, pattern_flag = "24hrs"))
  }
  
  ranges <- str_split(raw_string, ",")[[1]] %>% str_trim()
  parsed_ranges <- map_dfr(ranges, parse_open_close_range)
  
  if (nrow(parsed_ranges) == 0 || all(is.na(parsed_ranges$open))) {
    return(tibble(open = NA, close = NA, closed = FALSE, pattern_flag = "unparsed"))
  }
  
  open_time <- min(parsed_ranges$open, na.rm = TRUE)
  close_time <- max(parsed_ranges$close, na.rm = TRUE)
  
  tibble(open = open_time, close = close_time, closed = FALSE, pattern_flag = "multi")
}
