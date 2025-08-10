# scripts/enrich/about_tags.R
# Purpose: Extract and flatten structured JSON from `about` column
# Outputs: long-format and wide-format tag exports

rm(list = ls())

# ---- Libraries ----
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(jsonlite)
library(stringr)
library(glue)
library(here)

# ---- Step 0: Config ----
input_file <- here("data/interim", "places_lima_google_combined_20250714.csv")
output_date <- format(Sys.Date(), "%Y%m%d")
output_long <- here("data/exports/long", glue("about_tags_long_lima_google_{output_date}.csv"))
output_wide <- here("data/exports/wide", glue("about_tags_wide_lima_google_{output_date}.csv"))

# ---- Step 1: Load Data ----
message("📥 Loading input data...")
df <- read_csv(input_file, show_col_types = FALSE) %>%
  select(place_id, about)

# ---- Step 2: Define Helpers ----
safe_parse <- function(json_string) {
  tryCatch(fromJSON(json_string), error = function(e) NULL)
}

flatten_about_tags <- function(json_data) {
  if (!is.list(json_data)) return(tibble())
  
  rows <- list()
  for (tag_type in names(json_data)) {
    sub_data <- json_data[[tag_type]]
    if (is.null(sub_data)) next
    if (is.list(sub_data)) {
      for (desc in names(sub_data)) {
        rows[[length(rows) + 1]] <- tibble(
          tag_type = tag_type,
          tag_description = desc,
          tag_value = sub_data[[desc]]
        )
      }
    } else {
      rows[[length(rows) + 1]] <- tibble(
        tag_type = tag_type,
        tag_description = NA_character_,
        tag_value = sub_data
      )
    }
  }
  bind_rows(rows)
}

# ---- Step 3: Parse About Column ----
message("🔎 Parsing about JSON field...")
parsed <- df %>%
  mutate(
    parsed_json = map(about, safe_parse),
    parsed_rows = map(parsed_json, flatten_about_tags)
  )

# ---- Step 4: Generate Long Format ----
about_tags_long <- parsed %>%
  select(place_id, parsed_rows) %>%
  unnest(parsed_rows) %>%
  mutate(
    tag_value = case_when(
      tag_value %in% c("TRUE", "true", TRUE) ~ TRUE,
      tag_value %in% c("FALSE", "false", FALSE) ~ FALSE,
      TRUE ~ NA
    ),
    source = "parsed"
  )

# ---- Step 5: Add Recovery for Failed or Empty Parses ----
recovery <- parsed %>%
  mutate(failed = map_lgl(parsed_rows, ~ nrow(.x) == 0)) %>%
  filter(failed) %>%
  transmute(
    place_id,
    tag_type = "about_json_error_or_empty",
    tag_description = NA_character_,
    tag_value = NA,
    source = "recovery"
  )

# ---- Step 6: Combine Long Format ----
about_tags_long <- bind_rows(about_tags_long, recovery)

# ---- Step 6.5: Validate Final Long Format Has 190 Gyms ----
long_count <- n_distinct(about_tags_long$place_id)
if (long_count != 190) {
  stop(glue("❌ Final long-format gym count mismatch: Expected 190, but found {long_count}."))
}


# ---- Step 7: Create Wide Format ----
about_tags_wide <- about_tags_long %>%
  mutate(tag_key = if_else(
    is.na(tag_description),
    tag_type,
    str_c(tag_type, tag_description, sep = "_")
  )) %>%
  select(place_id, tag_key, tag_value) %>%
  pivot_wider(
    names_from = tag_key,
    values_from = tag_value,
    values_fill = list(tag_value = NA)
  )

# ---- Step 7.5: Validate Wide Format Has 190 Gyms ----
wide_count <- n_distinct(about_tags_wide$place_id)
if (wide_count != 190) {
  stop(glue("❌ Wide-format gym count mismatch: Expected 190, but found {wide_count}."))
}


# ---- Step 8: Export Files ----
write_csv(about_tags_long, output_long)
write_csv(about_tags_wide, output_wide)
message(glue("✅ Exported long format: {output_long}"))
message(glue("✅ Exported wide format with {wide_count} gyms: {output_wide}"))
