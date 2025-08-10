# clean_media.R

rm(list = ls())

# -----------------------------
# Cleans media fields: photo, street_view, logo
# Adds completeness flags and scoring for UI and gym enrichment

library(tidyverse)
library(here)
library(glue)

# ---- Step 1: Load Raw Data ----
input_file <- here("data/interim/places_lima_google_combined_20250713.csv")
df <- read_csv(input_file, show_col_types = FALSE) %>%
  select(place_id, photo, street_view, logo)

# ---- Step 2: Define Validity Functions ----
is_valid_url <- function(x) {
  x <- str_to_lower(x)
  str_detect(x, "^https?://") & !str_detect(x, "noimage|placeholder|default|missing|error|na")
}

# ---- Step 3: Clean + Tag Completeness ----
media_clean <- df %>%
  mutate(
    photo = if_else(is_valid_url(photo), photo, NA_character_),
    street_view = if_else(is_valid_url(street_view), street_view, NA_character_),
    logo = if_else(is_valid_url(logo), logo, NA_character_),
    
    has_photo = !is.na(photo),
    has_street_view = !is.na(street_view),
    has_logo = !is.na(logo),
    
    media_completeness_score = has_photo + has_street_view + has_logo,
    
    logo_status = case_when(
      has_logo ~ "has_logo",
      !has_logo & has_photo ~ "can_extract_later",
      TRUE ~ "missing_logo"
    )
  )

# ---- Step 4: Export Clean File ----
output_file <- here("data/exports/long/media_long_lima_google_20250714.csv")
write_csv(media_clean, output_file)
message("✅ Clean media data saved to: ", output_file)
