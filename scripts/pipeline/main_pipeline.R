# main_pipeline.R — Final assembly for WordPress ingestion

rm(list = ls())
library(tidyverse)
library(here)
library(glue)

# ---- Step 1: Load Core Gym Info ----
core <- read_csv(here("data/interim", "places_lima_google_combined_20250714.csv"), show_col_types = FALSE) %>%
  select(place_id, name, city, lat = latitude, lon = longitude, about)

# ---- Step 2: Load Enriched Tags ----
tags <- read_csv(here("data/exports/wide", "about_tags_wide_mvp_20250721.csv"), show_col_types = FALSE)

# ---- Step 3: Load Reviews Data ----
reviews <- read_csv(here("data/exports/wide", "reviews_lima_google_20250714.csv"), show_col_types = FALSE)

# ---- Step 4: Load Media Data ----
media <- read_csv(here("data/exports/long", "media_long_lima_google_20250714.csv"), show_col_types = FALSE) %>%
  select(place_id, photo, street_view, logo)

# ---- Step 5: Load Contact Channels ----
contacts <- read_csv(here("data/exports/long", "contact_channels_long_lima_google_20250714.csv"), show_col_types = FALSE) %>%
  select(place_id, website = site, email, whatsapp, instagram, facebook)

# ---- Step 6: Merge All Sources ----
final <- core %>%
  left_join(tags %>% select(-name), by = "place_id") %>%
  left_join(reviews, by = "place_id") %>%
  left_join(media, by = "place_id") %>%
  left_join(contacts, by = "place_id") %>%  # ← no need to drop 'name' if it’s not there
  mutate(across(where(is.logical), ~ replace_na(.x, FALSE))) %>%
  mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
  mutate(
    trust_level = case_when(
      is.na(rating) | is.na(reviews) ~ "Low Visibility",
      reviews < 10 ~ "Emerging Trust",
      reviews >= 10 & rating >= 4.4 ~ "Consistent Trust",
      reviews >= 50 & rating >= 4.6 ~ "High Trust",
      TRUE ~ "Moderate Trust"
    )
  )

# ---- Step 7: Final Output — Only Required Columns for WordPress ----
final_export <- final %>%
  mutate(
    whatsapp_number = case_when(
      str_detect(whatsapp, "wa\\.me/\\d+") ~ paste0("+", str_extract(whatsapp, "(?<=wa\\.me/)\\d+")),
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    place_id,
    name,
    city,
    lat,
    lon,
    has_kids_classes,
    has_women_classes,
    has_open_mat,
    drop_in_fee,
    beginner_friendly,
    family_friendly,
    accessible_entrance,
    speaks_english,
    speaks_portuguese,
    offers_gi,
    offers_no_gi,
    overall_rating    = rating,
    num_reviews       = reviews,
    reviews_link,
    photo_link        = photo,
    logo_link         = logo,
    whatsapp_number,
    instagram,
    facebook
  )


# ---- Step 8: Save Export ----
output_file <- here("data/exports/wide", "places_lima_google_FINAL_LATEST.csv")
write_csv(final_export, output_file)

message("✅ FINAL export completed successfully: places_lima_google_FINAL_LATEST.csv")
