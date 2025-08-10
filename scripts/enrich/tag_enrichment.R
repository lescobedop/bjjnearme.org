# scripts/enrich/tag_enrichment.R
# Purpose: Enrich gym data with persona-relevant tags for MVP display

rm(list = ls())

# ------------------------
# 0. Load Libraries
# ------------------------

library(tidyverse)
library(here)
library(jsonlite)
library(stringr)
library(glue)

# ------------------------
# 1. Load Input Files
# ------------------------

df_core <- read_csv(here("data/interim/places_lima_google_combined_20250714.csv"), show_col_types = FALSE) %>%
  select(place_id, name, about)

df_hours <- read_csv(here("data/exports/long/working_hours_long_lima_google_20250720.csv"), show_col_types = FALSE)

df_subtype <- read_csv(here("data/exports/long/martial_arts_long_lima_google_20250713.csv"), show_col_types = FALSE)

# ------------------------
# 2. Helper Functions
# ------------------------

detect_pattern <- function(text, patterns) {
  str_detect(str_to_lower(text), str_c(patterns, collapse = "|"))
}

get_open_mat_schedule <- function(hours_df) {
  hours_df %>%
    filter(str_detect(str_to_lower(raw), "open mat|mat.*libre")) %>%
    group_by(place_id) %>%
    summarise(open_mat_schedule = paste0(day, ": ", raw, collapse = " | "), .groups = "drop")
}

# ------------------------
# 3. Tag Inference
# ------------------------

# 3.1 Inferred from 'about' text
about_tags <- df_core %>%
  mutate(about_lower = str_to_lower(about)) %>%
  mutate(
    has_kids_classes     = detect_pattern(about_lower, c("niñ[oa]s", "infantil", "kids", "menores")),
    has_women_classes    = detect_pattern(about_lower, c("mujeres", "femenina", "women[’']?s", "clases.*mujeres")),
    has_open_mat         = detect_pattern(about_lower, c("open mat", "mat.*libre", "entrenamiento libre", "sin profesor")),
    drop_in_fee          = detect_pattern(about_lower, c("drop[- ]?in", "visita", "pase libre", "mat fee", "pago.*clase", "sin.*membresía")),
    beginner_friendly    = detect_pattern(about_lower, c("principiante", "beginner", "básico", "inicial", "nuevo en bjj")),
    family_friendly      = detect_pattern(about_lower, c("familia", "padres", "niños", "ambiente.*familiar")),
    accessible_entrance  = detect_pattern(about_lower, c("acceso.*discapacitados", "silla de ruedas", "rampa")),
    speaks_english       = detect_pattern(about_lower, c("english|inglés")),
    speaks_portuguese    = detect_pattern(about_lower, c("portugu[eé]s"))
  ) %>%
  select(place_id,
         has_kids_classes, has_women_classes, has_open_mat,
         drop_in_fee, beginner_friendly, family_friendly,
         accessible_entrance, speaks_english, speaks_portuguese)

# 3.2 Inferred from working hours (for confirmation of open mat)
open_mat_sched <- get_open_mat_schedule(df_hours)

about_tags <- about_tags %>%
  left_join(open_mat_sched, by = "place_id") %>%
  mutate(has_open_mat = has_open_mat | !is.na(open_mat_schedule)) %>%
  select(-open_mat_schedule)

# 3.3 Inferred from subtype field (gi / no-gi)
subtype_tags <- df_subtype %>%
  mutate(subtype_lower = str_to_lower(subtype)) %>%
  mutate(
    is_gi     = str_detect(subtype_lower, "gi|kimono"),
    is_nogi   = str_detect(subtype_lower, "no[- ]?gi|submission|sin kimono")
  ) %>%
  group_by(place_id) %>%
  summarise(
    offers_gi     = any(is_gi),
    offers_no_gi  = any(is_nogi),
    .groups = "drop"
  )

# ------------------------
# 4. Final Merge and Formatting
# ------------------------

tags_final <- df_core %>%
  select(place_id, name) %>%
  distinct() %>%
  left_join(about_tags, by = "place_id") %>%
  left_join(subtype_tags, by = "place_id") %>%
  mutate(across(where(is.logical), ~ replace_na(.x, FALSE)))


# ------------------------
# 5. Manual Overrides (selectively applied)
# ------------------------

manual_overrides <- list(
  has_kids_classes     = c(
    "ChIJbehiZfDHBZERxD71uXfBsvE",
    "ChIJBWk2LFfHBZEReyxW8Pzqrnc",
    "ChIJVxAkJBXHBZERuJ5Z1IzrEPc",
    "ChIJeZn4EQDHBZERK-YhvBUA_nI",
    "ChIJSb1NrmPJBZERf34Gt0a60eM"
  ),
  has_open_mat         = character(0),
  has_women_classes    = character(0),
  beginner_friendly    = character(0),
  offers_gi            = character(0),
  offers_no_gi         = character(0)
)

# Apply manual overrides
for (tag in names(manual_overrides)) {
  place_ids <- manual_overrides[[tag]]
  
  # Check that column exists in tags_final
  if (!(tag %in% colnames(tags_final))) {
    warning(glue::glue("⚠️ Column '{tag}' not found in tags_final. Skipping override."))
    next
  }
  
  # Apply overrides only if place_ids are provided
  if (length(place_ids) > 0) {
    tags_final[[tag]] <- if_else(
      tags_final$place_id %in% place_ids,
      TRUE,
      tags_final[[tag]]
    )
  }
}

# ------------------------
# 6. Export Final Outputs
# ------------------------

# 6.1 Wide export (1 row per gym)
write_csv(tags_final, here("data/exports/wide/about_tags_wide_mvp_20250721.csv"))

# 6.2 Long export (1 row per tag per gym)
tags_long <- tags_final %>%
  select(-name) %>%
  pivot_longer(
    cols = -c(place_id),
    names_to = "tag",
    values_to = "value"
  ) %>%
  filter(!is.na(value), value != FALSE)

write_csv(tags_long, here("data/exports/long/about_tags_long_mvp_20250721.csv"))

message("✅ MVP tag enrichment completed and saved: wide + long versions.")