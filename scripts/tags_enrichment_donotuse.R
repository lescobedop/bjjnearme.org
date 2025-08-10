# tag_enrichment.R — Generate structured tags from unstructured fields

rm(list = ls())
library(tidyverse)
library(here)
library(jsonlite)
library(stringr)

# ------------------------
# 1. Load input files
# ------------------------

df_core <- read_csv(here("data/interim/places_lima_google_combined.csv"), show_col_types = FALSE) %>%
  select(place_id, name, about)

df_about <- read_csv(here("data/processed/about_tags_long_20250713.csv"), show_col_types = FALSE)

df_hours <- read_csv(here("data/processed/working_hours_lima_google_MVP_20250713.csv"), show_col_types = FALSE)

df_subtype <- read_csv(here("data/processed/subtype_long_lima_google_2025-07-13.csv"), show_col_types = FALSE)

# ------------------------
# 2. Helper functions
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

# 3.1 From ABOUT column (regex on free text)
about_tags <- df_core %>%
  mutate(about_lower = str_to_lower(about)) %>%
  mutate(
    has_kids_classes     = detect_pattern(about_lower, c("niñ[oa]s", "infantil", "kids", "menores")),
    has_women_classes    = detect_pattern(about_lower, c("mujeres", "femenina", "women[’']?s", "clases.*mujeres")),
    women_only_classes   = detect_pattern(about_lower, c("solo.*mujeres", "women[’']?s only", "exclusiva.*mujeres")),
    has_open_mat         = detect_pattern(about_lower, c("open mat", "mat.*libre", "entrenamiento libre", "sin profesor")),
    drop_in_policy       = detect_pattern(about_lower, c("drop[- ]?in", "visita", "pase libre", "mat fee", "pago.*clase", "sin.*membresía")),
    beginner_friendly    = detect_pattern(about_lower, c("principiante", "beginner", "básico", "inicial", "nuevo en bjj")),
    family_friendly      = detect_pattern(about_lower, c("familia", "padres", "niños", "ambiente.*familiar")),
    accessible_entrance  = detect_pattern(about_lower, c("acceso.*discapacitados", "silla de ruedas", "rampa")),
    languages_spoken     = str_extract_all(about_lower, c("english|inglés|portugu[eé]s|español")) %>%
      map_chr(~ paste(unique(.x), collapse = ", "))
  ) %>%
  select(place_id,
         has_kids_classes, has_women_classes, women_only_classes, has_open_mat,
         drop_in_policy, beginner_friendly, family_friendly, accessible_entrance, languages_spoken)

# 3.2 From working_hours
open_mat_sched <- get_open_mat_schedule(df_hours)
about_tags <- about_tags %>%
  left_join(open_mat_sched, by = "place_id") %>%
  mutate(has_open_mat = has_open_mat | !is.na(open_mat_schedule))

# 3.3 From subtype listings
subtype_tags <- df_subtype %>%
  mutate(subtype_lower = str_to_lower(subtype)) %>%
  mutate(
    is_gi     = str_detect(subtype_lower, "gi|kimono"),
    is_nogi   = str_detect(subtype_lower, "no[- ]?gi|submission|sin kimono")
  ) %>%
  group_by(place_id) %>%
  summarise(
    class_types = case_when(
      any(is_gi) & any(is_nogi)     ~ "gi, no-gi",
      any(is_gi) & !any(is_nogi)    ~ "gi only",
      !any(is_gi) & any(is_nogi)    ~ "no-gi only",
      TRUE                          ~ NA_character_
    ),
    .groups = "drop"
  )

# ------------------------
# 4. Final Merge
# ------------------------

tags_final <- df_core %>%
  select(place_id, name) %>%
  distinct() %>%
  left_join(about_tags, by = "place_id") %>%
  left_join(subtype_tags, by = "place_id") %>%
  mutate(across(where(is.logical), ~ replace_na(.x, FALSE))) %>%
  mutate(across(c(open_mat_schedule, class_types, languages_spoken), ~ na_if(.x, "")))

# ------------------------
# 5. Manually set known place_ids with kids classes
# ------------------------

known_kids <- c(
  "ChIJbehiZfDHBZERxD71uXfBsvE",
  "ChIJBWk2LFfHBZEReyxW8Pzqrnc",
  "ChIJVxAkJBXHBZERuJ5Z1IzrEPc",
  "ChIJeZn4EQDHBZERK-YhvBUA_nI",
  "ChIJSb1NrmPJBZERf34Gt0a60eM"
)

tags_final <- tags_final %>%
  mutate(
    has_kids_classes = if_else(place_id %in% known_kids, TRUE, has_kids_classes)
  )


# ------------------------
# 6. Export
# ------------------------

write_csv(tags_final, here("data/processed/about_tags_wide_mvp.csv"))

message("✅ MVP tag enrichment completed and saved.")
