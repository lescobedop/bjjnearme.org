# martial_arts.R — Build long-format martial arts offerings from subtype data

rm(list = ls())

library(tidyverse)
library(jsonlite)
library(here)

# ------------------------
# 1. Load input data
# ------------------------

input_file <- here("data/interim/places_lima_google_combined.csv")
df <- read_csv(input_file, show_col_types = FALSE) %>%
  select(place_id, best_type, best_subtype)

# ------------------------
# 2. Clean up subtype field
# ------------------------

df <- df %>%
  mutate(best_subtype_raw = best_subtype) %>%
  mutate(
    best_subtype = case_when(
      is.na(best_subtype_raw) ~ NA_character_,
      str_trim(best_subtype_raw) %in% c("", "[]") ~ NA_character_,
      str_to_lower(str_trim(best_subtype_raw)) %in% c("n/a", "none", "na") ~ NA_character_,
      TRUE ~ best_subtype_raw
    )
  )

# ------------------------
# 3. Parse subtype list (handle JSON and fallback cases)
# ------------------------

parse_subtype_list <- function(x) {
  if (is.na(x)) return(NA_character_)
  try_json <- tryCatch(fromJSON(x), error = function(e) NULL)
  if (!is.null(try_json) && is.vector(try_json)) {
    return(try_json)
  } else {
    x_cleaned <- str_remove_all(x, "^c\\(|\\)$|\"|\\[|\\]")
    x_split <- str_split(x_cleaned, ",|\\.")[[1]]
    return(str_trim(x_split))
  }
}

df <- df %>%
  mutate(subtype_list = map(best_subtype, parse_subtype_list))

# ------------------------
# 4. Unnest to long format
# ------------------------

subtype_long <- df %>%
  select(place_id, best_type, subtype_list) %>%
  unnest_longer(subtype_list, values_to = "subtype", keep_empty = TRUE) %>%
  mutate(subtype = str_squish(subtype)) %>%
  filter(!is.na(best_type)) %>%
  distinct()

# ------------------------
# 5. Apply known corrections (manual overrides)
# ------------------------

corrections <- tribble(
  ~place_id, ~best_type,      ~subtype,
  "ChIJtwb4dADJBZER1-b2zKidZzU", "Muay Thai",    "Jiu Jitsu",
  "ChIJ_ajXUwDJBZERn-ZfOy2Pt4g", "Jiu Jitsu",    NA_character_,
  "ChIJ92Sw1zfJBZERjzp-2jBBPCc", "Muay Thai",    "Jiu Jitsu",
  "ChIJz03HLXzBBZERrg1vTS-hH8o", "Tae Kwon Do",  "MMA",
  "ChIJMWm_TyDFBZERPAmzxjVEhbo", "Tae Kwon Do",  "MMA",
  "ChIJwTj8Xu7FBZERi0agsCck9RU", "Tae Kwon Do",  "MMA",
  "ChIJf9J7Su7QBZER5OjNsKwQDeI", "MMA",          "Boxing",
  "ChIJf9J7Su7QBZER5OjNsKwQDeI", "MMA",          "Kickboxing",
  "ChIJz03HLXzBBZERrg1vTS-hH8o", "Tae Kwon Do",  "Kickboxing",
  "ChIJMWm_TyDFBZERPAmzxjVEhbo", "Tae Kwon Do",  "Muay Thai",
  "ChIJwTj8Xu7FBZERi0agsCck9RU", "Tae Kwon Do",  "Muay Thai",
  "ChIJ-Y9xE8PFBZERYnNQ_wmVuXg", "MMA",          "Wrestling"
)

subtype_cleaned <- subtype_long %>%
  anti_join(corrections, by = c("place_id", "best_type")) %>%
  bind_rows(corrections)

# ------------------------
# 6. Join back to full gym list to ensure all gyms represented
# ------------------------

all_gyms <- df %>%
  distinct(place_id, best_type)

subtype_complete <- all_gyms %>%
  left_join(subtype_cleaned %>% filter(!is.na(subtype)), by = c("place_id", "best_type"))

dim(subtype_complete)

# ------------------------
# 7. QA Check — Gym Coverage
# ------------------------

expected_gyms <- df %>% distinct(place_id) %>% nrow()
actual_gyms <- subtype_complete %>% distinct(place_id) %>% nrow()

if (actual_gyms == expected_gyms) {
  message("✅ subtype_complete contains ALL ", actual_gyms, " gyms. ✅")
} else {
  warning("⚠️ subtype_complete contains only ", actual_gyms, " gyms. Expected: ", expected_gyms)
  missing_ids <- setdiff(df$place_id, subtype_complete$place_id)
  message("🔍 Missing place_ids:\n", paste(missing_ids, collapse = "\n"))
}


# ------------------------
# 8. Export
# ------------------------

output_file <- here("data/processed", paste0("subtype_long_lima_google_", Sys.Date(), ".csv"))
write_csv(subtype_complete, output_file)
message("✅ subtype_long file saved: ", output_file)
