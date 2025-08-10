# scripts/pipeline/load_and_merge.R
# Purpose: Load and merge all relevant sheets from raw Google scrape into unified interim dataset

rm(list = ls())

# ---- Libraries ----
library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(here)
library(glue)
library(readr) 

# ---- Config ----
source_file <- here("data/raw/raw_lima_callao_google_search4_augmented.xlsx")
sheets_to_read <- c("Best_type_Only", "Best_Subtype_Only")  # Update if needed
output_date <- Sys.Date() |> format("%Y%m%d")
output_file <- here("data/interim", glue("places_lima_google_combined_{output_date}.csv"))

# ---- Step 1: Load Sheets & Clean ----
sheet_dfs <- lapply(sheets_to_read, function(sheet) {
  message(glue("📄 Reading sheet: {sheet}"))
  read_excel(source_file, sheet = sheet) %>%
    clean_names() %>%
    mutate(source_sheet = sheet)
})

# ---- Step 2: Merge & Deduplicate ----
combined_df <- bind_rows(sheet_dfs) %>%
  filter(!is.na(place_id)) %>%
  mutate(place_id = as.character(place_id)) %>%
  distinct(place_id, .keep_all = TRUE)

# ---- Step 3: Save Output ----
write_csv(combined_df, output_file)
message(glue("✅ Combined dataset saved: {output_file}"))
