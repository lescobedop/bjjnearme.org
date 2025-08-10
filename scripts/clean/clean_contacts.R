# scripts/clean/clean_contacts.R
# Purpose: Clean phone, email, website, WhatsApp, IG, and FB links for gym directory

rm(list = ls())

# ---- Libraries ----
library(tidyverse)
library(here)
library(glue)
library(urltools)

# ---- Load helpers ----
source(here("scripts/utils/parse_helpers.R"))

# ---- Config ----
input_file <- here("data/interim", "places_lima_google_combined_20250713.csv")  # Use most recent file
output_date <- Sys.Date() |> format("%Y%m%d")
output_file <- here("data/exports/long", glue("contact_channels_long_lima_google_{output_date}.csv"))

# ---- Load data ----
df <- read_csv(input_file, show_col_types = FALSE)

# ---- Clean phone numbers (Peruvian mobiles only) ----
clean_peru_mobile <- function(x) {
  if (is.na(x) || str_trim(x) == "") return(NA_character_)
  digits <- x %>%
    str_replace_all("[^0-9]", "") %>%
    str_replace("^51", "")
  if (str_detect(digits, "^9\\d{8}$")) {
    return(str_glue("+51 {substr(digits, 1, 3)} {substr(digits, 4, 6)} {substr(digits, 7, 9)}"))
  }
  return(NA_character_)
}

df <- df %>%
  mutate(
    phone_1_cleaned = sapply(phone_1, clean_peru_mobile),
    phone = coalesce(phone, phone_1_cleaned)
  ) %>%
  select(-starts_with("phone_"))

# ---- Clean emails ----
suspicious_email_domains <- c("bit\\.ly", "leadgods", "canva\\.site", "wa\\.link", "example", "facebook", "google")
valid_email_regex <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"

clean_email <- function(email_col) {
  email_col <- str_trim(email_col)
  email_col <- ifelse(str_detect(email_col, str_c(suspicious_email_domains, collapse = "|")), NA, email_col)
  email_col <- ifelse(!str_detect(email_col, valid_email_regex), NA, email_col)
  return(email_col)
}

df <- df %>%
  mutate(
    email_1 = clean_email(email_1),
    email_2 = clean_email(email_2),
    email = coalesce(email_1, email_2)
  ) %>%
  select(-starts_with("email_"))

# ---- Clean website ----
unwanted_domains <- c("wa.link", "wa.me", "bit.ly", "calendly.com", "canva.site")

df <- df %>%
  mutate(site_original = site) %>%
  
  # Step 1: Extract potential IG or FB URLs from site field
  mutate(
    site_instagram_candidate = if_else(
      str_detect(site_original, "instagram\\.com"), site_original, NA_character_
    ),
    site_facebook_candidate = if_else(
      str_detect(site_original, "facebook\\.com"), site_original, NA_character_
    )
  ) %>%
  
  # Step 2: Remove known bad domains from site (but NOT facebook/instagram)
  mutate(site = case_when(
    is.na(site_original) ~ NA_character_,
    str_detect(site_original, str_c(unwanted_domains, collapse = "|")) ~ NA_character_,
    str_detect(site_original, "facebook\\.com|instagram\\.com") ~ NA_character_,
    TRUE ~ site_original
  )) %>%
  
  # Step 3: Normalize to www. format if needed
  mutate(site = case_when(
    is.na(site) ~ NA_character_,
    str_detect(site, "^https?://www\\.") ~ site,
    str_detect(site, "^https?://") ~ str_replace(site, "^(https?://)([^/]+)", "\\1www.\\2"),
    str_detect(site, "^www\\.") ~ str_c("http://", site),
    TRUE ~ str_c("http://www.", site)
  )) %>%
  select(-site_original)

# ---- Final Robust Vectorized Normalization ----
normalize_instagram <- Vectorize(function(url) {
  if (is.na(url)) return(NA_character_)
  
  clean_url <- url %>%
    str_trim() %>%
    str_replace_all("//+", "/") %>%
    str_replace_all(".*instagram\\.com/+", "") %>%
    str_remove("\\?.*$") %>%
    str_remove("/$") %>%
    str_remove("^@") %>%
    str_trim()
  
  # Hardcoded invalid handles
  invalid_handles <- c("coachbenstein", "mrget2closing", "usuario%20de%20instagram")
  
  if (clean_url == "" || str_detect(clean_url, "login|about|facebook|@|\\.com|\\s|/") || str_to_lower(clean_url) %in% invalid_handles) {
    return(NA_character_)
  }
  
  glue("https://www.instagram.com/{clean_url}")
})

normalize_facebook <- Vectorize(function(url) {
  if (is.na(url)) return(NA_character_)
  
  clean_url <- url %>%
    str_trim() %>%
    str_replace_all("//+", "/") %>%
    str_replace_all(".*facebook\\.com/+", "") %>%
    str_remove("\\?.*$") %>%
    str_remove("/about/?$") %>%
    str_remove("/$") %>%
    str_trim()
  
  # Hardcoded invalid handles
  invalid_handles <- c("etzelliat", "joelsmotor", "nurseceolife")
  
  if (clean_url == "" || str_detect(clean_url, "login|pages|search|\\.com|\\s|/") || str_to_lower(clean_url) %in% invalid_handles) {
    return(NA_character_)
  }
  
  glue("https://www.facebook.com/{clean_url}")
})

# ---- Apply normalization and fill IG/FB columns ----
df <- df %>%
  mutate(
    instagram = coalesce(
      normalize_instagram(instagram),
      normalize_instagram(site_instagram_candidate)
    ),
    facebook = coalesce(
      normalize_facebook(facebook),
      normalize_facebook(site_facebook_candidate)
    )
  ) %>%
  select(-site_instagram_candidate, -site_facebook_candidate)

# ---- Manual Overrides: Specific place_ids ----
df <- df %>%
  mutate(
    facebook = case_when(
      place_id == "ChIJjY4SOaZ1BZERRHxcdzy-H4Y" ~ "https://www.facebook.com/people/Adamu-MMA/61555242338472/",
      place_id == "ChIJzwKq4Mi5BZER0d4ZhuG1mXA" ~ "https://www.facebook.com/p/INTI-WALLA-PERU-100063763851955/",
      place_id == "ChIJp8E3VQ3PBZERnloYE0DqDw4" ~ "https://www.facebook.com/p/Academia-Salmista-MMA-100050883829326/",
      place_id == "ChIJfR7pXMrRBZERFdTSKfVDIKg" ~ "https://www.facebook.com/teamchavezperu/?locale=es_LA",
      TRUE ~ facebook
    ),
    instagram = case_when(
      place_id == "ChIJjY4SOaZ1BZERRHxcdzy-H4Y" ~ "https://www.instagram.com/adamu.mma/",
      place_id == "ChIJzwKq4Mi5BZER0d4ZhuG1mXA" ~ "https://www.instagram.com/explore/locations/2281100328806244/inti-walla-peru/",
      place_id == "ChIJp8E3VQ3PBZERnloYE0DqDw4" ~ "https://www.instagram.com/salmistasmma/",
      place_id == "ChIJfR7pXMrRBZERFdTSKfVDIKg" ~ "https://www.instagram.com/teamchavez_tcv/",
      TRUE ~ instagram
    )
  )

# ---- WhatsApp cleaning and synthetic generation ----

# Helper function to synthesize link from phone
generate_whatsapp_link <- function(phone) {
  if (is.na(phone)) return(NA_character_)
  digits <- str_replace_all(phone, "[^0-9]", "")
  digits <- str_replace(digits, "^51", "")
  if (str_detect(digits, "^9\\d{8}$")) {
    return(glue::glue("https://wa.me/51{digits}"))
  }
  return(NA_character_)
}

# Step 1: Preserve original and generate synthetic
df <- df %>%
  mutate(
    whatsapp_original  = whatsapp,
    whatsapp_synthetic = sapply(phone, generate_whatsapp_link)
  )

# Step 2: Manual corrections (hardcoded, known issues)
df <- df %>%
  mutate(
    whatsapp_corrected = case_when(
      whatsapp_original == "https://wa.me/993795427"         ~ "https://wa.me/51993795427",
      whatsapp_original == "wa.link/lvs6vh"                  ~ "https://wa.me/51993175009",
      whatsapp_original == "bit.ly/INFORMES-MCC"             ~ "https://wa.me/51972102849",
      whatsapp_original == "https://wa.me/5491138071955"     ~ NA_character_,
      whatsapp_original == "http://wa.me/+919201444044"      ~ "https://wa.me/51971758785",
      whatsapp_original == "http://wa.me/51997430784"        ~ "https://wa.me/51997430784",
      TRUE ~ whatsapp_original
    )
  )

# Step 3: Normalize final link and flag source
df <- df %>%
  mutate(
    whatsapp = coalesce(whatsapp_corrected, whatsapp_synthetic),
    whatsapp = str_replace(whatsapp, "^http://", "https://"),
    whatsapp = str_replace(whatsapp, "https://wa.me/\\+?", "https://wa.me/"),
    whatsapp_source = case_when(
      !is.na(whatsapp_corrected) ~ "original_or_corrected",
      is.na(whatsapp_corrected) & !is.na(whatsapp_synthetic) ~ "synthetic",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-whatsapp_original, -whatsapp_synthetic, -whatsapp_corrected)

#address <- df %>% select(place_id, full_address)
#write_csv(
#  address,
#  file = "/Users/lescobedo/dev/bjjnearme.org/data/interim/preliminary_addresses.csv",
#  na = ""
#)


# ---- Final clean-up: normalize blanks, curate export ----
df <- df %>%
  mutate(across(
    c(place_id, site, phone, email, whatsapp, instagram, facebook),
    ~ .x %>% str_trim() %>% na_if("") %>% na_if("NA") %>% na_if("null")
  ))

contact_channels_cleaned <- df %>%
  transmute(
    place_id, name, site, phone, email, whatsapp, instagram, facebook,
    cleaned_on = Sys.Date()
  ) %>%
  distinct()

# ---- Save output ----
write_csv(contact_channels_cleaned, output_file)
message(glue("✅ Cleaned contacts saved: {output_file}"))
