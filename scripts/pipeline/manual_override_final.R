rm(list = ls())

library(tidyverse)
library(readxl)
library(glue)

# ---- Paths ----
base_dir <- "/Users/lescobedo/Downloads/fahad_data"

path_addresses <- file.path(base_dir, "modified_addresses_manual.xlsx")
path_taxonomies <- file.path(base_dir, "data_with_taxonomies.xlsx")
#path_final      <- file.path(base_dir, "places_lima_google_final_20250801.csv")

# ---- Load Data ----
df_addresses  <- read_excel(path_addresses, sheet = "Sheet1")
df_taxonomies <- read_excel(path_taxonomies)
#df_final      <- read_csv(path_final, show_col_types = FALSE)

# ---- Select Data ----

library(tidyverse)

# Mapping for replacements
tag_map <- c(
  "clases para adultos"       = "adultos",
  "clases para niños"         = "niños",
  "clases para mujeres"       = "mujeres",
  "open mat"                  = "open mat",
  "cuota para visitantes"     = "cuota para visitantes",
  "clases para principiantes" = "principiantes",
  "clases para jovenes"       = "jóvenes",
  "entrada accesible"         = "entrada accesible",
  "habla inglés"              = "habla inglés",
  "habla portugués"           = "habla portugués",
  "clase con kimono"          = "kimono",
  "clase sin kimono"          = "sin kimono"
)

df_taxonomies <- df_taxonomies %>%
  mutate(
    taxonomy = str_replace_all(taxonomy, tag_map)
  ) %>% 
  select(-name, -district)


# -- join data
df_joined <- df_taxonomies %>%
  left_join(df_addresses, by = "place_id") %>% 
  select(place_id,
         name,
         full_address,
         district,
         taxonomy,
         whatsapp_number,
         instagram,
         facebook,
         overall_rating,
         num_reviews,
         reviews_link,
         photo_link,     
         logo_link,
         full_address)

df_final <- df_joined %>%
  filter(!place_id %in% c(
    "ChIJf9J7Su7QBZER5OjNsKwQDeI", 
    "ChIJDYJJFte9BZERJbEyFnQ5e9I", 
    "ChIJ-Y9xE8PFBZERYnNQ_wmVuXg", 
    "ChIJvacSRQDVBZERBxHZNVWjEjw",
    "ChIJWcjah627BZERr3KaEviXWL4"
  ))

library(writexl)

# Example: save merged data
write_xlsx(
  df_final, 
  path = "/Users/lescobedo/Downloads/fahad_data/final_data_fahad_08082025.xlsx"
)

