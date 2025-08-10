rm(list = ls())

library(readr)
library(dplyr)
library(purrr)
library(stringr)

# Load your wide-format data
df <- read_csv("/Users/lescobedo/dev/bjjnearme.org/data/exports/wide/places_lima_google_final_20250801.csv", show_col_types = FALSE)
address <- read_csv("/Users/lescobedo/dev/bjjnearme.org/data/interim/preliminary_addresses.csv", show_col_types = FALSE)

# Define Spanish taxonomy mapping
#tag_map <- c(
#  has_adult_classes     = "clases para adultos",
#  has_kids_classes      = "clases para niños",
#  has_women_classes     = "clases para mujeres",
#  has_open_mat          = "open mat",
#  drop_in_fee           = "cuota para visitantes",
#  beginner_classes      = "clases para principiantes",
#  family_friendly       = "clases para jovenes",
#  accessible_entrance   = "entrada accesible",
#  speaks_english        = "habla inglés",
#  speaks_portuguese     = "habla portugués",
#  gi_class              = "clase con kimono",
#  no_gi_class           = "clase sin kimono"
#)

tag_map <- c(
  has_adult_classes     = "adultos",
  has_kids_classes      = "niños",
  has_women_classes     = "mujeres",
  has_open_mat          = "open mat",
  drop_in_fee           = "cuota para visitantes",
  beginner_classes      = "principiantes",
  family_friendly       = "jóvenes",
  accessible_entrance   = "entrada accesible",
  speaks_english        = "habla inglés",
  speaks_portuguese     = "habla portugués",
  gi_class              = "kimono",
  no_gi_class           = "sin kimono"
)


# Check that all required columns exist
missing_cols <- setdiff(names(tag_map), names(df))
if (length(missing_cols) > 0) {
  stop(paste("Missing columns in the data:", paste(missing_cols, collapse = ", ")))
}

# Normalize flags to logical (in case of NA)
df <- df %>%
  mutate(across(all_of(names(tag_map)), ~ if_else(is.na(.x), FALSE, .x)))

# Create taxonomy column
df <- df %>%
  rowwise() %>%
  mutate(
    taxonomy = names(tag_map)[which(c_across(all_of(names(tag_map))) == TRUE)] %>%
      map_chr(~ tag_map[[.x]]) %>%
      str_c(collapse = ", ")
  ) %>%
  ungroup()


## Getting addresess 
#df_addresses <- df %>% select(place_id, name, lat, lon)
#write_csv(
#  df_addresses,
#  file = "/Users/lescobedo/dev/bjjnearme.org/data/interim/lat_long_addreses.csv", na = ""
#)


# Drop source columns used in taxonomy + lat/lon
df <- df %>%
  select(-all_of(names(tag_map)), -lat, -lon)

# Reorder columns as specified
df <- df %>%
  select(
    place_id,
    name,
    about,
    #district,
    taxonomy,
    whatsapp_number,
    instagram,
    facebook,
    overall_rating,
    num_reviews,
    reviews_link,
    photo_link,
    logo_link
  )


write_csv(
  df,
  file = "/Users/lescobedo/dev/bjjnearme.org/data/interim/data_with_taxonomies.csv", na = ""
)

write_csv(
  df,
  file = "/Users/lescobedo/Downloads/fahad_data/data_with_taxonomies.csv", na = ""
)

#df <- df %>%
#  left_join(address, by = "place_id") %>% 
#  select(place_id, name, full_address, district)

#write.csv(
#  df,
#  file = "/Users/lescobedo/dev/bjjnearme.org/data/interim/modified_addresses.csv",
#  fileEncoding = "UTF-16LE", row.names = FALSE, na = ""
#)

#write.csv(df, file = "modified_addresses_excel.csv", fileEncoding = "UTF-16LE", row.names = FALSE, na = "")




write.csv(
  df,
  file = "/Users/lescobedo/dev/bjjnearme.org/data/interim/modified_addresses.csv",
  fileEncoding = "UTF-16LE", row.names = FALSE, na = ""
)

