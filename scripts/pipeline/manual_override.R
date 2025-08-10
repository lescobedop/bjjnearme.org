rm(list = ls())


library(tidyverse)
library(readxl)
library(here)
library(readr)


# Base exported file
df_base <- read_csv(here("data/exports/wide", "places_lima_google_FINAL_LATEST.csv"), show_col_types = FALSE)
names(df_base)

# Manual data from Excel (Sheet 3)
df_manual <- read_excel("/Users/lescobedo/dev/bjjnearme.org/data/processed/Lima_Names_Tags_About_Manual_Work.xlsx", sheet = "Sheet3")

#df_reviews_manual <-  read_excel("/Users/lescobedo/dev/bjjnearme.org/data/processed/Lima_Names_Tags_About_Manual_Work.xlsx", sheet = "Sheet4")
#df_reviews_manual <- df_reviews_manual %>% 
#  select(place_id, 
#         name_v3 = name, 
#         rating_v3 = rating, 
#         reviews_v3 = reviews, 
#         reviews_link_v3 = reviews_link,
#         photo_v3 = photo,
#         street_view_v3 = street_view, 
#         logo_v3 = logo, 
#         location_link_v3 = location_link, 
#         location_reviews_lin_v3 = location_reviews_link)


df_joined <- df_manual %>%
  left_join(df_base, by = "place_id") %>% 
  select(place_id,
         name,
         name_v2,
         city,
         city_v2,
         lat,
         lon,
         has_kids_classes,
         has_kids_classes_v2,
         has_women_classes,
         has_women_classes_v2,
         has_open_mat,
         has_open_mat_v2,
         drop_in_fee,
         drop_in_fee_v2,
         beginner_friendly,
         beginner_friendly_v2,
         family_friendly,
         family_friendly_v2,
         accessible_entrance,
         accessible_entrance_v2,
         speaks_english,
         speaks_english_v2,
         speaks_portuguese,
         speaks_portuguese_v2,
         offers_gi,
         offers_gi_v2,
         offers_no_gi,
         offers_no_gi_v2,
         overall_rating,
         num_reviews,
         reviews_link,
         photo_link,
         logo_link,
         whatsapp_number,
         whatsapp_number_v2,
         instagram,
         instagram_v2,
         facebook,
         facebook_v2,
         about
  )

df_joined <- df_joined %>% 
  select(place_id,
          name = name_v2,
          district = city_v2,
          lat,
          lon,
          has_kids_classes = has_kids_classes_v2,
          has_women_classes = has_women_classes_v2,
          has_open_mat = has_open_mat_v2,
          drop_in_fee = drop_in_fee_v2,
          beginner_classes = beginner_friendly_v2,
          family_friendly = family_friendly_v2,
          accessible_entrance = accessible_entrance_v2,
          speaks_english = speaks_english_v2,
          speaks_portuguese = speaks_portuguese_v2,
          gi_class = offers_gi_v2,
          no_gi_class = offers_no_gi_v2,
          whatsapp_number = whatsapp_number_v2,
          instagram = instagram_v2,
          facebook = facebook_v2,
          about,
          overall_rating,
          num_reviews,
          reviews_link,
          photo_link,
          logo_link)


df_joined <- df_joined %>%
  mutate(
    lat = if_else(place_id == "ChIJbehiZfDHBZERxD71uXfBsvE", -12.11076834, lat),
    lon = if_else(place_id == "ChIJbehiZfDHBZERxD71uXfBsvE", -76.97290805, lon),
    
    lat = if_else(place_id == "ChIJCUA-HwDHBZER258MJeLRb9w", -12.095649, lat),
    lon = if_else(place_id == "ChIJCUA-HwDHBZER258MJeLRb9w", -76.9914308, lon),
    
    lat = if_else(place_id == "ChIJEWiEAQDJBZERwMVx0cUndZY", -12.1137564, lat),
    lon = if_else(place_id == "ChIJEWiEAQDJBZERwMVx0cUndZY", -77.0120942, lon)
  )


df_joined <- df_joined %>%
  mutate(
    # --- Manual name override
    name = if_else(place_id == "ChIJ2_uPUwDJBZERQmMjPUSEARI", "Studio Seven Jiu Jitsu Club", name),
    
    # --- Manual overrides for overall_rating
    overall_rating = case_when(
      place_id == "ChIJCUA-HwDHBZER258MJeLRb9w" ~ 5,
      place_id == "ChIJkwetawDPBZERe4-TTmC1ho0" ~ 5,
      place_id == "ChIJEWiEAQDJBZERwMVx0cUndZY" ~ 5,
      TRUE ~ overall_rating
    ),
    
    # --- Manual overrides for num_reviews
    num_reviews = case_when(
      place_id == "ChIJbehiZfDHBZERxD71uXfBsvE" ~ 34,
      place_id == "ChIJCUA-HwDHBZER258MJeLRb9w" ~ 1,
      place_id == "ChIJkwetawDPBZERe4-TTmC1ho0" ~ 1,
      place_id == "ChIJEWiEAQDJBZERwMVx0cUndZY" ~ 19,
      TRUE ~ num_reviews
    ),
    
    # --- Manual overrides for reviews_link
    reviews_link = case_when(
      place_id == "ChIJCUA-HwDHBZER258MJeLRb9w" ~ "https://search.google.com/local/reviews?placeid=ChIJCUA-HwDHBZER258MJeLRb9w&authuser=0&hl=en&gl=UA",
      place_id == "ChIJEWiEAQDJBZERwMVx0cUndZY" ~ "https://search.google.com/local/reviews?placeid=ChIJEWiEAQDJBZERwMVx0cUndZY&q=Aether+jiu+jitsu,+Lima+Peru&authuser=0&hl=en&gl=PE",
      TRUE ~ reviews_link
    ),
    
    # --- Manual overrides for photo_link
    photo_link = case_when(
      place_id == "ChIJEWiEAQDJBZERwMVx0cUndZY" ~ "https://lh3.googleusercontent.com/gps-cs-s/AC9h4noOfpgGNEBA2SwevXLrao0AJytm3FbkOyMwnic5eYaCKru8_mKVdT14OfSt2MHeBBNchGZy3XVHaUvGPCh90jMcHln8cerNvlJaTzR2kivdVDTEUTBg0DKJ-jHUj6v7IcvGfQM=w800-h500-k-no",
      place_id == "ChIJCUA-HwDHBZER258MJeLRb9w" ~ "https://streetviewpixels-pa.googleapis.com/v1/thumbnail?panoid=gSY-XSpElYY3bllxXSk9Ag&cb_client=search.gws-prod.gps&w=800&h=500&yaw=202.90277&pitch=0&thumbfov=100",
      place_id == "ChIJCZ9ET-npBZERmwkARU697XA" ~ "https://streetviewpixels-pa.googleapis.com/v1/thumbnail?panoid=W5QqEWWmvMg027nPgkjZ9Q&cb_client=search.gws-prod.gps&w=800&h=500&yaw=160.82365&pitch=0&thumbfov=100",
      place_id == "ChIJe58qHQDPBZERcOlRkfXry8o" ~ "https://streetviewpixels-pa.googleapis.com/v1/thumbnail?panoid=5CzMmYoWI9z7uxdSspPExg&cb_client=search.gws-prod.gps&w=800&h=500&yaw=76.29447&pitch=0&thumbfov=100",
      place_id == "ChIJ-V1U4WiZBZEREqlFW69BUjs" ~ "https://lh3.googleusercontent.com/p/AF1QipOCYYBcdZZEzbJvAFG-GedREAYtepdz-OGgfoti=w800-h500-k-no",
      place_id == "ChIJN3QsxCbPBZERgR2YabPmQbA" ~ "https://streetviewpixels-pa.googleapis.com/v1/thumbnail?panoid=pKNT56Zo4gVOyJCLWeMb7g&cb_client=search.gws-prod.gps&w=800&h=500&yaw=356.10776&pitch=0&thumbfov=100",
      place_id == "ChIJNwNYneXPBZERvnx_1rWKLmQ" ~ "https://streetviewpixels-pa.googleapis.com/v1/thumbnail?panoid=kox02NBxnQqqW3xTznO38Q&cb_client=search.gws-prod.gps&w=800&h=500&yaw=320.21985&pitch=0&thumbfov=100",
      place_id == "ChIJcS5KKr_QBZERqHtSYCUJARg" ~ "https://streetviewpixels-pa.googleapis.com/v1/thumbnail?panoid=1wy8WN4jOhjkVn26tSiZ3Q&cb_client=search.gws-prod.gps&w=800&h=500&yaw=149.10118&pitch=0&thumbfov=100",
      place_id == "ChIJdyuWXifRBZERL7akeaIyRfA" ~ "https://streetviewpixels-pa.googleapis.com/v1/thumbnail?panoid=Zk10Dstht1lwCbmK7uT9oA&cb_client=search.gws-prod.gps&w=800&h=500&yaw=104.91051&pitch=0&thumbfov=100",
      place_id == "ChIJf9J7Su7QBZER5OjNsKwQDeI" ~ "https://streetviewpixels-pa.googleapis.com/v1/thumbnail?panoid=yQudlpjy-Ldrz0PhnsQ8wg&cb_client=search.gws-prod.gps&w=800&h=500&yaw=147.6795&pitch=0&thumbfov=100",
      TRUE ~ photo_link
    ),
    
    # --- Mark deletion
    to_delete = place_id == "ChIJtdJazrDh8YsRSc-mT_xbFvs"
  ) %>%
  filter(!to_delete) %>%
  select(-to_delete)

df_joined <- df_joined %>%
  mutate(has_adult_classes = TRUE)


write_csv(
  df_joined,
  file = "/Users/lescobedo/dev/bjjnearme.org/data/exports/wide/places_lima_google_final_20250801.csv",
  na = ""
)

