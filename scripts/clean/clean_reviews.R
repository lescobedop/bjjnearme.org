# clean_reviews.R

rm(list = ls())

# ---- Load Libraries ----
library(tidyverse)
library(janitor)
library(glue)

# ---- Load Data ----
input_file <- "data/interim/places_lima_google_combined_20250713.csv"
df <- read_csv(input_file, show_col_types = FALSE) %>% clean_names()

# ---- Select and Clean Relevant Columns ----
reviews_clean <- df %>%
  transmute(
    place_id = place_id,
    rating = as.numeric(rating),
    reviews = as.integer(reviews),
    reviews_link = if_else(
      !is.na(place_id),
      glue("https://search.google.com/local/reviews?placeid={place_id}"),
      NA_character_
    ),
    location_link = location_link,
    location_reviews_link = location_reviews_link
  )

# ---- Enrich with Trust Level ----
reviews_clean <- reviews_clean %>%
  mutate(
    total_reviews = as.numeric(reviews),
    avg_rating = as.numeric(rating),
    
    trust_level = case_when(
      is.na(avg_rating) | is.na(total_reviews) ~ NA_character_,
      total_reviews < 10                       ~ "Limited Trust",
      total_reviews < 30 & avg_rating >= 4.5   ~ "Emerging Trust",
      total_reviews >= 30 & avg_rating >= 4.2  ~ "Consistent Trust",
      total_reviews >= 50 & avg_rating >= 4.5  ~ "High Trust",
      TRUE                                     ~ "Low Trust"
    )
  )

reviews_clean <- reviews_clean %>%
  mutate(
    review_health_status = case_when(
      is.na(total_reviews) | total_reviews == 0 ~ "No Reviews Yet",
      total_reviews < 10                        ~ "Needs More Reviews",
      total_reviews >= 10                       ~ "Well Reviewed"
    ),
    
    review_growth_tip = case_when(
      is.na(total_reviews) | total_reviews == 0 ~ "Start asking your students to leave a review",
      total_reviews < 10                        ~ "Encourage more students to leave reviews",
      total_reviews >= 10                       ~ NA_character_
    ),
    
    review_kudos = case_when(
      total_reviews >= 50 & avg_rating >= 4.5 ~ "Excellent review reputation — your community shows up!",
      total_reviews >= 30 & avg_rating >= 4.2 ~ "Strong community feedback — keep it up!",
      total_reviews >= 10 & avg_rating >= 4.0 ~ "Good engagement — your presence is growing",
      TRUE                                    ~ NA_character_
    ),
    
    review_response_needed = case_when(
      is.na(avg_rating) | is.na(total_reviews)     ~ NA_character_,
      avg_rating < 4.0 & total_reviews >= 5        ~ "Consider responding to feedback",
      TRUE                                         ~ NA_character_
    )
  )


# ---- Export ----
output_file <- "data/exports/wide/reviews_lima_google_20250714.csv"
write_csv(reviews_clean, output_file)

message("✅ Reviews export complete: ", output_file)
