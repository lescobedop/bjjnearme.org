rm(list = ls())

library(tidyverse)
library(readxl)
library(glue)
library(writexl)


# ---- Paths ----
base_dir <- "/Users/lescobedo/Downloads/fahad_data"

path_newlinks <- file.path(base_dir, "Outscraper-20250809125335xs21.xlsx")
path_data_to_review <- file.path(base_dir, "final_data_fahad_08082025.xlsx")

# ---- Load Data ----
df_links  <- read_excel(path_newlinks)
df_toreview <- read_excel(path_data_to_review)

# ---- Issues ---
df_links <- df_links %>% 
   select(place_id, reviews_link, street_view, photo, logo)

df_toreview <- df_toreview %>% 
  select(place_id, original_link=reviews_link, photo_link, logo_link)

# ---- Join
df_link_joined <- df_toreview %>%
  left_join(df_links, by = "place_id") %>% 
  select(place_id,
         original_link, 
         reviews_link, 
         photo_link, 
         photo, 
         logo_link, 
         logo, 
         street_view)


# ---- Saving
write_xlsx(
  df_link_joined, 
  path = "/Users/lescobedo/Downloads/fahad_data/compare_links_manually.xlsx"
)








         
         