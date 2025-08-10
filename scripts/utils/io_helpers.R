# scripts/helpers/io_helpers.R
# Purpose: Centralize common input/output paths for easy reuse

rm(list = ls())

library(here)
library(glue)

# ---- Set Date ----
output_date <- format(Sys.Date(), "%Y%m%d")

# ---- Input Paths ----
path_interim <- here("data", "interim")
path_processed <- here("data", "processed")

# ---- Output Paths ----
path_long <- here("data", "exports", "long")
path_wide <- here("data", "exports", "wide")

# ---- Named Files ----
get_export_filename <- function(name, type = c("long", "wide"), prefix = "", suffix = "", extension = "csv") {
  type <- match.arg(type)
  dir <- if (type == "long") path_long else path_wide
  file <- glue("{prefix}{name}{suffix}_{output_date}.{extension}")
  return(file.path(dir, file))
}

# ---- Example Usage in Scripts ----
# output_long <- get_export_filename("working_hours_lima_google", type = "long")
# output_wide <- get_export_filename("about_tags_wide_mvp", type = "wide")
