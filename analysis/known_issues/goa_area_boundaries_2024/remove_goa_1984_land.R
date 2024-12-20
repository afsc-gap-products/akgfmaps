# Remove land area from GOA survey area

library(sf)
library(dplyr)

goa_area_without_land <- sf::st_read(here::here("analysis", "known_issues", "goa_area_boundaries_2024", "goa_strata.shp"), quiet = TRUE) |>
  dplyr::filter(STRATUM > 0) |>
  dplyr::group_by(REGION = "GOA") |>
  dplyr::summarise(AREA_KM2 = sum(AREA_KM2))

goa_area_without_land |>
  sf::st_write(here::here("inst", "extdata", "goa_area.shp"),
               append = FALSE)
