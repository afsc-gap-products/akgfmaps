# Remove land area from AI survey area

library(sf)
library(dplyr)

ai_area_without_land <- sf::st_read(here::here("analysis", "known_issues", "ai_area_boundaries_2024", "ai_strata.shp"), quiet = TRUE) |>
  dplyr::filter(STRATUM > 0 & STRATUM < 800) |>
  dplyr::group_by(REGION = "AI") |>
  dplyr::summarise(AREA_KM2 = sum(AREA_KM2))

ai_area_without_land |>
  sf::st_write(here::here("inst", "extdata", "ai_area.shp"),
               append = FALSE)


plot(ai_area_without_land)
