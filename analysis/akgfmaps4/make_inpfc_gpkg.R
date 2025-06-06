# Create INPFC stratum geopackage
# Use survey stratum polygons from akgfmaps 3.6.2 to create INPFC polygons
# Sean Rohan
# January 2, 2024

library(akgfmaps)

goa_inpfc <- get_inpfc_strata(
  select.region = "goa",
  set.crs = "EPSG:3338"
) |>
  dplyr::select(AREA_NAME = INPFC_STRATUM) |>
  dplyr::mutate(SURVEY_DEFINITION_ID = 47,
                DESIGN_YEAR = 1984,
                AREA_M2 = as.numeric(sf::st_area(geometry))) |>
  dplyr::inner_join(data.frame(AREA_NAME =
                                 c("Chirikof",
                                   "Kodiak",
                                   "Shumagin",
                                   "Southeastern",
                                   "Yakutat"),
                               AREA_ID = c(929, 939, 919, 959, 949),
                               AREA_TYPE = "INPFC"),
                    by = "AREA_NAME") |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_NAME, AREA_ID, AREA_M2)

ai_inpfc <- get_inpfc_strata(
  select.region = "ai",
  set.crs = "EPSG:3338"
) |>
  dplyr::select(AREA_NAME = INPFC_STRATUM) |>
  dplyr::mutate(SURVEY_DEFINITION_ID = 52,
                DESIGN_YEAR = 1991,
                AREA_M2 = as.numeric(sf::st_area(geometry))) |>
  dplyr::inner_join(data.frame(AREA_NAME =
                                 c("Western Aleutians",
                                   "Southern Bering Sea",
                                   "Central Aleutians",
                                   "Eastern Aleutians"),
                               AREA_ID = c(299, 799, 6499, 5699),
                               AREA_TYPE = "INPFC"),
                    by = "AREA_NAME") |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_NAME, AREA_ID, AREA_M2)


sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
             layer = "inpfc_strata",
             append = FALSE,
             delete_dsn = TRUE)
