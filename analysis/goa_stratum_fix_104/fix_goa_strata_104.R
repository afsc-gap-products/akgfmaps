# Fix GOA stratum shapefile
# January 2, 2025
# Sean Rohan
#
# The official GOA stratum shapefile erroneously had a gap around Samalga Island. Consequently,
# the shapefile area for stratum 10 did not match the stratum areas in RACEBASE or GAP_PRODUCTS.
# This code uses the GOA survey 1984 grid to generate a GOA stratum shapefile for the 1984 GOA
# survey design.

library(akgfmaps)
library(navmaps)

goa_strata_new <- sf::st_read(system.file("extdata/goa_grid.shp", package = "akgfmaps")) |>
  sf::st_transform(crs = "EPSG:3338") |>
  dplyr::filter(STRATUM > 0) |>
  dplyr::select(STRATUM, AREA_KM2) |>
  dplyr::group_by(STRATUM) |>
  dplyr::summarise(do_union = TRUE,
                   AREA_KM2 = sum(AREA_KM2)) |>
  dplyr::ungroup() |>
  dplyr::mutate(SF_AREA_KM2 = as.numeric(sf::st_area(geometry)/1e6))

channel <- navmaps::get_connected(schema = "AFSC")

gp_stratum_areas <- RODBC::sqlQuery(channel = channel,
                          query = paste0("SELECT SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID AS STRATUM, AREA_NAME, AREA_TYPE AS GP_AREA_TYPE, AREA_KM2 AS GP_AREA_KM2 FROM GAP_PRODUCTS.AREA
  WHERE SURVEY_DEFINITION_ID = 47 AND DESIGN_YEAR = 1984 AND AREA_TYPE = 'STRATUM'")
)

area_diff <- sf::st_drop_geometry(goa_strata_new) |>
  dplyr::right_join(gp_stratum_areas, by = "STRATUM") |>
  dplyr::mutate(DIFF_AREA_SHP_GP = AREA_KM2 - GP_AREA_KM2,
                DIFF_AREA_SF_GP = SF_AREA_KM2 - GP_AREA_KM2,
                PCT_DIFF_SHP = (AREA_KM2 - GP_AREA_KM2)/GP_AREA_KM2 * 100,
                PCT_DIFF_SF = (SF_AREA_KM2 - GP_AREA_KM2)/GP_AREA_KM2 * 100)

goa_strata_new |>
  dplyr::select(STRATUM) |>
  dplyr::left_join(gp_stratum_areas |>
                     dplyr::select(STRATUM, AREA_KM2 = GP_AREA_KM2)) |>
  dplyr::select(STRATUM, AREA_KM2) |>
  sf::st_write(here::here("analysis", "goa_stratum_fix_104", "goa_strata.shp"))

# goa_strata <- sf::st_read(system.file("extdata/goa_strata.shp", package = "akgfmaps"))



