##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Creation of DESIGN_YEAR 2024 GOA survey area and stations
## Author:        Sean Rohan (sean.rohan@noaa.gov)
## Description:   Union 1984 strata that fall within 2025 survey design year to
##                make 2024 survey area. Intersect 1984 station grid with 2024
##                survey area to make 2024 station grid.
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create survey area and station features for 2024 GOA DESIGN_YEAR

library(akgfmaps) # v.4.1.0

# Get GOA 1984 data
goa_1984 <-
  akgfmaps::get_base_layers(select.region = "goa", design.year = 1984, set.crs = "EPSG:3338")

file.copy(
  from = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
  to = here::here("analysis", "goa_strata_2024", "afsc_bottom_trawl_surveys.gpkg"),
  overwrite = FALSE
)

# Retrieve new 2024 strata
goa_strata_2024 <-
  sf::st_read(
    dsn = here::here("analysis", "goa_strata_2024", "goa_strata_2024.gpkg")
  ) |>
  dplyr::mutate(AREA_TYPE = "STRATUM") |>
  dplyr::select(
    AREA_TYPE,
    SURVEY_DEFINITION_ID,
    DESIGN_YEAR,
    AREA_ID = STRATUM, # Reclassify STRATUM as AREA_ID to match gpkg and GAP_PRODUCTS
    AREA_M2,
    geom
    )

head(goa_strata_2024)

# Union stratum polygons to obtain survey area
goa_area_2024 <-
  goa_strata_2024 |>
  dplyr::select(-AREA_M2, -AREA_ID, -AREA_TYPE) |>
  dplyr::group_by(SURVEY_DEFINITION_ID, DESIGN_YEAR) |>
  dplyr::summarise(do_union = TRUE) |>
  dplyr::mutate(SURVEY_NAME = "Gulf of Alaska Bottom Trawl Survey",
                AREA_TYPE = "REGION",
                AREA_ID = 99903,
                AREA_M2 = as.numeric(sf::st_area(geom))) |>
  dplyr::select(
    AREA_TYPE,
    SURVEY_NAME,
    SURVEY_DEFINITION_ID,
    DESIGN_YEAR,
    AREA_ID,
    AREA_M2,
    geom
  )

head(goa_area_2024)

ggplot() +
  geom_sf(data = goa_1984$survey.area,
          mapping = aes(fill = factor(DESIGN_YEAR))) +
  geom_sf(data = goa_area_2024,
          mapping = aes(fill = factor(DESIGN_YEAR)),
          color = NA) +
  scale_fill_viridis_d()


# Make stations by intersecting the 1984 station grid with the 2024 survey area
goa_stations_2024 <-
  goa_1984$survey.grid |>
  sf::st_intersection(
    dplyr::select(goa_area_2024,
                  SURVEY_NAME)
  ) |>
  dplyr::mutate(
    DESIGN_YEAR = 2024,
    AREA_TYPE = "STATION"
  ) |>
  dplyr::select(
    AREA_TYPE,
    SURVEY_DEFINITION_ID,
    DESIGN_YEAR,
    AREA_ID = STRATUM,
    GRID_ID,
    STATION,
    AREA_M2
  ) |>
  sf::st_set_geometry("geom")

head(goa_stations_2024)

sum(sf::st_area(goa_1984$survey.grid))

sum(sf::st_area(goa_stations_2024))

ggplot() +
  geom_sf(data = goa_1984$survey.grid,
          mapping = aes(fill = factor(DESIGN_YEAR)),
          color = NA) +
  geom_sf(data = goa_stations_2024,
          mapping = aes(fill = factor(DESIGN_YEAR)),
          color = NA) +
  scale_fill_viridis_d()

# Add 2024 DESIGN_YEAR layers to the geopackage - start with afsc_bottom_trawl_surveys.gpkg from
# akgfmaps version <= 4.0.8

sf::st_write(
  goa_area_2024,
  dsn = here::here("analysis", "goa_strata_2024", "afsc_bottom_trawl_surveys.gpkg"),
  layer = "survey_area",
  append = TRUE,
  delete_dsn = FALSE)

sf::st_write(
  goa_strata_2024,
  dsn = here::here("analysis", "goa_strata_2024", "afsc_bottom_trawl_surveys.gpkg"),
  layer = "survey_strata",
  append = TRUE,
  delete_dsn = FALSE)

sf::st_write(
  goa_stations_2024,
  dsn = here::here("analysis", "goa_strata_2024", "afsc_bottom_trawl_surveys.gpkg"),
  layer = "survey_grid",
  append = TRUE,
  delete_dsn = FALSE
)

# Divide layers among GeoPackages
all_survey_areas <-
  sf::st_read(dsn = here::here("analysis", "goa_strata_2024", "afsc_bottom_trawl_surveys.gpkg"),
              layer = "survey_area")

all_survey_strata <-
  sf::st_read(dsn = here::here("analysis", "goa_strata_2024", "afsc_bottom_trawl_surveys.gpkg"),
              layer = "survey_strata")

all_survey_stations <-
  sf::st_read(dsn = here::here("analysis", "goa_strata_2024", "afsc_bottom_trawl_surveys.gpkg"),
              layer = "survey_grid")

inpfc_strata <-
  sf::st_read(dsn = here::here("analysis", "goa_strata_2024", "afsc_bottom_trawl_surveys.gpkg"),
              layer = "inpfc_strata")

sf::st_write(
  all_survey_strata,
  dsn = here::here("analysis", "goa_strata_2024", "afsc_bts_strata.gpkg"),
  layer = "survey_strata",
  append = FALSE,
  delete_dsn = FALSE
)

sf::st_write(
  all_survey_areas,
  dsn = here::here("analysis", "goa_strata_2024", "afsc_bts_strata.gpkg"),
  layer = "survey_area",
  append = TRUE,
  delete_dsn = FALSE
)

sf::st_write(
  inpfc_strata,
  dsn = here::here("analysis", "goa_strata_2024", "afsc_bts_strata.gpkg"),
  layer = "inpfc_strata",
  append = TRUE,
  delete_dsn = FALSE
)

sf::st_write(
  all_survey_stations,
  dsn = here::here("analysis", "goa_strata_2024", "afsc_bts_stations.gpkg"),
  layer = "survey_grid",
  append = FALSE,
  delete_dsn = TRUE
)

# Note: Updated geopackage manually moved to inst/extdata
