library(akgfmaps)

# Eastern Bering Sea and Northern Bering Sea
ebs_layers <- akgfmaps:::get_base_layers_v3(select.region = "ebs",
                                            set.crs = "EPSG:3338")

ebs_layers$survey.area |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)),
                SURVEY_NAME = c("Eastern Bering Crab/Groundfish Bottom Trawl Survey", "Northern Bering Sea Crab/Groundfish Survey - Eastern Bering Sea Shelf Survey Extension"),
                SURVEY_DEFINITION_ID = c(98, 143),
                AREA_ID = c(99900, 99902),
                DESIGN_YEAR = c(1987, 2010),
                AREA_TYPE = "REGION") |>
  dplyr::select(AREA_TYPE, SURVEY_NAME, DESIGN_YEAR, SURVEY_DEFINITION_ID, AREA_ID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_area",
               delete_dsn = TRUE)

ebs_layers$survey.strata |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)),
                SURVEY_DEFINITION_ID = dplyr::if_else(Stratum %in% c(70, 71, 81), 143, 98),
                DESIGN_YEAR = dplyr::if_else(Stratum %in% c(70, 71, 81), 2010, 1987),
                AREA_ID = as.numeric(Stratum),
                AREA_TYPE = "STRATUM") |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_strata",
               delete_layer = TRUE,
               append = FALSE)

ebs_layers$survey.grid |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)),
                DESIGN_YEAR = 2024,
                SURVEY_DEFINITION_ID = dplyr::if_else(STATIONID %in% akgfmaps::get_survey_stations(select.region = "nbs"), 143, 98),
                GRID_ID = 999,
                AREA_TYPE = "STATION",
                DESIGN_YEAR = dplyr::if_else(STATIONID %in% akgfmaps::get_survey_stations(select.region = "nbs"), 2010, 2024)) |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, GRID_ID, STATION = STATIONID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_grid",
               delete_layer = TRUE,
               append = FALSE)

# Eastern Bering Sea corners
ebs_corners <- akgfmaps:::get_base_layers_v3(select.region = "sebs",
                                             set.crs = "EPSG:3338",
                                             include.corners = TRUE)

ebs_corners$survey.grid |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)),
                DESIGN_YEAR = 1987,
                AREA_TYPE = "STATION",
                SURVEY_DEFINITION_ID = 98) |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, STATION = STATIONID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_grid",
               delete_layer = FALSE,
               append = TRUE)

# Aleutian Islands
ai_layers <- akgfmaps:::get_base_layers_v3(select.region = "ai",
                                           set.crs = "EPSG:3338")

sf::st_read(here::here("inst", "extdata", "ai_area.shp"))  |>
  sf::st_transform(crs = "EPSG:3338") |>
  akgfmaps:::fix_geometry() |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)),
                SURVEY_DEFINITION_ID = 52,
                DESIGN_YEAR = 1991,
                AREA_ID = 99904,
                AREA_TYPE = "REGION",
                SURVEY_NAME = "Aleutian Islands Bottom Trawl Survey") |>
  dplyr::select(AREA_TYPE, SURVEY_NAME, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_area",
               append = TRUE,
               delete_dsn = FALSE)

sf::st_read(here::here("inst", "extdata", "ai_strata.shp"))  |>
  sf::st_transform(crs = "EPSG:3338") |>
  akgfmaps:::fix_geometry() |>
  dplyr::filter(STRATUM > 0, STRATUM < 800) |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)),
                DESIGN_YEAR = 1991,
                AREA_TYPE = "STRATUM",
                SURVEY_DEFINITION_ID = 52) |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID = STRATUM, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_strata",
               append = TRUE,
               delete_dsn = FALSE)

sf::st_read(system.file("extdata", "ai_grid.shp", package = "akgfmaps"))  |>
  sf::st_transform(crs = "EPSG:3338") |>
  akgfmaps:::fix_geometry() |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)),
                DESIGN_YEAR = 1980,
                AREA_TYPE = "STATION",
                SURVEY_DEFINITION_ID = 52) |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, GRID_ID = AIGRID_ID, STATION = ID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_grid",
               append = TRUE,
               delete_dsn = FALSE)

sf::st_read(system.file("extdata", "ai_grid.shp", package = "akgfmaps"))  |>
  sf::st_transform(crs = "EPSG:3338") |>
  akgfmaps:::fix_geometry() |>
  dplyr::filter(STRATUM > 0, STRATUM < 800) |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)),
                DESIGN_YEAR = 1991,
                AREA_TYPE = "STATION",
                SURVEY_DEFINITION_ID = 52) |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, GRID_ID = AIGRID_ID, STATION = ID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_grid",
               append = TRUE,
               delete_dsn = FALSE)

# Gulf of ALaska 1984 ----
goa_layers <- akgfmaps:::get_base_layers_v3(select.region = "goa",
                                            set.crs = "EPSG:3338")

goa_layers$survey.area |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)),
                SURVEY_DEFINITION_ID = 47,
                DESIGN_YEAR = 1984,
                AREA_ID = 99903,
                AREA_TYPE = "REGION",
                SURVEY_NAME = "Gulf of Alaska Bottom Trawl Survey") |>
  dplyr::select(AREA_TYPE, SURVEY_NAME, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_area",
               append = TRUE,
               delete_dsn = FALSE)

goa_layers$survey.strata |>
  sf::st_transform(crs = "EPSG:3338") |>
  akgfmaps:::fix_geometry() |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)),
                DESIGN_YEAR = 1984,
                AREA_TYPE = "STRATUM",
                SURVEY_DEFINITION_ID = 47) |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID = STRATUM, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_strata",
               append = TRUE,
               delete_dsn = FALSE)

goa_layers$survey.grid |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)),
                DESIGN_YEAR = 1984,
                AREA_TYPE = "STATION",
                SURVEY_DEFINITION_ID = 47) |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, GRID_ID = GOAGRID_ID, STATION = ID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_grid",
               append = TRUE,
               delete_dsn = FALSE)


# Gulf of ALaska 2025 ----
goa_stratum_2025 <- sf::st_read(here::here("analysis", "goa_strata_2025", "goa_strata_2025.gpkg")) |>
  sf::st_set_geometry( "geometry") |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)),
                DESIGN_YEAR = 2025,
                AREA_TYPE = "STRATUM",
                SURVEY_DEFINITION_ID = 47) |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID = STRATUM, AREA_M2)

goa_stratum_2025 |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_strata",
               append = TRUE,
               delete_dsn = FALSE)

goa_stratum_2025 |>
  dplyr::select(SURVEY_DEFINITION_ID) |>
  dplyr::group_by(SURVEY_DEFINITION_ID) |>
  dplyr::summarise() |>
  dplyr::mutate(AREA_TYPE = "REGION",
                AREA_M2 = sf::st_area(geometry),
                SURVEY_DEFINITION_ID = 47,
                DESIGN_YEAR = 2025,
                AREA_ID = 99903,
                AREA_TYPE = "REGION",
                SURVEY_NAME = "Gulf of Alaska Bottom Trawl Survey") |>
  dplyr::select(AREA_TYPE, SURVEY_NAME, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_area",
               append = TRUE,
               delete_dsn = FALSE)


goa_station_grid_2025 <-
  sf::st_read(here::here("analysis", "goa_strata_2025", "goaai_grid_2025.shp")) |>
  sf::st_intersection(goa_stratum_2025) |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)),
                STATION = paste0(AREA_ID, "-", GRIDID),
                AREA_TYPE = "STATION") |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, GRID_ID = id, STATION, AREA_M2)

goa_station_grid_2025 |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_grid",
               append = TRUE,
               delete_dsn = FALSE)

# Eastern Chukchi Sea ----
ecs_layers <- akgfmaps:::get_base_layers_v3(select.region = "ecs",
                                            set.crs = "EPSG:3338")

ecs_layers$survey.area |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)),
                SURVEY_DEFINITION_ID = 6,
                DESIGN_YEAR = 2012,
                AREA_TYPE = "REGION",
                SURVEY_NAME = "Chukchi Sea Trawl Survey") |>
  dplyr::select(AREA_TYPE, SURVEY_NAME, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_area",
               append = TRUE,
               delete_dsn = FALSE)

ecs_layers$survey.strata |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)),
                DESIGN_YEAR = 2012,
                AREA_TYPE = "STRATUM",
                SURVEY_DEFINITION_ID = 6) |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_strata",
               append = TRUE,
               delete_dsn = FALSE)

# EBS slope ----
bss_layers <- akgfmaps:::get_base_layers_v3(select.region = "ebs.slope",
                                            set.crs = "EPSG:3338")

bss_layers$survey.area |>
  dplyr::mutate(SURVEY_DEFINITION_ID = 78,
                SURVEY_NAME = "Eastern Bering Sea Slope Bottom Trawl Survey",
                DESIGN_YEAR = 2023,
                AREA_TYPE = "REGION",
                AREA_ID = 99905,
                AREA_M2 = as.numeric(sf::st_area(geometry))) |>
  dplyr::select(AREA_TYPE, SURVEY_NAME, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_area",
               append = TRUE,
               delete_dsn = FALSE)

bss_layers$survey.strata |>
  dplyr::mutate(SURVEY_DEFINITION_ID = 78,
                DESIGN_YEAR = 2023,
                AREA_TYPE = "STRATUM",
                AREA_M2 = as.numeric(sf::st_area(geometry))) |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID = STRATUM, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_strata",
               append = TRUE,
               delete_dsn = FALSE)
