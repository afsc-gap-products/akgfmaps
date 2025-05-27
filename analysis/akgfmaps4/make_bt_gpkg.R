# Add bottom trawl survey layers to .gpkg

library(akgfmaps)

# Gulf of Alaska 1984 design ----
goa_layers <- akgfmaps:::get_base_layers_v3(select.region = "goa",
                                            set.crs = "EPSG:3338")

goa_layers$survey.area |>
  dplyr::mutate(AREA_M2 = AREA_KM2*1e6,
                SURVEY_DEFINITION_ID = 47,
                DESIGN_YEAR = 1984,
                AREA_ID = 99903,
                AREA_TYPE = "REGION",
                SURVEY_NAME = "Gulf of Alaska Bottom Trawl Survey") |>
  dplyr::select(AREA_TYPE, SURVEY_NAME, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_area",
               append = FALSE,
               delete_dsn = TRUE)

goa_layers$survey.strata |>
  sf::st_transform(crs = "EPSG:3338") |>
  akgfmaps:::fix_geometry() |>
  dplyr::group_by(STRATUM) |>
  dplyr::summarise(AREA_M2 = sum(AREA_KM2*1e6), do_union = TRUE) |>
  dplyr::ungroup() |>
  dplyr::mutate(DESIGN_YEAR = 1984,
                AREA_TYPE = "STRATUM",
                SURVEY_DEFINITION_ID = 47) |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID = STRATUM, AREA_M2) |>
  dplyr::group_by(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID) |>
  dplyr::summarise(AREA_M2 = sum(AREA_M2), do_union = TRUE) |>
  dplyr::ungroup() |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_strata",
               append = FALSE,
               delete_dsn = FALSE)

goa_layers$survey.grid |>
  dplyr::mutate(AREA_M2 = AREA_KM2*1e6,
                DESIGN_YEAR = 1984,
                AREA_TYPE = "STATION",
                SURVEY_DEFINITION_ID = 47) |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID = STRATUM, GRID_ID = GOAGRID_ID, STATION = ID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_grid",
               append = FALSE,
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
  sf::st_read(here::here("analysis", "goa_strata_2025", "goa_stations_2025.gpkg")) |>
  dplyr::mutate(AREA_M2 = AREA_KM2*1e6,
                SURVEY_DEFINITION_ID = 47,
                DESIGN_YEAR = 2025,
                AREA_TYPE = "STATION") |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID = STRATUM, GRID_ID = GRIDID, STATION, AREA_M2)

goa_station_grid_2025 |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_grid",
               append = TRUE,
               delete_dsn = FALSE)

# EBS and NBS 2010 ----

# EBS and NBS 2019 ----
ebs_strata_2019 <- sf::st_read(dsn = here::here("inst", "extdata", "EBS_NBS_2019.shp")) |>
  sf::st_transform(crs = "EPSG:3338") |>
  dplyr::mutate(SURVEY_NAME = dplyr::if_else(STRATUM %in% c(70, 71, 81), "Northern Bering Sea Crab/Groundfish Survey - Eastern Bering Sea Shelf Survey Extension", "Eastern Bering Crab/Groundfish Bottom Trawl Survey"),
                SURVEY_DEFINITION_ID = dplyr::if_else(STRATUM %in% c(70, 71, 81), 143, 98),
                AREA_ID = dplyr::if_else(STRATUM %in% c(70, 71, 81), 99902, 99900),
                AREA_TYPE = "REGION",
                DESIGN_YEAR = 2019) |>
  dplyr::group_by(AREA_TYPE, SURVEY_NAME, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID)  |>
  dplyr::summarise(AREA_M2 = sum(AREA_KM2*1e6),
                   do_union = TRUE) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_area",
               append = TRUE,
               delete_dsn = FALSE)

sf::st_read(dsn = here::here("inst", "extdata", "EBS_NBS_2019.shp")) |>
  sf::st_transform(crs = "EPSG:3338") |>
  dplyr::mutate(AREA_M2 = AREA_KM2 * 1e6,
                AREA_ID = STRATUM,
                AREA_TYPE = "STRATUM",
                DESIGN_YEAR = 2019,
                SURVEY_DEFINITION_ID = dplyr::if_else(STRATUM %in% c(70, 71, 81), 143, 98)) |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_strata",
               append = TRUE,
               delete_dsn = FALSE)

# Eastern Bering Sea and Northern Bering Sea 2022 calculations ----
ebs_layers <- akgfmaps:::get_base_layers_v3(select.region = "ebs",
                                            set.crs = "EPSG:3338")

ebs_layers$survey.area |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)),
                SURVEY_NAME = c("Northern Bering Sea Crab/Groundfish Survey - Eastern Bering Sea Shelf Survey Extension",
                                "Eastern Bering Crab/Groundfish Bottom Trawl Survey"),
                SURVEY_DEFINITION_ID = c(143, 98),
                AREA_ID = c(99902, 99900),
                DESIGN_YEAR = 2022,
                AREA_TYPE = "REGION") |>
  dplyr::select(AREA_TYPE, SURVEY_NAME, DESIGN_YEAR, SURVEY_DEFINITION_ID, AREA_ID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_area",
               append = TRUE,
               delete_dsn = FALSE)

ebs_layers$survey.strata |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)),
                SURVEY_DEFINITION_ID = dplyr::if_else(Stratum %in% c(70, 71, 81), 143, 98),
                DESIGN_YEAR = 2022,
                AREA_ID = as.numeric(Stratum),
                AREA_TYPE = "STRATUM") |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_strata",
               append = TRUE,
               delete_layer = FALSE)


# Eastern Bering Sea and Northern Bering Sea grid 2010 and 2024 designs ----
ebs_layers$survey.grid |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)),
                SURVEY_DEFINITION_ID = dplyr::if_else(STATIONID %in% akgfmaps::get_survey_stations(select.region = "nbs"), 143, 98),
                AREA_TYPE = "STATION",
                AREA_ID = NA,
                DESIGN_YEAR = dplyr::if_else(STATIONID %in% akgfmaps::get_survey_stations(select.region = "nbs"), 2010, 2024)) |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID, STATION = STATIONID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_grid",
               append = TRUE,
               delete_layer = FALSE)

# Eastern Bering Sea grid w/ corners 2010 design ----
ebs_corners <- akgfmaps:::get_base_layers_v3(select.region = "sebs",
                                             set.crs = "EPSG:3338",
                                             include.corners = TRUE)

ebs_corners$survey.grid |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)),
                DESIGN_YEAR = 2010,
                AREA_TYPE = "STATION",
                AREA_ID = NA,
                SURVEY_DEFINITION_ID = 98) |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID, STATION = STATIONID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_grid",
               append = TRUE,
               delete_layer = FALSE)

# Aleutian Islands 1991 design ----
ai_layers <- akgfmaps:::get_base_layers_v3(select.region = "ai",
                                           set.crs = "EPSG:3338")

sf::st_read(here::here("inst", "extdata", "ai_area.shp"))  |>
  sf::st_transform(crs = "EPSG:3338") |>
  akgfmaps:::fix_geometry() |>
  dplyr::mutate(AREA_M2 = AREA_KM2*1e6,
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
  dplyr::group_by(STRATUM) |>
  dplyr::summarise(AREA_M2 = sum(AREA)) |>
  dplyr::ungroup() |>
  dplyr::mutate(DESIGN_YEAR = 1991,
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
  dplyr::filter(STRATUM > 0, STRATUM < 800) |>
  dplyr::mutate(AREA_M2 = AREA,
                DESIGN_YEAR = 1991,
                AREA_TYPE = "STATION",
                SURVEY_DEFINITION_ID = 52) |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID = STRATUM, GRID_ID = AIGRID_ID, STATION = ID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_grid",
               append = TRUE,
               delete_dsn = FALSE)


# Aleutian Islands 1980 design ----
sf::st_read(system.file("extdata", "ai_grid.shp", package = "akgfmaps"))  |>
  dplyr::filter(STRATUM > 0) |>
  sf::st_transform(crs = "EPSG:3338") |>
  akgfmaps:::fix_geometry() |>
  dplyr::mutate(AREA_M2 = AREA,
                DESIGN_YEAR = 1980,
                AREA_TYPE = "STATION",
                SURVEY_DEFINITION_ID = 52) |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID = STRATUM, GRID_ID = AIGRID_ID, STATION = ID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_grid",
               append = TRUE,
               delete_dsn = FALSE)

sf::st_read(system.file("extdata", "ai_grid.shp", package = "akgfmaps"))  |>
  dplyr::filter(STRATUM > 0) |>
  sf::st_transform(crs = "EPSG:3338") |>
  akgfmaps:::fix_geometry() |>
  dplyr::group_by(AREA_ID = STRATUM) |>
  dplyr::summarise(AREA_M2 = sum(AREA), do_union = TRUE) |>
  dplyr::ungroup() |>
  dplyr::mutate(DESIGN_YEAR = 1980,
                AREA_TYPE = "STRATUM",
                SURVEY_DEFINITION_ID = 52) |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_strata",
               append = TRUE,
               delete_dsn = FALSE)

sf::st_read(system.file("extdata", "ai_grid.shp", package = "akgfmaps"))  |>
  dplyr::filter(STRATUM > 0) |>
  sf::st_transform(crs = "EPSG:3338") |>
  dplyr::mutate(id = 1) |>
  dplyr::summarise(AREA_M2 = sum(AREA), do_union = TRUE) |>
  akgfmaps:::fix_geometry() |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)),
                DESIGN_YEAR = 1980,
                AREA_TYPE = "REGION",
                SURVEY_NAME = "Aleutian Islands Bottom Trawl Survey",
                AREA_ID = 99904,
                SURVEY_DEFINITION_ID = 52) |>
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "survey_area",
               append = TRUE,
               delete_dsn = FALSE)

# Eastern Chukchi Sea 2012 design ----
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

# EBS slope 2023 area recalculation ----
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


# Historical GOA and AI INPFC strata ----

get_inpfc_strata(
  select.region = "goa",
  set.crs = "EPSG:3338",
  use.v3 = TRUE
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
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_NAME, AREA_ID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "inpfc_strata",
               append = FALSE,
               delete_dsn = FALSE)

get_inpfc_strata(
  select.region = "ai",
  set.crs = "EPSG:3338",
  use.v3 = TRUE
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
  dplyr::select(AREA_TYPE, SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_NAME, AREA_ID, AREA_M2) |>
  sf::st_write(dsn = here::here("inst", "extdata", "afsc_bottom_trawl_surveys.gpkg"),
               layer = "inpfc_strata",
               append = TRUE,
               delete_dsn = FALSE)
