# Create land polygon geopackage for akgfmaps v4

sf::st_read(dsn = here::here("inst", "extdata", "Alaska_Generalized.shp")) |>
  dplyr::select() |>
  dplyr::mutate(ID = 1) |>
  sf::st_transform(crs = "EPSG:3338") |>
  sf::st_make_valid() |>
  dplyr::group_by(ID) |>
  dplyr::summarise(do_union = TRUE) |>
  dplyr::mutate(COUNTRY = "US",
                STATE_PROVINCE = "Alaska") |>
  dplyr::select(COUNTRY, STATE_PROVINCE) |>
  sf::st_write(dsn = here::here("inst", "extdata", "land_layers.gpkg"),
               layer = "lores_land",
               append = FALSE,
               delete_dsn = TRUE)

sf::st_read(dsn = here::here("inst", "extdata", "Alaska_Coastline.shp")) |>
  dplyr::select() |>
  dplyr::mutate(ID = 1) |>
  sf::st_transform(crs = "EPSG:3338") |>
  sf::st_make_valid() |>
  dplyr::mutate(COUNTRY = "US",
                STATE_PROVINCE = "Alaska") |>
  dplyr::select(COUNTRY, STATE_PROVINCE) |>
  sf::st_write(dsn = here::here("inst", "extdata", "land_layers.gpkg"),
               layer = "hires_land",
               append = FALSE,
               delete_dsn = FALSE)

sf::st_read(dsn = here::here("inst", "extdata", "ak_russia.shp")) |>
  dplyr::filter(DESC_ == "Russia") |>
  sf::st_transform(crs = "EPSG:3338") |>
  sf::st_make_valid() |>
  dplyr::mutate(COUNTRY = "Russia",
                STATE_PROVINCE = NA) |>
  dplyr::select(COUNTRY, STATE_PROVINCE) |>
  sf::st_write(dsn = here::here("inst", "extdata", "land_layers.gpkg"),
               layer = "lores_land",
               append = TRUE,
               delete_dsn = FALSE)

sf::st_read(dsn = here::here("inst", "extdata", "alaska_canada_dcw.shp")) |>
  dplyr::filter(POPYADMIN %in%
                  c("ALBERTA",
                    "BRITISH COLUMBIA",
                    "YUKON TERRITORY",
                    "NORTHWEST TERRITORIES")) |>
  sf::st_transform(crs = "EPSG:3338") |>
  sf::st_make_valid() |>
  dplyr::mutate(COUNTRY = "Canada",
                STATE_PROVINCE = c("Alberta", "British Columbia", "Northwest Territories", "Yukon")) |>
  dplyr::select(COUNTRY, STATE_PROVINCE) |>
  sf::st_write(dsn = here::here("inst", "extdata", "land_layers.gpkg"),
               layer = "lores_land",
               append = TRUE,
               delete_dsn = FALSE)
