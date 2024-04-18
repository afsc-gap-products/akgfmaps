library(akgfmaps) # Version 3.5.1

set_crs <- "EPSG:3338"

dir.create(here::here("analysis", "goaai_grid_2025", "plots"))

# Filtering criteria
min_towpath_length <- 1000 # meters
min_towpath_cruise <- 200100
min_split_length <- 50 # meters

# X and Y offsets for the grid (m)
offsets <- expand.grid(x_off = seq(from = -2000, to = 2000, by = 1000),
                       y_off = seq(from = -2000, to = 2000, by = 1000))

offsets$option <- 1:nrow(offsets)
offsets$creation_date <- Sys.Date()

# Get towpaths created by navmaps

towpaths <- dplyr::bind_rows(
  sf::st_read(dsn = here::here("analysis", "goaai_grid_2025", "data", "ai_towpath.shp")),
  sf::st_read(dsn = here::here("analysis", "goaai_grid_2025", "data", "goa_towpath.shp")) |>
    dplyr::filter(BOTTOM_ < 700) # Remove 700-1000 m depths from the 3-boat survey
) |>
  dplyr::filter(CRUISE > min_towpath_cruise)

towpaths$LENGTH <- as.numeric(sf::st_length(towpaths))

# Only include towpaths that are > km
towpaths <- dplyr::filter(towpaths, as.numeric(LENGTH) >= min_towpath_length)

# Load base layers and 2025 GOA stratum shapefile
ai_layers <- akgfmaps::get_base_layers(select.region = "ai", set.crs = set_crs)
goa_layers <- akgfmaps::get_base_layers(select.region = "goa", set.crs = set_crs)
goa_strata_2025 <- sf::st_read(
  here::here("analysis", "goaai_grid_2025", "data", "goa_strata_2025.gpkg")
) |>
  sf::st_cast(to = "POLYGON") |>
  dplyr::rename(geometry = geom)

names(goa_strata_2025)[names(goa_strata_2025) != "geometry"] <- toupper(names(goa_strata_2025)[names(goa_strata_2025) != "geometry"])

sf::st_crs(goa_strata_2025) <- set_crs

strata_2025 <- dplyr::bind_rows(
  dplyr::mutate(ai_layers$survey.strata,
                REGION = "AI"),
  dplyr::mutate(goa_strata_2025,
                REGION = "GOA")
) |>
  dplyr::select(REGION, STRATUM)

# Calculate number of towpaths that intersect with current survey grid
baseline <- sf::st_intersection(towpaths,
                                dplyr::bind_rows(
                                  dplyr::mutate(ai_layers$survey.grid,
                                                REGION = "AI"),
                                  dplyr::mutate(goa_layers$survey.grid,
                                                REGION = "GOA")
                                )
) |>
  sf::st_cast(to = "LINESTRING")

baseline$LENGTH <- as.numeric(sf::st_length(baseline))

baseline <- baseline |>
  dplyr::filter(LENGTH >= min_split_length)

n_lines <- baseline |>
  sf::st_drop_geometry() |>
  dplyr::group_by(REGION) |>
  dplyr::summarise(n = n()) |>
  dplyr::mutate(grid = "Current")

for(jj in 1:nrow(offsets)) {

  grid_strata <- sf::st_read(dsn = here::here("analysis", "goaai_grid_2025", "grid_options", "grids",
                                              paste0(gsub(x = Sys.Date(),
                                                          pattern = "-",
                                                          replacement = ""),
                                                     "_goaai_grid_2025_",
                                                     offsets$option[jj],
                                                     ".shp"))
  ) |>
    sf::st_intersection(strata_2025)

  grid_intersect <- sf::st_intersection(towpaths,
                                        grid_strata) |>
    sf::st_cast(to = "LINESTRING")

  grid_intersect$LENGTH <- grid_intersect |>
    sf::st_length() |>
    as.numeric()

  grid_intersect <- grid_intersect |>
    dplyr::filter(LENGTH > min_split_length)

  n_lines <- grid_intersect |>
    sf::st_drop_geometry() |>
    dplyr::group_by(REGION) |>
    dplyr::summarise(n = n()) |>
    dplyr::mutate(grid = paste0("Option ", offsets$option[jj]),
                  x_off = offsets$x_off[jj],
                  y_off = offsets$y_off[jj]) |>
    dplyr::bind_rows(n_lines)

}

# n_lines
#
# nrow(baseline)

dplyr::filter(n_lines, REGION == "AI") |>
  dplyr::arrange(n) |>
  write.csv(here::here("analysis", "goaai_grid_2025", "plots", "grid_intersects_ai.csv"),
            row.names = FALSE)

dplyr::filter(n_lines, REGION == "GOA") |>
  dplyr::arrange(n) |>
  write.csv(here::here("analysis", "goaai_grid_2025", "plots", "grid_intersects_goa.csv"),
            row.names = FALSE)
