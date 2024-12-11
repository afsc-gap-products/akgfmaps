# Options for a new 2025 GOA/AI grid (WGS84)
#
# Created by: Sean Rohan <sean.rohan@noaa.gov>
# Date: April 17, 2024
#
# Description:
#
# This script generates prospective survey grid shapefiles for GOA/AI bottom trawl surveys.
# The grid is designed to have 5 km x 5 km cells across a fixed extent in the Alaska Albers Equal
# Area projection (EPSG:3338).
#
# The script produces grids with different origin points that are offset by -2000 m to 2000 m from
# the proposed standard extent in order to determine whether the origin should be shifted to avoid
# small areas along stratum boundaries.

# install.packages("here", "remotes")
# remotes::install_github(repo = "afsc-gap-products/akgfmaps")

library(akgfmaps)
library(here)
library(cowplot)

dir.create(here::here("analysis", "goaai_grid_2025", "grid_options", "grids"), recursive = TRUE)
dir.create(here::here("analysis", "goaai_grid_2025", "grid_options", "centroids"), recursive = TRUE)

# Define grid extent

set_crs <- "WGS84"
grid_cell_size <- c(0.054, 0.054)

offsets <- expand.grid(x_off = -2:2,
                       y_off = -2:2)

offsets$option <- 1:nrow(offsets)
offsets$creation_date <- Sys.Date()

write.csv(offsets, file = here::here("analysis", "goaai_grid_2025", "grid_options",
                                     paste0(gsub(x = Sys.Date(), pattern = "-", replacement = ""), "_goaai_grid_2025_options.csv")),
          row.names = FALSE)

for(ii in 1:nrow(offsets)) {

  left_extent <- c(xmin = 180-(157*grid_cell_size[1]+offsets$x_off[ii]),
                   xmax = 180,
                   ymin = 47.25,
                   ymax = 47.25+260*grid_cell_size[2]+grid_cell_size[2]*offsets$y_off[ii])

  right_extent <- c(xmin = -180,
                    xmax = -180+525*grid_cell_size[1]+offsets$x_off[ii],
                    ymin = 47.25,
                    ymax = 47.25+260*grid_cell_size[2]+grid_cell_size[2]*offsets$y_off[ii])

  grid_poly_left <- matrix(c(left_extent[c("xmin", "ymin",
                                                   "xmax", "ymin",
                                                   "xmax", "ymax",
                                                   "xmin", "ymax",
                                                   "xmin", "ymin")]),
                           ncol = 2,
                           byrow = TRUE) |>
    sf::st_multipoint() |>
    sf::st_cast("POLYGON") |>
    sf::st_make_grid(cellsize = grid_cell_size) |>
    sf::st_as_sf() |>
    dplyr::rename(geometry = x) |>
    sf::st_set_crs(value = set_crs)

  grid_poly_right <- matrix(c(right_extent[c("xmin", "ymin",
                                             "xmax", "ymin",
                                             "xmax", "ymax",
                                             "xmin", "ymax",
                                             "xmin", "ymin")]),
                            ncol = 2,
                            byrow = TRUE) |>
    sf::st_multipoint() |>
    sf::st_cast("POLYGON") |>
    sf::st_make_grid(cellsize = grid_cell_size) |>
    sf::st_as_sf() |>
    dplyr::rename(geometry = x) |>
    sf::st_set_crs(value = set_crs)

  grid_poly <- dplyr::bind_rows(grid_poly_left, grid_poly_right) |>
    sf::st_transform(crs = "EPSG:3338")

  grid_poly$area <- as.numeric(sf::st_area(grid_poly))

  ggplot() +
    geom_sf(data = grid_poly,
            mapping = aes(fill = area),
            color = NA) +
    scale_fill_viridis_c(option = "H")

  # Assign grid cell names to the polygon (could use a different system)
  grid_poly$id <- 1:nrow(grid_poly)

  coords_df <- sf::st_centroid(grid_poly) |>
    sf::st_coordinates(grid_poly) |>
    as.data.frame() |>
    dplyr::select(X, Y)

  coords_df$id <- grid_poly$id

  x_index <- data.frame(X = sort(unique(coords_df$X)))
  x_index$x_rank <- 1:nrow(x_index)

  y_index <- data.frame(Y = sort(unique(coords_df$Y)))
  y_index$y_rank <- 1:nrow(y_index)

  coords_df <- coords_df |>
    dplyr::inner_join(y_index, by = "Y") |>
    dplyr::inner_join(x_index, by = "X") |>
    dplyr::mutate(STATION = paste0(y_rank, "-", x_rank))

  grid_poly$STATION <- coords_df$STATION

  # Verify that all stations are unique

  stopifnot("Mismatch between number of unique grid cells and unique station names" = all(table(grid_poly$STATION) == 1))

  # Write polygons and centroids to shapefiles
  sf::st_write(
    obj = grid_poly,
    dsn = here::here("analysis", "goaai_grid_2025", "grid_options", "grids",
                     paste0(gsub(x = Sys.Date(), pattern = "-", replacement = ""), "_goaai_grid_2025_", offsets$option[ii], ".shp")),
    append = FALSE
  )

  sf::st_write(
    obj = sf::st_centroid(grid_poly),
    dsn = here::here("analysis", "goaai_grid_2025", "grid_options", "centroids",
                     paste0(gsub(x = Sys.Date(), pattern = "-", replacement = ""), "_goaai_grid_centroid_2025_", offsets$option[ii], ".shp")),
    append = FALSE
  )

}


# Verify that the center grid matches Mark's grid
marks_centered_grid <- sf::st_read(here::here("analysis", "goaai_grid_2025", "Fishnets", "Center", "FISHNET_C.shp"))

seans_centered_grid <- sf::st_read(dsn = here::here("analysis", "goaai_grid_2025", "grid_options", "grids",
                                                    paste0(gsub(x = Sys.Date(), pattern = "-", replacement = ""),
                                                           "_goaai_grid_2025_",
                                                           offsets$option[offsets$x_off == 0 & offsets$y_off == 0], ".shp"))) |>
  sf::st_transform(centered_grid, crs = sf::st_crs(marks_centered_grid))

identical(dplyr::select(seans_centered_grid, geometry),
          dplyr::select(marks_centered_grid, geometry))

marks_coords <- sf::st_coordinates(marks_centered_grid) |>
  as.data.frame() |>
  dplyr::select(X, Y) |>
  unique()

seans_coords <- sf::st_coordinates(seans_centered_grid) |>
  as.data.frame() |>
  dplyr::select(X, Y) |>
  unique()


nrow(marks_coords) == nrow(seans_coords)
nrow(dplyr::inner_join(marks_coords, seans_coords)) == nrow(marks_coords)
nrow(dplyr::inner_join(marks_coords, seans_coords)) == nrow(seans_coords)
