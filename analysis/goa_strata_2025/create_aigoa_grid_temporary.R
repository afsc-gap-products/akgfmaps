##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Create new AIGOA 5 km survey grid
## Author:        Sean Rohan <sean.rohan@noaa.gov>
##                Zack Oyafuso <zack.oyafuso@noaa.gov>
## Description:   This script generates prospective survey grid shapefiles 
##                for GOA/AI bottom trawl surveys. The grid is designed to have
##                 5 km x 5 km cells across a fixed extent in the Alaska Albers
##                 Equal Area projection (EPSG:3338).
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define grid extent
standard_extent <- c(xmin = -2400000, ymin = 300000,  
                     xmax = 1500000, ymax = 1250000)
set_crs <- "EPSG:3338"
grid_cell_size <- c(5000, 5000)

grid_poly <- matrix(c(standard_extent[c("xmin", "ymin",
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
  dsn = "analysis/goa_strata_2025/goaai_grid_2025.shp",
  append = FALSE
)

sf::st_write(
  obj = sf::st_centroid(grid_poly),
  dsn = "analysis/goa_strata_2025/goaai_grid_centroid_2025.shp",
  append = FALSE
)

