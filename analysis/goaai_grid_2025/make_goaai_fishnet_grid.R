
# install.packages("here", "remotes")
# remotes::install_github(repo = "afsc-gap-products/akgfmaps")

library(akgfmaps)
library(here)

dir.create(here::here("analysis", "goaai_grid_2025", "grid_options", "grids"), recursive = TRUE)
dir.create(here::here("analysis", "goaai_grid_2025", "grid_options", "centroids"), recursive = TRUE)

# Define grid extent
standard_extent <- c(xmin = -2400000, ymin = 300000,  xmax = 1500000, ymax = 1250000)

set_crs <- "EPSG:3338"
grid_cell_size <- c(5000, 5000)

# Setup offsets for grid options
offsets <- expand.grid(x_off = seq(from = -2000, to = 2000, by = 1000),
                       y_off = seq(from = -2000, to = 2000, by = 1000))

offsets$option <- 1:nrow(offsets)
offsets$creation_date <- Sys.Date()

write.csv(offsets, file = here::here("analysis", "goaai_grid_2025", "grid_options",
                                     paste0(gsub(x = Sys.Date(), pattern = "-", replacement = ""), "_goaai_grid_2025_options.csv")),
          row.names = FALSE)

for(ii in 1:nrow(offsets)) {

  shifted_extent <- standard_extent
  shifted_extent['xmin'] <- shifted_extent['xmin'] + offsets$x_off[ii]
  shifted_extent['xmax'] <- shifted_extent['xmax'] + offsets$x_off[ii]
  shifted_extent['ymin'] <- shifted_extent['ymin'] + offsets$y_off[ii]
  shifted_extent['ymax'] <- shifted_extent['ymax'] + offsets$y_off[ii]

  grid_poly <- matrix(c(shifted_extent[c("xmin", "ymin",
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

