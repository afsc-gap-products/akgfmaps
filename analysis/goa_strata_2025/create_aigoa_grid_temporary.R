##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Create new AIGOA 5 km survey grid
## Author:        Sean Rohan <sean.rohan@noaa.gov>
##                Zack Oyafuso <zack.oyafuso@noaa.gov>
## Description:   This script generates a 5 km grid that covers the Gulf of 
##                Alaska bottom trawl survey projected via Alaska Albers
##                projection (EPSG:3338).
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import Libraries
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(sf)
library(dplyr)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Set Spatial Constants
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Define grid extent
standard_extent <- c(xmin = -2400000, ymin = 302000,  
                     xmax = 1500000, ymax = 1252000)
## Define coordinate reference system NAD83 / Alaska Albers 
set_crs <- "EPSG:3338"

## Define cell resolution in meters
grid_cell_size <- c(5000, 5000)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Create Alaska-wide 5 km survey grid
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
grid_poly <- 
  ## Define bounding box
  matrix(c(standard_extent[c("xmin", "ymin",
                             "xmax", "ymin",
                             "xmax", "ymax",
                             "xmin", "ymax",
                             "xmin", "ymin")]),
         ncol = 2, byrow = TRUE) |>
  ## Create bounding box
  sf::st_multipoint() |> sf::st_cast(to = "POLYGON") |>
  ## Create 5 km grid within the bounding box
  sf::st_make_grid(cellsize = grid_cell_size) |>
  sf::st_as_sf() |>
  dplyr::rename(geometry = x) |>
  ## Project to CRS EPSG:3338
  sf::st_set_crs(value = set_crs)

# Assign grid cell names to the polygon (could use a different system)
grid_poly$id <- 1:nrow(x = grid_poly)

## Calculate centroid location of each grid cell
coords_df <- sf::st_centroid(x = grid_poly) |>
  sf::st_coordinates(grid_poly) |>
  as.data.frame() |>
  dplyr::select(X, Y)
coords_df$id <- grid_poly$id

## The GRIDID field is a unique identifier that consists of two numbers
## connected with a hyphen. The first number is the index of the grid cell on 
## the horizontal "x" axis and the second number is the index of the grid cell
## on the vertical "y" axis. The GRIDID 1-1 refers to the bottom-left corner
## of the grid. 
x_index <- data.frame(X = sort(x = unique(x = coords_df$X)))
x_index$x_rank <- 1:nrow(x = x_index)

y_index <- data.frame(Y = sort(x = unique(x = coords_df$Y)))
y_index$y_rank <- 1:nrow(x = y_index)

coords_df <- coords_df |>
  dplyr::inner_join(y_index, by = "Y") |>
  dplyr::inner_join(x_index, by = "X") |>
  dplyr::mutate(GRIDID = paste0(y_rank, "-", x_rank))

grid_poly$GRIDID <- coords_df$GRIDID

# Verify that all GRIDIDs are unique
stopifnot("Mismatch between number of unique grid cells and unique GRIDID names" = all(table(grid_poly$GRIDID) == 1))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Write polygons and centroids to shapefiles
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sf::st_write(obj = grid_poly,
             dsn = "analysis/goa_strata_2025/goaai_grid_2025.shp",
             append = FALSE)

sf::st_write(obj = sf::st_centroid(x = grid_poly),
             dsn = "analysis/goa_strata_2025/goaai_grid_centroid_2025.shp",
             append = FALSE)

