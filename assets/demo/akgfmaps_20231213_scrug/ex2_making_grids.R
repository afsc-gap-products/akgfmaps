# Making grids with akgfmaps, sf, and terra
# Sean Rohan <sean.rohan@noaa.gov>
# Survey-Centric R Users Group
# December 13, 2023
# https://github.com/afsc-gap-products/akgfmaps


library(akgfmaps)

nbs_layers <- akgfmaps::get_base_layers(select.region = "nbs",
                                        set.crs = "EPSG:3338") # Alaska Albers Equal area


# make_2d_grid() -----------------------------------------------------------------------------------
#
# Make a grid using a polygon as a template.

nbs_stratum_7071 <- nbs_layers$survey.strata |>
  dplyr::select(STRATUM, geometry) |>
  dplyr::filter(STRATUM %in% c(70, 71))

ggplot() +
  geom_sf(data = nbs_stratum_7071)

nbs_7071_point_grid <- make_2d_grid(obj = nbs_stratum_7071,
                                    resolution = c(5000, 5000), # 5 km x 5 km grid
                                    output_type = "point",
                                    include_tile_center = TRUE)

head(nbs_7071_point_grid)

# Note that the areas at grid locations vary due to spatial distortion and because areas for cells
# along the stratum boundary only include the portion of the cell that's inside of the boundary.
# The points for grid cells along the boundary are also not at the exact center-- they're at the
# center of the grid cell after it's been split by the boundary. This can be a problem down the line
# after converting to WGS84 then UTM, then back to the original projection (which is useful when
# working with VAST or sdmTMB)

# By setting include_tile_center = TRUE, the center point of each grid cell in the original projection
# will also be returned in lon_plot and lat_plot arguments.

ggplot() +
  geom_sf(data = nbs_7071_point_grid,
          mapping = aes(color = as.numeric(AREA))) +
  scale_color_distiller(name = "Area (m^2)")

ggplot() +
  geom_sf(data = nbs_7071_point_grid,
          mapping = aes(color = factor(Stratum))) +
  scale_color_viridis_d(name = "Stratum")


# Convert to WGS84
nbs_7071_wgs84 <- sf::st_transform(nbs_7071_point_grid,
                                   crs = "WGS84")

nbs_7071_wgs84 <- cbind(nbs_7071_wgs84 |>
                          sf::st_coordinates() |>
                          as.data.frame(),
                        nbs_7071_wgs84)

nbs_7071_wgs84 <- dplyr::select(nbs_7071_wgs84, -geometry)


# Making a 'fishnet' grid using the sf and terra packages ------------------------------------------

# Some akgfmaps layers are created by performing spatial operations internally to avoid redundancy
# and facilitate consistency. Performing spatial operations in this manner also ensures shapefiles
# can updated using a fast and reproducible approach.

# When users run get_base_layers(), the function intersects a basin-wide EBS grid the the NBS survey
# stratum, then filters stations by STATIONID to only include stations within the survey grid.

# Load the full grid
full_ebs_grid <- sf::st_read(system.file("./extdata/bs_grid_w_corners.shp", package = "akgfmaps"))

# Plot the grid
ggplot() +
  geom_sf(data = full_ebs_grid) +
  geom_sf(data = nbs_layers$survey.grid,
          fill = "black") +
  geom_sf(data = nbs_layers$survey.strata, fill = NA, color = "red")


# A blank 2D grid can be created by defining an extent, starting point, grid resolution, and
# coordinate reference system. With this info, we can recreate a grid on a variety of software (such
# as ArcMap) to within machine precision.

# Here, we're creating a grid in the Alaska AEA projection starting with a bounding box

ext_coords <- akgfmaps::transform_data_frame_crs(x = data.frame(x = c(-173, -160),
                                                                y = c(60, 70)),
                                                 coords = c("x", "y"),
                                                 in.crs = "WGS84",
                                                 out.crs = "EPSG:3338") |>
  unlist()

# Make a 30 x 30 km grid that has a vertical and horizontal ID system
new_poly_grid_prefix <- terra::rast(extent = terra::ext(ext_coords),
                                    resolution = c(30000, 30000),
                                    crs = "EPSG:3338",
                                    vals = matrix(rep(1:27, 32), nrow = 32, ncol = 27, byrow = TRUE)) |>
  terra::as.polygons(aggregate = FALSE) |>
  sf::st_as_sf()

new_poly_grid_suffix <- terra::rast(extent = terra::ext(ext_coords),
                                    resolution = c(30000, 30000),
                                    crs = "EPSG:3338",
                                    vals = matrix(rep(1:32, 27), nrow = 32, ncol = 27, byrow = FALSE)) |>
  terra::as.polygons(aggregate = FALSE) |>
  sf::st_as_sf()

# Combine vertical and horizontal IDs into a grid cell ID.
new_poly_grid <- new_poly_grid_prefix |>
  dplyr::rename(prefix = lyr.1) |>
  dplyr::mutate(suffix = new_poly_grid_suffix$lyr.1) |>
  dplyr::mutate(grid_id = paste0(prefix, "-", suffix))

# Intersect the grid with the strata object
stratum_grid <- sf::st_intersection(new_poly_grid,
                                    nbs_stratum_7071)
stratum_grid$area <- sf::st_area(stratum_grid)

# Visualize the results
ggplot() +
  geom_sf(data = stratum_grid,
          mapping = aes(fill = factor(Stratum)))

ggplot() +
  geom_sf(data = stratum_grid,
          mapping = aes(fill = as.numeric(area))) +
  scale_fill_viridis_c(name = "Area (m^2)")

ggplot() +
  geom_sf(data = stratum_grid,
          mapping = aes(fill = prefix)) +
  scale_fill_viridis_c(name = "Prefix")

ggplot() +
  geom_sf(data = stratum_grid,
          mapping = aes(fill = suffix)) +
  scale_fill_viridis_c(name = "Suffix")
