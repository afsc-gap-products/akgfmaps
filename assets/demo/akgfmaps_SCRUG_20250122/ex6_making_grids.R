# Example: Make 2D grid
# SCRUG: akgfmaps, January 22, 2025
# Created by Sean Rohan (GitHub: sean-rohan-noaa)


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
          mapping = aes(color = factor(STRATUM))) +
  scale_color_viridis_d(name = "Stratum")


# Convert to WGS84
nbs_7071_wgs84 <- sf::st_transform(nbs_7071_point_grid,
                                   crs = "WGS84")

nbs_7071_wgs84 <- cbind(nbs_7071_wgs84 |>
                          sf::st_coordinates() |>
                          as.data.frame() |>
                          dplyr::rename(LON = X, LAT = Y),
                        nbs_7071_wgs84)

nbs_7071_wgs84 <- dplyr::select(nbs_7071_wgs84, -geometry)

head(nbs_7071_wgs84)
