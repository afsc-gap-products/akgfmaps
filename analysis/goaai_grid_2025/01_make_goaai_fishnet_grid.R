# Options for a new 2025 GOA/AI grid
#
# Created by: Sean Rohan <sean.rohan@noaa.gov>
# Date: March 9, 2024
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
standard_extent <- c(xmin = -2400000, ymin = 300000,  xmax = 1500000, ymax = 1250000)

set_crs <- "EPSG:3338" # "+proj=longlat +ellps=WGS84 +pm=-360 +datum=WGS84 +no_defs"
grid_cell_size <- c(5000, 5000) # c(0.095, 0.054)

# Offsets to explore
offsets <- expand.grid(x_off = seq(from = -2000, to = 2000, by = 1000),
                       y_off = seq(from = -2000, to = 2000, by = 1000))

# offsets <- expand.grid(x_off = seq(from = -0.15, to = 0.15, by = 0.05),
#                        y_off = seq(from = -0.15, to = 0.15, by = 0.05))

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



# Compare grid at stratum boundaries - AI

ai_layers <- akgfmaps::get_base_layers(select.region = "ai", set.crs = set_crs)

focal_zones <- data.frame(x = c(-177.1766891, -170.1220727, 177.2177623),
                          y = c(51.7841635, 52.7870693, 51.9537367),
                          label = c("Break 1", "Break 2", "Break 3")) |>
  sf::st_as_sf(coords = c("x", "y"),
               crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338") |>
  sf::st_buffer(dist = 50000)

pdf(file = here::here("analysis", "goaai_grid_2025", "ai_grid_alignment.pdf"),
    width = 8,
    height = 8,
    onefile = TRUE)

for(jj in 1:nrow(offsets)) {

  grid_poly <- sf::st_read(dsn = here::here("analysis", "goaai_grid_2025", "grid_options", "grids",
                                            paste0(gsub(x = Sys.Date(), pattern = "-", replacement = ""), "_goaai_grid_2025_", offsets$option[jj], ".shp"))
  )

  ai_grid_strata <- sf::st_intersection(grid_poly,
                                        ai_layers$survey.strata)

  ai_grid_strata$AREA <- sf::st_area(ai_grid_strata)


  bbox_1 <- sf::st_bbox(focal_zones$geometry[1])

  p1 <- ggplot() +
    geom_sf(data = ai_layers$akland, color = "grey50") +
    geom_sf(data = ai_grid_strata,
            mapping = aes(fill = as.numeric(AREA)),
            color = "white") +
    scale_x_continuous(limits = c(bbox_1['xmin'], bbox_1['xmax'])) +
    scale_y_continuous(limits = c(bbox_1['ymin'], bbox_1['ymax'])) +
    scale_fill_viridis_c(name = "Area") +
    ggtitle(label = paste0("Offset X: ", offsets$x_off[jj],
                           "; Offset Y: ", offsets$y_off[jj])) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

  bbox_2 <- sf::st_bbox(focal_zones$geometry[2])

  p2 <- ggplot() +
    geom_sf(data = ai_layers$akland, color = "grey50") +
    geom_sf(data = ai_grid_strata,
            mapping = aes(fill = as.numeric(AREA)),
            color = "white") +
    scale_x_continuous(limits = c(bbox_2['xmin'], bbox_2['xmax'])) +
    scale_y_continuous(limits = c(bbox_2['ymin'], bbox_2['ymax'])) +
    scale_fill_viridis_c(name = "Area") +
    ggtitle(label = paste0("Offset X: ", offsets$x_off[jj],
                           "; Offset Y: ", offsets$y_off[jj])) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none")

  bbox_3 <- sf::st_bbox(focal_zones$geometry[3])

  p3 <- ggplot() +
    geom_sf(data = ai_layers$akland, color = "grey50") +
    geom_sf(data = ai_grid_strata,
            mapping = aes(fill = as.numeric(AREA)),
            color = "white") +
    scale_x_continuous(limits = c(bbox_3['xmin'], bbox_3['xmax'])) +
    scale_y_continuous(limits = c(bbox_3['ymin'], bbox_3['ymax'])) +
    scale_fill_viridis_c(name = "Area") +
    ggtitle(label = paste0("Offset X: ", offsets$x_off[jj],
                           "; Offset Y: ", offsets$y_off[jj])) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none")

  legend <- cowplot::get_legend(p1)


  print(
    ggdraw(cowplot::plot_grid(p1 + theme(legend.position = "none"), p2, p3, legend, nrow = 2))
  )


}

dev.off()


# Compare grid at stratum boundaries - GOA

goa_strata <- sf::st_read(here::here("analysis", "goa_strata_2025", "GOA_shapes.shp"))
goa_layers <- akgfmaps::get_base_layers(select.region = "goa", set.crs = set_crs)

focal_zones <- data.frame(x = c(-177.1766891, -170.1220727, 177.2177623),
                          y = c(51.7841635, 52.7870693, 51.9537367),
                          label = c("Break 1", "Break 2", "Break 3")) |>
  sf::st_as_sf(coords = c("x", "y"),
               crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338") |>
  sf::st_buffer(dist = 50000)

pdf(file = here::here("analysis", "goaai_grid_2025", "goa_grid_alignment.pdf"),
    width = 8,
    height = 8,
    onefile = TRUE)

for(jj in 1:nrow(offsets)) {

  grid_poly <- sf::st_read(dsn = here::here("analysis", "goaai_grid_2025", "grid_options", "grids",
                                            paste0(gsub(x = Sys.Date(),
                                                        pattern = "-",
                                                        replacement = ""),
                                                   "_goaai_grid_2025_",
                                                   offsets$option[jj],
                                                   ".shp"))
  )

  goa_grid_strata <- sf::st_intersection(grid_poly,
                                        goa_strata)

  goa_grid_strata$AREA <- sf::st_area(goa_grid_strata)


  bbox_1 <- sf::st_bbox(focal_zones$geometry[1])

  p1 <- ggplot() +
    geom_sf(data = goa_layers$akland, color = "grey50") +
    geom_sf(data = goa_grid_strata,
            mapping = aes(fill = as.numeric(AREA)),
            color = "white") +
    scale_x_continuous(limits = c(bbox_1['xmin'], bbox_1['xmax'])) +
    scale_y_continuous(limits = c(bbox_1['ymin'], bbox_1['ymax'])) +
    scale_fill_viridis_c(name = "Area") +
    ggtitle(label = paste0("Offset X: ", offsets$x_off[jj],
                           "; Offset Y: ", offsets$y_off[jj])) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))


  ggplot() +
    geom_sf(data = goa_layers$akland, color = "grey50") +
    geom_sf(data = goa_grid_strata,
            mapping = aes(fill = factor(floor(STRATUM/100))),
            color = "white") +
    scale_fill_viridis_c(name = "Area") +
    ggtitle(label = paste0("Offset X: ", offsets$x_off[jj],
                           "; Offset Y: ", offsets$y_off[jj])) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

  bbox_2 <- sf::st_bbox(focal_zones$geometry[2])

  p2 <- ggplot() +
    geom_sf(data = goa_layers$akland, color = "grey50") +
    geom_sf(data = goa_grid_strata,
            mapping = aes(fill = as.numeric(AREA)),
            color = "white") +
    scale_x_continuous(limits = c(bbox_2['xmin'], bbox_2['xmax'])) +
    scale_y_continuous(limits = c(bbox_2['ymin'], bbox_2['ymax'])) +
    scale_fill_viridis_c(name = "Area") +
    ggtitle(label = paste0("Offset X: ", offsets$x_off[jj],
                           "; Offset Y: ", offsets$y_off[jj])) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none")

  bbox_3 <- sf::st_bbox(focal_zones$geometry[3])

  p3 <- ggplot() +
    geom_sf(data = goa_layers$akland, color = "grey50") +
    geom_sf(data = goa_grid_strata,
            mapping = aes(fill = as.numeric(AREA)),
            color = "white") +
    scale_x_continuous(limits = c(bbox_3['xmin'], bbox_3['xmax'])) +
    scale_y_continuous(limits = c(bbox_3['ymin'], bbox_3['ymax'])) +
    scale_fill_viridis_c(name = "Area") +
    ggtitle(label = paste0("Offset X: ", offsets$x_off[jj],
                           "; Offset Y: ", offsets$y_off[jj])) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none")

  legend <- cowplot::get_legend(p1)


  print(
    ggdraw(cowplot::plot_grid(p1 + theme(legend.position = "none"), p2, p3, legend, nrow = 2))
  )


}

dev.off()



grid_poly_wgs84 <- sf::st_transform(grid_poly, crs = "WGS84")

calc_coord_range <- function(x) {

  coords <- x |>
    sf::st_coordinates()

  coords <- data.frame(x = abs(diff(range(coords[,1]))),
                       y = abs(diff(range(coords[,2]))))

  return(coords)

}

coord_range_lon <- numeric(length = nrow(grid_poly_wgs84))
coord_range_lat <- numeric(length = nrow(grid_poly_wgs84))

for(ii in 1:nrow(grid_poly)) {

  coord_range <- calc_coord_range(grid_poly_wgs84$geometry[[ii]])

  coord_range_lon[ii] <- coord_range$x
  coord_range_lat[ii] <- coord_range$y

}


median(coord_range_lat)
median(coord_range_lon)

hist(coord_range_lon[coord_range_lon < 300])
hist(coord_range_lat[coord_range_lon < 300])

