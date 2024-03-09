library(akgfmaps)
library(dplyr)
library(ggthemes)
library(cowplot)

ai_grid_3338 <- akgfmaps::get_base_layers(select.region = "ai", set.crs = "EPSG:3338")$survey.grid |>
  tidyr::separate(ID, into = c("prefix", "suffix"), sep = "-", remove = FALSE) |>
  dplyr::mutate(prefix = as.numeric(prefix),
                suffix = as.numeric(suffix))

goa_grid_3338 <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "EPSG:3338")$survey.grid |>
  tidyr::separate(ID, into = c("prefix", "suffix"), sep = "-", remove = FALSE) |>
  dplyr::mutate(prefix = as.numeric(prefix),
                suffix = as.numeric(suffix))


# Correct dateline wrapping
ai_grid_nad83 <- sf::st_transform(ai_grid_3338, crs = "NAD83") |>
  sf::st_wrap_dateline()

goa_grid_nad83 <- sf::st_transform(goa_grid_3338, crs = "NAD83")

# Both AI and GOA grids use grid naming conventions where:
# - The prefix (before the dash) runs from west to east
# - The suffix (after the dash) runs from south to north
ggplot() +
  geom_sf(data = dplyr::bind_rows(goa_grid_3338,
                                  ai_grid_3338),
          mapping = aes(fill = prefix),
          color = NA)

ggplot() +
  geom_sf(data = dplyr::bind_rows(goa_grid_3338,
                                  ai_grid_3338),
          mapping = aes(fill = suffix),
          color = NA)

ggplot() +
  geom_sf(data = ai_grid_3338,
          mapping = aes(fill = suffix),
          color = NA)

# In the ID fields for the GOA and AI grids, there are:
# - 491 GOA grid cells that have the same name as AI grid cells
# - 758 AI grid cells that have the same name as GOA grid cells
length(which(goa_grid_3338$ID %in% ai_grid_3338$ID))
length(which(ai_grid_3338$ID %in% goa_grid_3338$ID))

overlapping_ids <- dplyr::bind_rows(
  dplyr::mutate(goa_grid_3338,
                overlapping_ID = ID %in% ai_grid_3338$ID),
  dplyr::mutate(ai_grid_3338,
                overlapping_ID = ID %in% goa_grid_3338$ID)
  )

# This is where grid IDs overlap
ggplot() +
  geom_sf(data = overlapping_ids,
          mapping = aes(fill = overlapping_ID),
          color = NA) +
  scale_fill_colorblind()

overlapping_only <- dplyr::filter(overlapping_ids, overlapping_ID)

# The minimum GOA and AI grid prefixes are 0 and 8, respectively
min(goa_grid_3338$prefix)
min(ai_grid_3338$prefix)

# The minimum GOA and AI grid suffixes are 0 and 8, respectively
min(goa_grid_3338$suffix)
min(ai_grid_3338$suffix)

# Prefix and suffix ranges

ai_prefix_range <- range(ai_grid_3338$prefix)
ai_suffix_range <- range(ai_grid_3338$suffix)

goa_prefix_range <- range(goa_grid_3338$prefix)
goa_suffix_range <- range(goa_grid_3338$suffix)


# If the GOA and AI grids were created using a similar process, it appears there were 4-5 grid cells
# north of the AI grid and 8 grid cells west of the AI grid. The GOA grid appears to start at the origin
cowplot::plot_grid(
  ggplot() +
    geom_sf(data = dplyr::mutate(goa_grid_3338,
                                 prefix_origin = prefix == 0),
            mapping = aes(fill = prefix_origin),
            color = NA) +
    geom_sf(data = dplyr::mutate(goa_grid_3338,
                                 prefix_origin = prefix == 0) |>
              sf::st_centroid(),
            mapping = aes(color = prefix_origin)) +
    scale_fill_manual(values = c("grey80", "red")) +
    scale_color_manual(values = c("grey80", "red")) +
    theme_bw(),
  ggplot() +
    geom_sf(data = dplyr::mutate(goa_grid_3338,
                                 suffix_origin = suffix == 0),
            mapping = aes(fill = suffix_origin),
            color = NA) +
    geom_sf(data = dplyr::mutate(goa_grid_3338,
                                 suffix_origin = suffix == 0) |>
              sf::st_centroid(),
            mapping = aes(color = suffix_origin)) +
    scale_fill_manual(values = c("grey80", "red")) +
    scale_color_manual(values = c("grey80", "red")) +
    theme_bw(),
  nrow = 2
)

cowplot::plot_grid(
  ggplot() +
    geom_sf(data = dplyr::mutate(ai_grid_3338,
                                 prefix_origin = prefix == 8),
            mapping = aes(fill = prefix_origin), color = NA) +
    geom_sf(data = dplyr::mutate(ai_grid_3338,
                                 prefix_origin = prefix == 8) |>
              sf::st_centroid(),
            mapping = aes(color = prefix_origin)) +
    scale_fill_manual(values = c("grey80", "red")) +
    scale_color_manual(values = c("grey80", "red")) +
    theme_bw(),
  ggplot() +
    geom_sf(data = dplyr::mutate(ai_grid_3338,
                                 suffix_origin = suffix == 5),
            mapping = aes(fill = suffix_origin), color = NA) +
    geom_sf(data = dplyr::mutate(ai_grid_3338,
                                 suffix_origin = suffix == 5) |>
              sf::st_centroid(),
            mapping = aes(color = suffix_origin)) +
    scale_fill_manual(values = c("grey80", "red")) +
    scale_color_manual(values = c("grey80", "red")) +
    theme_bw(),
  nrow = 2)

# We can drop fields other than ID and geometry, then union the GOA and AI grids based on grid ID

# GOA grid IDs are split among 1-9 polygons. AI grid IDs split among 1-13 polygons
range(as.numeric(table(goa_grid_3338$ID)))
range(as.numeric(table(ai_grid_3338$ID)))
hist(as.numeric(table(ai_grid_3338$ID)))
hist(as.numeric(table(goa_grid_3338$ID)))

ai_grid_union <- dplyr::select(ai_grid_3338, ID) |>
  dplyr::group_by(ID) |>
  summarise()

ai_grid_union$area <- as.numeric(sf::st_area(ai_grid_union))

goa_grid_union <- dplyr::select(goa_grid_3338, ID) |>
  dplyr::group_by(ID) |>
  summarise()

goa_grid_union$area <- as.numeric(sf::st_area(goa_grid_union))


# Polygons are comprised of lines that intersect at beginning and end coordinates

# Trial and error to figure out which grid cells are 'typically sized', i.e. are squares
# Note that there are way more in the GOA because the bathymetry isn't as steep
# Once again, we add ID prefixes and suffixes
ai_area_quantiles <- as.numeric(quantile(ai_grid_union$area, c(0.71, 0.99)))

ggplot() +
  geom_sf(data = (dplyr::filter(ai_grid_union,
                                area >= ai_area_quantiles[1] & area <= ai_area_quantiles[2]) |>
                    dplyr::arrange(area))[1,],
          linewidth = 3)

ggplot() +
  geom_sf(data = (dplyr::filter(ai_grid_union,
                                area >= ai_area_quantiles[1] & area <= ai_area_quantiles[2]) |>
                    dplyr::arrange(-area))[1,],
          linewidth = 3)

goa_area_quantiles <- as.numeric(quantile(goa_grid_union$area, c(0.24, 1)))

ggplot() +
  geom_sf(data = (dplyr::filter(goa_grid_union,
                                area >= goa_area_quantiles[1] & area <= goa_area_quantiles[2]) |>
                    dplyr::arrange(area))[1,],
          linewidth = 3)

ggplot() +
  geom_sf(data = (dplyr::filter(goa_grid_union,
                                area >= goa_area_quantiles[1] & area <= goa_area_quantiles[2]) |>
                    dplyr::arrange(-area))[1,],
          linewidth = 3)

ai_grid_whole_cells <- dplyr::filter(ai_grid_union,
                                     area >= ai_area_quantiles[1] & area <= ai_area_quantiles[2]) |>
  tidyr::separate(ID, into = c("prefix", "suffix"), sep = "-", remove = FALSE) |>
  dplyr::mutate(prefix = as.numeric(prefix),
                suffix = as.numeric(suffix))

goa_grid_whole_cells <- dplyr::filter(goa_grid_union,
                                      area >= goa_area_quantiles[1] & area <= goa_area_quantiles[2]) |>
  tidyr::separate(ID, into = c("prefix", "suffix"), sep = "-", remove = FALSE) |>
  dplyr::mutate(prefix = as.numeric(prefix),
                suffix = as.numeric(suffix))

# Visualizing the locations of whole cells
# Atypical cells tend to appear near land and along the edges of the survey area
ggplot() +
  geom_sf(data = dplyr::mutate(ai_grid_union,
                               typical_cell = area >= ai_area_quantiles[1] & area <= ai_area_quantiles[2]),
          mapping = aes(fill = typical_cell),
          color = NA) +
  scale_fill_manual(values = c("black", "red")) +
  theme_bw()

ggplot() +
  geom_sf(data = dplyr::mutate(goa_grid_union,
                               typical_cell = area >= goa_area_quantiles[1] & area <= goa_area_quantiles[2]),
          mapping = aes(fill = typical_cell),
          color = NA) +
  scale_fill_manual(values = c("black", "red")) +
  theme_bw()


ggplot() +
  geom_sf(data = dplyr::bind_rows(dplyr::mutate(ai_grid_union,
                                                typical_cell = area >= ai_area_quantiles[1] & area <= ai_area_quantiles[2]),
                                  dplyr::mutate(goa_grid_union,
                               typical_cell = area >= goa_area_quantiles[1] & area <= goa_area_quantiles[2])),
          mapping = aes(fill = typical_cell),
          color = NA) +
  scale_fill_manual(values = c("black", "red")) +
  theme_bw()


# Grids are comprised of lines and lines intersect at points. We need to create lines that run along the original rows and columns in order to reconstruct the grid.

# To do so, we first create POLYGON/MULTIPOLYGONs with rows and columns of grid cells that are grouped based on ID prefixes (columns) and ID suffixes (rows)
# POLYGONS/MULTIPOLYGONS are shown for illustrative purposes
goa_grid_columns <- goa_grid_whole_cells |>
  select(-ID, -suffix) |>
  dplyr::group_by(prefix) |>
  dplyr::summarise()

goa_grid_rows <- goa_grid_whole_cells |>
  select(-ID, -prefix) |>
  dplyr::group_by(suffix) |>
  dplyr::summarise()

which.max(table(goa_grid_whole_cells$prefix))
which.max(table(goa_grid_whole_cells$suffix))

ggplot() +
  geom_sf(data = dplyr::filter(goa_grid_rows, suffix == 166), fill = NA, color = "red") +
  geom_sf(data = dplyr::filter(goa_grid_columns, prefix == 217), fill = NA, color = "blue")

ai_grid_columns <- ai_grid_whole_cells |>
  select(-ID, -suffix) |>
  dplyr::group_by(prefix) |>
  dplyr::summarise()

ai_grid_rows <- ai_grid_whole_cells |>
  select(-ID, -prefix) |>
  dplyr::group_by(suffix) |>
  dplyr::summarise()

which.max(table(ai_grid_whole_cells$prefix))
which.max(table(ai_grid_whole_cells$suffix))

ggplot() +
  geom_sf(data = dplyr::filter(ai_grid_rows, suffix == 30), fill = NA, color = "red") +
  geom_sf(data = dplyr::filter(ai_grid_columns, prefix == 264), fill = NA, color = "blue")

# POLYGONS are constructed from vertices, meaning that the first and last points in the geometry must be the same to create an enclosed polygon
# To find the starting and ending points, we first cast MULTIPOLYGONs to POLYGONs. Then, we then find the intersection point for each POLYGON

# goa_grid_columns$geom_type <- sf::st_geometry_type(goa_grid_columns)
#
# goa_grid_multi <- dplyr::filter(goa_grid_columns, geom_type == "MULTIPOLYGON")
# goa_grid_single <- dplyr::filter(goa_grid_columns, geom_type == "POLYGON")
#
# goa_grid_columns_poly <- dplyr::bind_rows(sf::st_cast(goa_grid_multi, to = "POLYGON"),
#                                           goa_grid_single)
#
# goa_grid_rows$geom_type <- sf::st_geometry_type(goa_grid_rows)
#
# goa_grid_multi <- dplyr::filter(goa_grid_rows, geom_type == "MULTIPOLYGON")
# goa_grid_single <- dplyr::filter(goa_grid_rows, geom_type == "POLYGON")
#
# goa_grid_rows_poly <- dplyr::bind_rows(sf::st_cast(goa_grid_multi , to = "POLYGON"),
#                                           goa_grid_single)
#
# ai_grid_columns$geom_type <- sf::st_geometry_type(ai_grid_columns)
#
# ai_grid_multi <- dplyr::filter(ai_grid_columns, geom_type == "MULTIPOLYGON")
# ai_grid_single <- dplyr::filter(ai_grid_columns, geom_type == "POLYGON")
#
# ai_grid_columns_poly <- dplyr::bind_rows(sf::st_cast(ai_grid_multi , to = "POLYGON"),
#                                          ai_grid_single)
#
# ai_grid_rows$geom_type <- sf::st_geometry_type(ai_grid_rows)
#
# ai_grid_multi <- dplyr::filter(ai_grid_rows, geom_type == "MULTIPOLYGON")
# ai_grid_single <- dplyr::filter(ai_grid_rows, geom_type == "POLYGON")
#
# goa_grid_columns_poly <- dplyr::bind_rows(sf::st_cast(ai_grid_multi, to = "POLYGON"),
#                                           ai_grid_single)
#
#
# # Verify that we've successfully split MULTIPOLYGONs into POLYGONs
#
# cowplot::plot_grid(
#   ggplot() +
#     geom_sf(data = dplyr::filter(goa_grid_columns, prefix == 217),
#             fill = "red") +
#     geom_sf(data = dplyr::filter(goa_grid_columns_poly, prefix == 217),
#             color = "black", fill = NA, linewidth = 1.2),
#   ggplot() +
#     geom_sf(data = dplyr::filter(goa_grid_rows, suffix == 166),
#             fill = "red") +
#     geom_sf(data = dplyr::filter(goa_grid_rows_poly, suffix == 166),
#             color = "black", fill = NA, linewidth = 1.2)
# )
#
# cowplot::plot_grid(
#   ggplot() +
#     geom_sf(data = dplyr::filter(ai_grid_columns, prefix == 264),
#             fill = "red") +
#     geom_sf(data = dplyr::filter(ai_grid_columns_poly, prefix == 264),
#             color = "black", fill = NA, linewidth = 1.2),
#   ggplot() +
#     geom_sf(data = dplyr::filter(ai_grid_rows, suffix == 30),
#             fill = "red") +
#     geom_sf(data = dplyr::filter(ai_grid_rows_poly, suffix == 30),
#             color = "black", fill = NA, linewidth = 1.2)
# )
#
#
# find_intersection <- function(x) {
#
#   poly_coords <- sf::st_coordinates(x)
#
#   lat_counts <- table(poly_coords[,1])
#   lon_counts <- table(poly_coords[,2])
#
#   intersection_lat <- names(which.max(lat_counts))
#   intersection_lon <- names(which.max(lon_counts))
#
#   out <- data.frame(lon = as.numeric(intersection_lon),
#                     lat = as.numeric(intersection_lat)
#                     )
#
#   return(out)
#
# }
#
# wrapper_find_intersection <- function(x_poly) {
#
#   poly_start <- data.frame()
#
#   for(ii in 1:nrow(x_poly)) {
#
#     poly_start <- dplyr::bind_rows(
#       poly_start,
#       find_intersection(x = x_poly$geometry[[ii]])
#     )
#
#
#   }
#
#   return(poly_start)
#
# }

# Now we find the intersection points for the survey grids
check_crs <- function(whole_cells_sf, try.crs) {

  centroid_sf <- whole_cells_sf |>
    sf::st_transform(crs = try.crs) |>
    sf::st_centroid() |>
    sf::st_coordinates() |>
    as.data.frame()

  centroid_sf <- dplyr::bind_cols(whole_cells_sf, centroid_sf) |>
    as.data.frame()

  lon_df <- dplyr::group_by(centroid_sf, prefix) |>
    dplyr::summarise(min_X = min(X),
                     max_X = max(X),
                     min_Y = min(Y),
                     max_Y = max(Y),
                     n = n()) |>
    as.data.frame() |>
    dplyr::arrange(prefix)

  lat_df <- dplyr::group_by(centroid_sf, suffix) |>
    dplyr::summarise(min_Y = min(Y),
                     max_Y = max(Y),
                     min_Y = min(Y),
                     max_Y = max(Y),
                     n = n()) |>
    as.data.frame() |>
    dplyr::arrange(suffix)

  lon_df$diff_X <- lon_df$max_X-lon_df$min_X
  lat_df$diff_Y <- lat_df$max_Y-lat_df$min_Y

  lat_spacing <- range(diff(lat_df$min_Y)/diff(lat_df$suffix))
  lon_spacing <- range(diff(lon_df$min_X)/diff(lon_df$prefix))

  lat_delta <- mean(diff(diff(lat_df$min_Y)/diff(lat_df$suffix)))

  lon_delta_crosses <- diff(diff(lon_df$min_X)/diff(lon_df$prefix))

  lon_delta <- mean(lon_delta_crosses)

  return(list(lon_df = lon_df,
              lat_df = lat_df,
              lat_delta = lat_delta,
              lon_delta = lon_delta,
              range_x_diff = range(lon_df$diff_X),
              range_y_diff = range(lat_df$diff_Y),
              lat_spacing = lat_spacing,
              lon_spacing = lon_spacing))
}



(output <- check_crs(whole_cells = goa_grid_whole_cells,
          try.crs = "NAD83"))

cor(output$lon_df$prefix, output$lon_df$diff_X)
cor(output$lat_df$suffix, output$lat_df$diff_Y)

range(goa_grid_whole_cells$prefix)
range(goa_grid_whole_cells$prefix)

# what do you need to create a standard grid?
# - Coordinate reference system
# - Origin
# - Grid cell resolution
# - Number of cells in each direction/bounding box

# Plotting versus interpolation
# - Geographic (e.g. WGS84)
# - Distance Conserving (e.g. Universal Transverse Mercator)
# - Area Conserving (e.g. Alaska Albers Equal Area)

goa_box <- data.frame(x = c(-170, -170, -133, -133, -170),
                      y = c(52, 60.5, 60.5, 52, 52)) |>
  sf::st_as_sf(coords = c("x", "y"), crs = "WGS84") |>
  dplyr::group_by('GOA') |>
  summarise(do_union = FALSE) |>
  sf::st_cast(to = "POLYGON")

terra::rast()

range(goa_grid_whole_cells$prefix)
range(goa_grid_whole_cells$suffix)
