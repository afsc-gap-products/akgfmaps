#' Make a raster, point, or polygon grid from an sf polygon
#'
#' Creates a raster (terra 'SpatRaster'), point grid (sf 'POINT'), or polygon mesh/'fishnet' (sf 'POLYGON') from an sf polygon.
#'
#' @param obj An sf polygon.
#' @param resolution Resolution for grid cells. Default = c(3704, 3704), equivalent of 2x2 nautical miles.
#' @param output_type Type of output as a 1L character vector ("point", "raster", "polygon).
#' @param bbox Optional. A 4L numeric vector to define the edges of the interpolation grid c(xmin, ymin, xmax, ymax).
#' @param model Character vector indicating the geometry model to use for st_intersection boundaries. Default = "semi-open"; for details see ?s2::s2_options
#' @param include_tile_center Should the center point of each tile be included in the output? This helps with plotting data because the center point for grid locations after interecting with the stratum shapefile is not always the center of a grid cell since intersected cells are not always squares.
#' @export

make_2d_grid <- function(obj, resolution = c(3704, 3704), output_type = "point", bbox = NULL, model = "semi-open", include_tile_center = FALSE) {

  # Extract CRS
  obj_srid <- sf::st_crs(obj, parameters = TRUE)$srid

  if(is.null(bbox)) {
    # Find bounding box
    bbox <- sf::st_bbox(obj)
    buffer <- resolution
    obj_mask <- FALSE
  } else {
    bbox <- c(xmin = bbox[1], ymin = bbox[2], xmax = bbox[3], ymax = bbox[4])
    buffer <- c(0,0)
    obj_mask <- TRUE
  }

  # Create grid
  interp_grid <- terra::rast(xmin = bbox[1],
                             xmax = bbox[3]+buffer[1],
                             ymin = bbox[2],
                             ymax = bbox[4]+buffer[2],
                             resolution = resolution,
                             crs = obj_srid)

  # Only use area within and around obj extent to minimize processing time
  if(obj_mask) {
    message("Trimming interp_grid based on obj extent")
    obj_bbox <- sf::st_bbox(obj)
    interp_mask <- terra::rast(xmin = obj_bbox[1]-resolution[1]*2, # Two cell buffer
                               xmax = obj_bbox[3]+resolution[2]*2,
                               ymin = obj_bbox[2]-resolution[1]*2,
                               ymax = obj_bbox[4]+resolution[2]*2,
                               resolution = resolution,
                               crs = obj_srid)

    interp_grid <- terra::crop(interp_grid, interp_mask)
  }

  # Populate grid with unique cell numbers
  interp_grid[] <- 1:terra::ncell(interp_grid)

  names(interp_grid)[1] <- "CELL_ID"

  if(output_type == "raster") {

    interp_grid <- terra::mask(interp_grid, obj, touches = FALSE)

    return(interp_grid)

  }

  # Convert to polygon and find intersection with obj
  interp_polygons <- terra::as.polygons(interp_grid) |>
    sf::st_as_sf(crs = obj_srid)

  # Add tile center coordinates for plotting
  if(include_tile_center) {

    coords_for_plots <- sf::st_centroid(interp_polygons) |>
      sf::st_coordinates() |>
      as.data.frame() |>
      dplyr::rename(lon_plot = X, lat_plot = Y)

    interp_polygons <- interp_polygons |>
      dplyr::bind_cols(coords_for_plots)

  }

  interp_polygons <- interp_polygons |>
    sf::st_intersection(obj, model = model)

  # Convert to square kilometers
  interp_polygons$AREA <- sf::st_area(interp_polygons)

  if(output_type == "polygon") {

    return(interp_polygons)

  }

  # Create point object using polygon centroids
  interp_centroid <- sf::st_centroid(interp_polygons)

  return(interp_centroid)

}
