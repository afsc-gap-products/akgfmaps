#' Make a raster, point, or polygon grid from an sf polygon
#' 
#' Creates a raster (terra 'SpatRaster'), point grid (sf 'POINT'), or polygon mesh/'fishnet' (sf 'POLYGON') from an sf polygon.
#' 
#' @param obj An sf polygon.
#' @param resolution Resolution for grid cells in meters. Default = c(3704, 3704), equivalent of 2x2 nautical miles.
#' @param output_type Type of output as a 1L character vector ("point", "raster", "polygon).
#' @param bbox Optional. A 4L numeric vector to define the edges of the interpolation grid c(xmin, ymin, xmax, ymax).
#' @param max_precision Logical.
#' @noRd


make_2d_grid_matrix <- function(obj, resolution = c(3704, 3704), output_type = "point", bbox = NULL, max_precision = TRUE) {
  
  # Extract CRS
  obj_srid <- sf::st_crs(obj, parameters = TRUE)$srid
  
  if(is.null(bbox)) {
    # Find bounding box
    bbox <- sf::st_bbox(obj)
  } else {
    bbox <- c(xmin = bbox[1], ymin = bbox[2], xmax = bbox[3], ymax = bbox[4])
  }
  
  x_coords <- seq(bbox[1], bbox[3], by = resolution[1])
  y_coords <- seq(bbox[2], bbox[4], by = resolution[2])
  x_coords <- c(x_coords, max(x_coords) + resolution[1])
  y_coords <- c(y_coords, max(y_coords) + resolution[2])
  
  x_vec <- seq(1, length(x_coords), by = 1)
  y_vec <- seq(1, length(y_coords), by = 1)
  
  n_x_vec <- length(x_vec)
  n_y_vec <- length(y_vec)
  
  x_ind <- matrix(x_vec, ncol = n_x_vec, nrow = n_y_vec, byrow = TRUE)
  y_ind <- matrix(y_vec, ncol = n_x_vec, nrow = n_y_vec)
  
  x_mat <- matrix(x_coords, ncol = n_x_vec, nrow = n_y_vec, byrow = TRUE)
  y_mat <- matrix(y_coords, ncol = n_x_vec, nrow = n_y_vec)
  
  
  x_1 <- x_mat[c(1:(nrow(x_mat)-1)), c(1:(ncol(x_mat)-1))]
  x_2 <- x_mat[c(2:(nrow(x_mat))), c(2:(ncol(x_mat)))]
  
  y_1 <- y_mat[c(1:(nrow(y_mat)-1)), c(1:(ncol(y_mat)-1))]
  y_2 <- y_mat[c(2:(nrow(y_mat))), c(2:(ncol(y_mat)))]
  
  # Create WKT for polygons
  poly_wkt <-  paste0("POLYGON((",
                      paste(paste(x_1, y_1, sep = " "), 
                            paste(x_2, y_1, sep = " "), 
                            paste(x_2, y_2, sep = " "), 
                            paste(x_1, y_2, sep = " "), 
                            paste(x_1, y_1, sep = " "), sep = ", "), "))")
  
  df <- data.frame(CELL_ID = 1:length(x_1), 
                   geom = poly_wkt)
  
  df <- sf::st_as_sf(df, wkt = "geom", crs = obj_srid)
  
  df_int <- df %>% 
    sf::st_intersection(obj, model = "semi-open") 
  
  df_int <- df_int %>%
    dplyr::filter(sf::st_geometry_type(df_int) != "POINT")
  
  df_int$AREA <- sf::st_area(df_int)
  
  df_centroid <- sf::st_centroid(df_int)
  
  if(output_type == "point") {
    return(df_centroid)
  } else if(output_type == "polygon") {
    return(df_int)
  }
  
}

