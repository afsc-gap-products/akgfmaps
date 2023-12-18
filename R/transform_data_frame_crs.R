#' Transform data frame to new coordinate reference system
#'
#' @param x Input data frame.
#' @param coords Vector of names of coordinate column with order longitude and latitude. Default c("x", "y")
#' @param in.crs Coordinate reference system for input data frame. Default = '+proj=longlat'
#' @param out.crs Coordinate reference system for data frame to return. Default = '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'
#' @param only.new.coords Should only the new coordinates be returned without the input data frame? Default = FALSE
#' @return Returns the input data frame with coordinates converted, or a data frame containing just the coordinates, if \code{only.new.coords == TRUE}.
#' @author Sean K. Rohan, \email{sean.rohan@@noaa.gov}
#' @export

transform_data_frame_crs <- function(x, coords = c("x", "y"),
                                     in.crs,
                                     out.crs,
                                     only.new.coords = FALSE) {

  if(nrow(x) > 0) {
    new.coords <- sf::st_as_sf(x, coords = coords,
                               crs = sf::st_crs(in.crs)) |>
      sf::st_transform(crs = sf::st_crs(out.crs)) |>
      sf::st_coordinates() |>
      as.data.frame()

    if(only.new.coords) {
      return(new.coords)
    } else {
      x[coords[1]] <- new.coords$X
      x[coords[2]] <- new.coords$Y
    }
  }
  return(x)
}
