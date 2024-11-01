#' Convert sf to a data.frame with WKT geometry
#'
#' This function converts a simple features (sf) geometry to a data.frame containing well-known text(WKT) representation of vector geometries and calculates the area of POLYGONS.
#'
#' @param x A simple features objects
#' @param area.crs Character vector or numeric indicating which coordinate reference system to use for calculating areas. The default is Alaska Albers Equal Area ("EPSG:3338")
#' @param wkt.crs Character vector or numeric indicating which coordinate reference system to use for WKT outputs. The default is WGS84 ("WGS84")
#' @import units
#' @export

make_wkt_table <- function(x, area.crs, wkt.crs = "WGS84") {

  x$AREA_KM2 <- x |>
    sf::st_transform(crs = area.crs) |>
    sf::st_make_valid() |>
    sf::st_area() |>
    units::set_units(km^2) |>
    as.numeric()

  output <- sf::st_transform(x, wkt.crs = "WGS84") |>
    sf::st_make_valid()

  output$wkt <- sf::st_as_text(output$geometry)

  output <- output |>
    as.data.frame() |>
    dplyr::select(-geometry)

  return(output)

}
