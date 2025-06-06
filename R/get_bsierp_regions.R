#' Bering Sea Integrated Ecosystem Research Program (BSIERP) regions
#'
#' Retrieve BSIERP region polygons.
#'
#' @param set.crs Which coordinate reference system should be used? If 'auto', Alaska Albers Equal Area coordinate reference system (EPSG:3338) is automatically assigned.
#' @export
#' @examples \dontrun{
#' library(akgfmaps)
#'
#' bsierp_regions <- get_bsierp_regions(set.crs = "EPSG:3338")
#'
#' bsierp_centroid <- sf::st_centroid(bsierp_regions)
#'
#' ggplot() +
#'   geom_sf(data = bsierp_regions) +
#'   geom_sf_text(data = bsierp_centroid,
#'                mapping = aes(label = BSIERP_Region_Name))}

get_bsierp_regions <- function(set.crs) {

  if(set.crs == "auto") {
    set.crs = "EPSG:3338"
  }


  layer <- sf::st_read(system.file("extdata", "Alaska_Marine_Management_Areas.gdb", package = "akgfmaps"),
                       layer = "Alaska_Marine_Areas_AK_prj",
                       quiet = TRUE)

  layer <- layer[layer$Area_Type == "BSIERP Region", ]
  layer <- layer[c("Area_Name", "BSIERP_ID", "BSIERP_Region_Name", "Shape_Area")]

  names(layer)[names(layer) == "Area_Name"] <- "AREA_NAME"
  names(layer)[names(layer) == "Shape_Area"] <- "AREA_M2"

  sf::st_geometry(layer)<- "geometry"

  layer <- layer |>
    sf::st_transform(crs = set.crs) |>
    fix_geometry()

  return(layer)

}
