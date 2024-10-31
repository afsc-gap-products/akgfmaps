#' Bering Sea Integrated Ecosystem Research Program (BSIERP) regions
#'
#' Retrieve BSIERP region polygons.
#'
#' @param set.crs Which coordinate reference system should be used? If 'auto', Alaska Albers Equal Area coordinate reference system (EPSG:3338) is automatically assigned.
#' @export

get_bsierp_regions <- function(set.crs) {

  if(set.crs == "auto") {
    set.crs = "EPSG:3338"
  }


  layer <- sf::st_read(here::here("inst/extdata/Alaska_Marine_Management_Areas.gdb"),
                       layer = "Alaska_Marine_Areas_AK_prj") |>
    dplyr::filter(Area_Type == "BSIERP Region") |>
    dplyr::select(AreaID, Area_Name, BSIERP_ID, BSIERP_Region_Name, Area_m2 = Shape_Area) |>
    dplyr::arrange(BSIERP_ID)

  sf::st_geometry(layer)<- "geometry"


  layer <- layer |>
    sf::st_transform(crs = set.crs)

  return(layer)

}
