#' NMFS Statistical Areas
#'
#' Load NMFS statistical areas shapefile.
#'
#' @param set.crs Which coordinate reference system should be used? If 'auto', an Albers Equal Area coordinate reference system is automatically assigned.
#' @return NMFS statistical areas in Alaska as an sf object.
#' @export

get_nmfs_areas <- function(set.crs) {

  if(set.crs == "auto") {
    set.crs <- "EPSG:3338"
  }

  nmfs_areas <- sf::st_read(
    system.file("extdata",
                "NMFS_Reporting_Areas.shp",
                package = "akgfmaps"),
    quiet = TRUE) |>
    dplyr::filter(REP_AREA > 0 & REP_AREA < 660) |> # Select Alaska regions only and exclude land
    sf::st_transform(crs = set.crs) |>
    sf::st_make_valid()

  return(nmfs_areas)

}
