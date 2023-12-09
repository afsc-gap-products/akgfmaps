#' NMFS Statistical Areas
#'
#' Load NMFS statistical areas shapefile.
#'
#' @param set.crs Which coordinate reference system should be used? If 'auto', an Albers Equal Area coordinate reference system is automatically assigned.
#' @return NMFS statistical areas in Alaska as an sf object.
#' @export

get_nmfs_areas <- function(set.crs) {

  if(set.crs == "auto") {
    set.crs <- "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
  }

  nmfs_areas <- sf::st_read(
    system.file("extdata",
                "NMFS Reporting Areas.shp",
                package = "akgfmaps"),
    quiet = TRUE) |>
    dplyr::filter(REP_AREA > 0 & REP_AREA < 651) |> # Select Alaska regions only and exclude land
    sf::st_transform(crs = set.crs) |>
    sf::st_make_valid()

  return(nmfs_areas)

}
