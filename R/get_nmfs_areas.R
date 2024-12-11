#' NMFS Statistical Areas
#'
#' Load NMFS statistical areas polygons.
#'
#' @param set.crs Which coordinate reference system should be used? If 'auto', an Albers Equal Area coordinate reference system is automatically assigned.
#' @return NMFS statistical areas in Alaska as an sf object.
#' @export
#' @examples \dontrun{
#' library(akgfmaps)
#'
#' # Get all NMFS Alaska areas
#'
#' nmfs_areas <- get_nmfs_areas(set.crs = "EPSG:3338")
#' nmfs_areas_centroid <- sf::st_centroid(nmfs_areas)
#'
#' ggplot() +
#'   geom_sf(data = nmfs_areas) +
#'   geom_sf_text(data = nmfs_areas_centroid,
#'                mapping = aes(label = REP_AREA))}

get_nmfs_areas <- function(set.crs) {

  if(set.crs == "auto") {
    set.crs <- "EPSG:3338"
  }

  nmfs_areas <- sf::st_read(
    system.file("extdata",
                "NMFS_Reporting_Areas.shp",
                package = "akgfmaps"),
    quiet = TRUE)

  nmfs_areas[["AREA_M2"]] <- nmfs_areas[["Shape_Area"]]
  nmfs_areas <- nmfs_areas[c("REP_AREA", "AREA_M2")]

  # Select Alaska regions only and exclude land
  nmfs_areas <- nmfs_areas[nmfs_areas$REP_AREA > 0 & nmfs_areas$REP_AREA < 660, ] |>
    sf::st_transform(crs = set.crs) |>
      fix_geometry()

  return(nmfs_areas)

}
