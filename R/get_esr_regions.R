#' Alaska Ecosystem Status Report (ESR) large marine ecosystem regions
#'
#' Retrieve Alaska Ecosystem Status Report area polygons.
#'
#' @param select.region Character vector indicating which region to retrieve. Options are: 'esr_subarea', 'esr_area', 'esr_suberea_inside', 'esr_area_inside'
#' @param set.crs Which coordinate reference system should be used? If 'auto', Alaska Albers Equal Area coordinate reference system (EPSG:3338) is automatically assigned.
#' @export

get_esr_regions <- function(select.region = "esr_subarea", set.crs) {

  if(set.crs == "auto") {
    set.crs = "EPSG:3338"
  }

  stopifnot("get_esr_regions: Invalid select.region. Must be one of: 'esr_subarea', 'esr_area', 'esr_subarea_inside', 'esr_area_inside'"
            = select.region %in% c('esr_subarea', 'esr_area', 'esr_subarea_inside', 'esr_area_inside'))

  area_type <- switch(select.region,
         'esr_subarea' = "Ecosystem Subarea",
         'esr_area' = "Ecosystem Area",
         'esr_subarea_inside' = "Ecosystem Subarea Inside",
         'esr_area_inside'= "Ecosystem Area Inside"
         )

  layer <- suppressWarnings(sf::st_read(here::here("inst/extdata/Alaska_Marine_Management_Areas.gdb"),
                       layer = "Alaska_Marine_Areas_AK_prj",
                       quiet = TRUE))

  layer <- layer[layer$Area_Type == area_type, ]

  layer$AREA_TYPE <- layer$Area_Type
  layer$AREA_NAME <- layer$Area_Name
  layer$AREA_M2 <- layer$Shape_Area

  layer <- layer[c("AREA_TYPE", "AREA_NAME", "AREA_M2", "Shape")]

  sf::st_geometry(layer)<- "geometry"

  layer <- layer |>
    sf::st_transform(crs = set.crs) |>
    wrap_dateline_silent()

  return(layer)

}
