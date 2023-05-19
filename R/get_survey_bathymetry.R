#' Function to get survey bathymetry layers
#' 
#' Retrieves bathymetric contours that have been used in tech memos and reports through 2019 for the EBS.
#' 
#' @param select.region Region to select
#' @param set.crs CRS to use
#' @return Returns an sf multiline object with survey contours.
#' @export

get_survey_bathymetry <- function(select.region, 
                                  set.crs) {
  
  # Automatically set CRS---------------------------------------------------------------------------
  if(set.crs == "auto") {
    region.crs <- c(
      "+proj=aea +lat_1=55 +lat_2=60 +lat_0=57.5 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=55 +lat_2=60 +lat_0=57.5 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=68.1 +lat_2=70.7 +lat_0=69.4 +lon_0=-162.6 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=60.8 +lat_2=67 +lat_0=63.9 +lon_0=-167 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
    set.crs <- region.crs[match(select.region, c("bs.south", "sebs", "bs.all", "ebs", "bs.north", "nbs", "ecs", "ebs.ecs"))]
  }
  
  if(select.region %in% c("bs.all", "ebs", "nbs", "bs.north")) {
    bathymetry <- sf::st_read(system.file("extdata", "ebs_survey_bathymetry.shp", package = "akgfmaps"), 
                              quiet = TRUE) |>
      sf::st_transform(crs = set.crs)
  } else if(select.region %in% c("bs.south", "sebs")) {
    bathymetry <- sf::st_read(system.file("extdata", "ebs_survey_bathymetry.shp", package = "akgfmaps"), 
                              quiet = TRUE) |>
      sf::st_transform(crs = set.crs) |> 
      dplyr::filter(FNODE_ != 5)
  } else {
    stop(paste0("No survey-specific bathymetry available for ", select.region, ". If using make_idw_map, set use.survey.bathymetry = FALSE."))
  }
  
  return(bathymetry)
  
}

