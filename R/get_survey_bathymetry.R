#' Function to get survey bathymetry layers
#'
#' Retrieves bathymetric contours that approximate groundfish bottom trawl survey stratum boundaries on the EBS shelf.
#'
#' @param select.region Character vector indicating which region to retrieve. Options = ebs or bs.all, sebs or bs.south, nbs or bs.north
#' @param set.crs Which coordinate reference system should be used? If 'auto', Alaska Albers Equal Area (EPSG:3338) will be used.
#' @return Returns an sf multiline object with survey contours.
#' @export

get_survey_bathymetry <- function(select.region,
                                  set.crs) {

  # Automatically set CRS---------------------------------------------------------------------------
  if(set.crs == "auto") {
    message("get_survey_bathymetry: select.region = 'auto' now uses Alaska Albers Equal Area (EPSG:3338) by default. This replaces the custom equal area projection PROJ4 strings that were used prior to akgfmaps 3.6.0.")
    set.crs <- "EPSG:3338"
  }

  if(select.region %in% c("bs.all", "ebs", "nbs", "bs.north")) {
    bathymetry <- sf::st_read(system.file("extdata", "ebs_survey_bathymetry.shp", package = "akgfmaps"),
                              quiet = TRUE) |>
      sf::st_transform(crs = set.crs)
  } else if(select.region %in% c("bs.south", "sebs")) {
    bathymetry <- sf::st_read(system.file("extdata", "ebs_survey_bathymetry.shp", package = "akgfmaps"),
                              quiet = TRUE) |>
      sf::st_transform(crs = set.crs)

    bathymetry <- bathymetry[bathymetry$FNODE_ != 5, ]

  } else {
    stop(paste0("No survey-specific bathymetry available for ", select.region, ". If using make_idw_map, set use.survey.bathymetry = FALSE."))
  }

  bathymetry <- fix_geometry(bathymetry)


  return(bathymetry)

}

