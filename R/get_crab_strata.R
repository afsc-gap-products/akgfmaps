#' Get crab stratum layers
#'
#' Load crab strata polygons for red king crab, blue king crab, snow crab, or Tanner crab. Species and areas are described below.
#'
#' @param select.unit Select a species and management unit as a character vector. Options are Bristol Bay RKC ('bbrkc'), Pribilof Islands RKC ('pirkc'), Pribilof Islands BKC ('pibkc'), St. Matthew's BKC ('smbkc'), Norton Sound RKC ('nsrkc'), eastern Bering Sea snow crab ('ebssc'), and eastern Bering Sea Tanner crab ('ebstc').
#' @param set.crs Which coordinate reference system should be used? If 'auto', Alaska Albers Equal Area coordinate reference system (EPSG:3338) is automatically assigned.
#' @export

get_crab_strata <- function(select.unit, set.crs) {

  valid_units <- c("bbrkc", "pirkc", "pibkc", "nsrkc", "ebssc", "ebstc", "smbkc")

  select_unit <- tolower(select.unit)

  stopifnot("get_crab_strata: Unit must be one of 'bbrkc', 'pirkc', 'pibkc', 'nsrkc', 'ebssc', 'ebstc', 'smbkc'" = select_unit %in% valid_units)

  sys_filename <- switch(select_unit,
                         "bbrkc" = "BBRKC_strata.shp",
                         "pirkc" = "Pribilof_RKC_strata.shp",
                         "pibkc" = "Pribilof_BKC_strata.shp",
                         "nsrkc" = "Norton_RKC_Strata.shp",
                         "ebssc" = "EBS_CO_CB_strata.shp",
                         "ebstc" = "EBS_CO_CB_strata.shp",
                         "smbkc" = "StMatt_BKC_strata.shp")

  stratum_name <- switch(select_unit,
                         "bbrkc" = "BBRKC",
                         "pirkc" = "Pribilof_RKC",
                         "pibkc" = "Pribilof_BKC",
                         "nsrkc" = "Norton_RKC",
                         "ebssc" = "EBS_CO_CB",
                         "ebstc" = "EBS_CO_CB",
                         "smbkc" = "StMatt_BKC")

  crab_strata <- sf::st_read(system.file("extdata", "crab_strata", sys_filename, package = "akgfmaps"),
                             quiet = TRUE) |>
    sf::st_transform(crs = set.crs) |>
    fix_geometry()

  names(crab_strata)[names(crab_strata) == "Shape_Area"] <- "AREA_M2"

  crab_strata$STRATUM <- stratum_name

  crab_stratum <- subset(crab_strata, select = c("STRATUM", "AREA_M2", "geometry"))

  return(crab_strata)

}
