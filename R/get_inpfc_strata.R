#' INPFC strata for the GOA or AI
#'
#' Create INPFC strata sf POLYGONs by dissolving GOA or AI bottom trawl survey strata POLYGONs based on numerical stratum codes.
#'
#' @param select.region Region for INPFC strata. Either GOA or AI.
#' @param set.crs Which coordinate reference system should be used? If 'auto', an Albers Equal Area coordinate reference system is automatically assigned.
#' @return INPFC strata as an sf POLYGON.
#' @export

get_inpfc_strata <- function(select.region, set.crs) {

  select.region <- tolower(select.region)

  .check_region(select.region = select.region, type = "inpfc")

  if(select.region %in% c("goa", "inpfc.goa")) {
    path <- "goa_strata.shp"
    stratum_names <- c("Shumagin", "Chirikof", "Kodiak", "Yakutat", "Southeastern")
    stratum_index <- 2
    offset <- 0

  } else if(select.region %in% c("ai", "inpfc.ai")) {
    path <- "ai_strata.shp"
    stratum_names <- c("Western Aleutians", "Central Aleutians", "Central Aleutians",
                       "Eastern Aleutians", "Eastern Aleutians", "Southern Bering Sea", "Southern Bering Sea")
    stratum_index <- 1
    offset <- 1
  }

  strata <- sf::st_read(
    system.file("extdata",
                path,
                package = "akgfmaps"),
    quiet = TRUE)

  strata <- strata[strata$STRATUM != 0, ] # Remove land

  # Assign strata based on numerical stratum codes
  strata$STRATUM <- formatC(strata$STRATUM, width = 3, flag = 0)
  strata$INPFC_STRATUM <- as.integer(substr(strata$STRATUM, start = stratum_index, stop = stratum_index))
  strata$INPFC_STRATUM <- stratum_names[strata$INPFC_STRATUM-offset]

  # Combine strata
  strata <- strata |>
    dplyr::select(INPFC_STRATUM) |>
    dplyr::group_by(INPFC_STRATUM) |>
    dplyr::summarise() |>
    dplyr::ungroup()

  stata <- sf::st_transform(strata, crs = set.crs) |>
    wrap_dateline_silent()

  return(strata)

}
