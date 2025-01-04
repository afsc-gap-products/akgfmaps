#' INPFC strata for the GOA or AI
#'
#' Create INPFC strata sf POLYGONs by dissolving GOA or AI bottom trawl survey strata POLYGONs based on numerical stratum codes.
#'
#' @param select.region Region for INPFC strata. Either GOA or AI.
#' @param set.crs Which coordinate reference system should be used? If 'auto', an Albers Equal Area coordinate reference system is automatically assigned.
#' @param use.v3 Should stratum geometries from akgfmaps v3 be used? Default is FALSE.
#' @param design.year Survey design year for files to retrieve. Returns the most recent year when NULL or closest year before the design year when there isn't an exact match.
#' @return INPFC strata as an sf POLYGON.
#' @export
#' @examples \dontrun{
#' library(akgfmaps)
#'
#' # INPFC strata for the Aleutian Islands (within AI bottom trawl survey extent)
#' inpfc_ai <- get_inpfc_strata(select.region = "ai", set.crs = "EPSG:3338")
#'
#' ggplot() +
#'   geom_sf(data = inpfc_ai,
#'           mapping = aes(fill = INPFC_STRATUM))
#'
#' # INPFC strata for the Gulf of Alaska (within GOA bottom trawl survey extent)
#' inpfc_goa <- get_inpfc_strata(select.region = "goa", set.crs = "EPSG:3338")
#'
#' ggplot() +
#'   geom_sf(data = inpfc_goa,
#'           mapping = aes(fill = INPFC_STRATUM))}

get_inpfc_strata <- function(select.region, set.crs, use.v3 = FALSE, design.year = NULL) {

  if(set.crs == "auto") {
    set.crs = "EPSG:3338"
  }

  select.region <- tolower(select.region)

  .check_region(select.region = select.region, type = "survey")

  # akgfmaps v3 code-- to remove in December 2025
  if(use.v3) {
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
    strata <- strata["INPFC_STRATUM"]

    strata <- aggregate(strata["geometry"],
                        by = list(INPFC_STRATUM = strata$INPFC_STRATUM),
                        FUN = function(x) x[1],
                        do_union = TRUE)

    stata <- sf::st_transform(strata, crs = set.crs) |>
      fix_geometry()

    return(strata)

  }

  survey_definition_id <- NULL

  if(any(select.region %in%
         c("goa", "ai", "goa.west", "goa.east", "ai.west", "ai.central", "ai.east"))
     ) {

    for(ii in 1:length(select.region)) {
      survey_definition_id <- c(
        survey_definition_id,
        switch(select.region[ii],
               "bs.south" = 98,
               "sebs" = 98,
               "bs.all" = c(98, 143),
               "ebs" = c(98, 143),
               "nbs" = 143,
               "bs.north" = 143,
               "ecs" = 6,
               "ai" = 52,
               "goa" = 47,
               "ai.west" = 52,
               "ai.central" = 52,
               "ai.east" = 52,
               "goa" = 47,
               "goa.west" = 47,
               "goa.east" = 47,
               "ebs.slope" = 78,
               "bssa1" = 78,
               "bssa2" = 78,
               "bssa3" = 78,
               "bssa4" = 78,
               "bssa5" = 78,
               "bssa6" = 78)
      )
    }

  }

  strata <- sf::st_read(
    system.file("extdata", "afsc_bottom_trawl_surveys.gpkg", package = "akgfmaps"),
    query = paste0("SELECT SURVEY_DEFINITION_ID, AREA_NAME,
                      DESIGN_YEAR, AREA_ID, AREA_M2, GEOM AS geometry
                     FROM INPFC_STRATA WHERE SURVEY_DEFINITION_ID IN (",
                   paste(survey_definition_id, collapse = ", "), ")"
    ),
    quiet = TRUE
  ) |>
    select_design_year(
      design.year = design.year,
      layer.type = "inpfc.strata"
    )

  return(strata)

}
