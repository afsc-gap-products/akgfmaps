#' INPFC strata for the GOA or AI
#'
#' Create INPFC strata sf POLYGONs by dissolving GOA or AI bottom trawl survey strata POLYGONs based on numerical stratum codes.
#'
#' @param select.region Region for INPFC strata. Either GOA or AI.
#' @param set.crs Which coordinate reference system should be used? If 'auto', an Albers Equal Area coordinate reference system is automatically assigned.
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

get_inpfc_strata <- function(select.region, set.crs, design.year = NULL) {

  if(set.crs == "auto") {
    set.crs = "EPSG:3338"
  }

  select.region <- tolower(select.region)

  .check_region(select.region = select.region, type = "survey")

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
    system.file("extdata", "afsc_bts_strata.gpkg", package = "akgfmaps"),
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
