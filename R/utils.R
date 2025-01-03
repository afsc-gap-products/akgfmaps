#' Check that the region is valid (internal)
#'
#' Internal function to check that region is valid.
#'
#' @param select.region Region (1L character vector).
#' @param type One of 'inpfc', 'nmfs', 'all', or 'survey'
#' @noRd

.check_region <- function(select.region, type = "all") {

  survey <- c(
    "bs.south", "sebs", "bs.all", "ebs", "bs.north", "nbs", "ecs", "ebs.ecs", "ai", "ai.west",
    "ai.central", "ai.east", "goa", "goa.west", "goa.east", "ebs.slope", "bssa1", "bssa2",
    "bssa3", "bssa4", "bssa5", "bssa6", "ll.ebs", "ll.bssa1", "ll.bssa2", "ll.bssa3", "ll.bssa4",
    "ll.bssa5", "ll.goa", "ll.goa.west", "ll.goa.central", "ll.goa.east", "ll.ai", "ll.ai.west",
    "ll.ai.central")

  inpfc <- c("ai", "goa")

  nmfs <- "nmfs"

  if(type == "all") {
    valid_regions <- c(survey, inpfc, nmfs)
  }

  if(type == "survey") {
    valid_regions <- survey
  }

  if(type == "inpfc") {
    valid_regions <- inpfc
  }

  if(type == "nmfs") {
    valid_regions <- "nmfs"
  }

  flag <- !any(select.region %in% valid_regions)

  if(flag) {
    stop(
      paste0("Error: Invalid region selection ('", select.region, "'). Valid options: ", paste(valid_regions, collapse = ", "))
    )
  }

}


#' Handle degenerate geometry issues
#'
#' This function applies sf::st_wrap_dateline and sf::st_make_valid to geometries to correct
#' degenerate geometries.
#'
#' @param x sf object
#' @noRd

fix_geometry <- function(x) {

  if(is.null(x)) {
    return(x)
  }

  stopifnot("fix_geometry: x must be a simple features class." = is(x, "sf"))

  x <- suppressWarnings(sf::st_make_valid(x))

  if(sf::st_is_longlat(x)) {

    x <- suppressWarnings(sf::st_wrap_dateline(x))

  }

  return(x)

}

#' Set decimal degree interval
#'
#' Set decimal degree interval based on the difference between minimum and maximum values
#'
#' @param deg_dif Difference in degrees between minimum and maximum coordinates as a positive number
#' @noRd

set_dd_interval <- function(deg_diff) {

  interval <- 0.5

  if(deg_diff > 2) {
    interval <- 1
  }

  if(deg_diff > 8) {
    interval <- 2
  }

  if(deg_diff > 12) {
    interval <- 4
  }

  if(deg_diff > 20) {
    interval <- 5
  }

  if(deg_diff > 50) {
    interval <- 10
  }

  return(interval)
}


#' Filter survey layer based on design year
#'
#' Retrieve the matching or closest available design.year to the specified year from an sf or data.frame with a column named DESIGN_YEAR.
#'
#' @param x An sf object or data.frame
#' @param design.year Design year as a 1L numeric.
#' @param layer.type Optional character vector indicating the name of the input layer.
#' @examples \dontrun{
#' loc <- data.frame(DESIGN_YEAR = c(1987, 2010, 1984, 2025),
#'                   SURVEY_DEFINITION_ID = c(98, 98, 47, 47),
#'                   x = 1,
#'                   y = 1) |>
#'   sf::st_as_sf(coords = c("x", "y"), crs = "WGS84")
#'
#' # Case where design.year is specified
#' select_design_year(x = loc,
#'                    design.year = 2010,
#'                    layer.type = "survey.grid")
#'
#' # Return the most recent design when design.year is NULL
#' select_design_year(x = loc,
#'                    design.year = NULL,
#'                    layer.type = "survey.grid")
#' }
#' @noRd

select_design_year <- function(x, design.year = NULL, layer.type = NA) {

  stopifnot("select_design_year: No DESIGN_YEAR column in x" = "DESIGN_YEAR" %in% names(x))

  if(nrow(x) < 1) {

    warning("select_design_year: No geometries in x.")

    return(NULL)
  }

  survey_definition_id <- unique(x[['SURVEY_DEFINITION_ID']])

  output <- NULL

  design_year_null <- is.null(design.year)

  for(ii in 1:length(survey_definition_id)) {

    x_sel <- x[x[['SURVEY_DEFINITION_ID']] == survey_definition_id[ii], ]

    # Case where design.year is NULL returns the most recent DESIGN_YEAR
    if(design_year_null) {
      design.year <- max(x_sel[['DESIGN_YEAR']])
    }

    if(design.year %in% x_sel[['DESIGN_YEAR']]) {

      # Case where design.year exactly matches a design.year for a layer returns the specified year
      sel_design_year <- x_sel[x_sel[['DESIGN_YEAR']] == design.year, ]

    } else if(any(x_sel[['DESIGN_YEAR']] < design.year)) {

      # Case where there isn't an exact match, but there is a DESIGN_YEAR earlier than design.year
      new_design_year <- x_sel[['DESIGN_YEAR']][x_sel[['DESIGN_YEAR']] < design.year]

      new_design_year <- max(new_design_year)

      sel_design_year <- x_sel[x_sel[['DESIGN_YEAR']] == new_design_year, ]

      warning("get_base_layers: Chosen design.year (", design.year, ") does not exist for ",
              layer.type,
              ". Returning ", layer.type, " with design.year ", new_design_year, ".")

    } else {

      # Case where there isn't a year available before the selected year causes and error
      earliest_year <- min(x_sel[['DESIGN_YEAR']])

      stop("get_base_layers: Cannot retrieve design.year ", design.year, " for ", layer.type,
           ". The earliest available design.year is ", earliest_year, ".")
    }

    output <- rbind(output, sel_design_year)

  }

  return(output)

}
