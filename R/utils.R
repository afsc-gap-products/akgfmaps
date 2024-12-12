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

  flag <- !any(select.region == valid_regions)

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
