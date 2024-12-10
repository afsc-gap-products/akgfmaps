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
