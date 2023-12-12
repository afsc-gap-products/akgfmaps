#' Save regional shapefiles to a zip file
#'
#' Write regional shapefiles returned by get_base_layers() and a README file with version information to a zip file.
#'
#' @param select.region Character vector indicating which region. Options = ebs or bs.all, sebs or bs.south, nbs or bs.north, ecs, ebs.ecs, ai, ai.west, ai.central, ai.east, goa, goa.west, goa.east, ebs.slope, bssa1, bssa2, bssa3, bssa4, bssa5, bssa6
#' @param set.crs Which coordinate reference system should be used? If 'auto', an Albers Equal Area coordinate reference system is automatically assigned.
#' @param zip.path Path to the zip file where shapefiles should be written.
#' @importFrom utils zip
#' @export
#' @import sf here
#' @examples
#' generate_region_zip(select.region = "ai.west", set.crs = "WGS84")

generate_region_zip <- function(select.region, set.crs = "EPSG:4326", zip.path = NULL) {

  zip_path <- zip.path

  survey_name <- data.frame(
    survey = c("eastern Bering Sea continental shelf summer bottom trawl survey",
               "eastern Bering Sea continental shelf summer bottom trawl survey",
               "eastern and northern Bering Sea continental shelf summer bottom trawl surveys",
               "eastern and northern Bering Sea continental shelf summer bottom trawl surveys",
               "northern Bering Sea shelf summer bottom trawl survey",
               "northern Bering Sea shelf summer bottom trawl survey",
               "Chukchi Sea summer bottom trawl survey",
               "eastern and northern Bering Sea continental shelf and eastern Chukchi Sea bottom trawl surveys",
               "Aleutian Islands summer bottom trawl survey",
               "Aleutian Islands summer bottom trawl survey (western)",
               "Aleutian Islands summer bottom trawl survey (central)",
               "Aleutian Islands summer bottom trawl survey (eastern)",
               "Gulf of Alaska summer bottom trawl survey",
               "Gulf of Alaska summer bottom trawl survey (western)",
               "Gulf of Alaska summer bottom trawl survey (eastern)",
               "eastern Bering Sea slope summer bottom trawl survey",
               "eastern Bering Sea slope summer bottom trawl survey (subarea 1)",
               "eastern Bering Sea slope summer bottom trawl survey (subarea 2)",
               "eastern Bering Sea slope summer bottom trawl survey (subarea 3)",
               "eastern Bering Sea slope summer bottom trawl survey (subarea 4)",
               "eastern Bering Sea slope summer bottom trawl survey (subarea 5)",
               "eastern Bering Sea slope summer bottom trawl survey (subarea 6)"),
    select.region = c("bs.south", "sebs", "bs.all", "ebs", "bs.north", "nbs", "ecs", "ebs.ecs",
                      "ai", "ai.west", "ai.central", "ai.east", "goa", "goa.west", "goa.east",
                      "ebs.slope", "bssa1", "bssa2", "bssa3", "bssa4", "bssa5", "bssa6")
  )

  survey_title <- survey_name$survey[survey_name$select.region == select.region]

  on_fail <- function(x, fail_file, tmp_path) {

    if(any(class(x) == "try-error")) {

      unlink(tmp_path, recursive = TRUE)

      stop("generate_region_zip: Failed to write ", fail_file)

    }

  }

  tmp_path <- here::here(paste0("tmp", floor(as.numeric(Sys.time()))))

  dir.create(path = tmp_path)

  if(is.null(zip_path)) {
    zip_path <- paste0("akgfmaps_",
                       gsub(x = select.region, pattern = "\\.", replacement = ""), "_",
                       gsub(x = Sys.Date(), pattern = "-", replacement = ""),
                       ".zip")
  } else {

    stopifnot("generate_region_zip: zip_path must have the filetype extension .zip" = tolower(substr(x = zip_path, start = nchar(zip_path) - 2, stop = nchar(zip_path))) == "zip")

  }


  map_layers <- akgfmaps::get_base_layers(select.region = select.region, set.crs = set.crs)

  layer_names <- c("akland", "survey.area", "survey.strata", "survey.grid", "bathymetry", "graticule")

  keep_names <- layer_names[which(layer_names %in% names(map_layers))]

  tmp_files <- character()


  for(ii in 1:length(layer_names)) {

    tmp_files <- c(tmp_files,
                   here::here(tmp_path,
                              paste0(
                                gsub(x = layer_names[ii],
                                     pattern = "\\.",
                                     replacement = "_"), ".shp")
                   ))

    if(!("sf" %in% class(map_layers[[layer_names[ii]]]))) {
      next
    }

    try_write <- try(sf::st_write(obj = map_layers[[layer_names[ii]]], tmp_files[ii]), silent = TRUE)

    on_fail(x = try_write, fail_file = tmp_files[ii], tmp_path = tmp_path)


  }

  pkg_info <- sessionInfo(package = "akgfmaps")

  readme_lines <- c(paste0("Dataset name: Shapefiles for the NOAA/AFSC ", survey_title),
                    "Created by: NOAA Fisheries Alaska Fisheries Science Center, Resource Assessment and Conservation Engineering Division, Groundfish Assessment Program",
                    paste0("Description: Created on ", Sys.Date(), " using akgfmaps version ", pkg_info$otherPkgs$akgfmaps$Version),
                    "Website: https://github.com/afsc-gap-products/akgfmaps",
                    paste0("Coordinate Reference System: " , set.crs)
  )

  readme_path <- here::here(tmp_path, "0_README.txt")

  try_write <- try(writeLines(text = readme_lines, con = readme_path), silent = TRUE)

  on_fail(x = try_write, fail_file = tmp_files[ii], tmp_path = tmp_path)

  if(file.exists(zip_path)) {
    warning("generate_region_zip: removing existing .zip file from ", zip_path)
  }

  base_path <- getwd()

  setwd(tmp_path)

  try_copy <- try(zip(zipfile = zip_path, files = list.files(getwd())), silent = TRUE)

  on_fail(x = try_copy, fail_file = tmp_files[ii], tmp_path = tmp_path)

  setwd(base_path)

  print(here::here(tmp_path, zip_path))
  print(here::here(zip_path))
  file.rename(from = here::here(tmp_path, zip_path), to = here::here(zip_path))

  unlink(tmp_path, recursive = TRUE)

  message("generate_region_zip: ", length(tmp_files), " shapefiles saved to ", zip_path)

}
