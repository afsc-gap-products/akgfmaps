#' Function to get base layers for bottom trawl and longline survey regions, bathymetry, and land
#'
#' This function retrieves layers that are commonly used for mapping and spatial analysis of AFSC bottom trawl survey and longline survey data.
#'
#' @param select.region Character vector indicating which region to retrieve. Bottom trawl survey options: ebs or bs.all, sebs or bs.south, nbs or bs.north, ecs, ebs.ecs, ai, ai.west, ai.central, ai.east, goa, goa.west, goa.east, ebs.slope, bssa1, bssa2, bssa3, bssa4, bssa5, bssa6. Longline survey options: ll.ebs, ll.bssa1, ll.bssa2, ll.bssa3, ll.bssa4, ll.bssa5, ll.ai, ll.ai.west, ll.ai.central, ll.goa, ll.goa.west, ll.goa.central, ll.goa.east
#' @param set.crs Which coordinate reference system should be used? If 'auto', Alaska Albers Equal Area (EPSG:3338) will be used.
#' @param use.survey.bathymetry Should survey bathymetry be used?
#' @param include.corners Logical. Should corner stations be returned in the survey grid? Only for the EBS.
#' @param fix.invalid.geom Should invalid geometries be corrected using st_make_valid() and st_wrap_dateline()?
#' @param split.land.at.180 Logical. If set.crs is a geographic coordinate system, should the land polygon be split at 180 degrees to prevent polygons from wrapping around the world? Default = TRUE.
#' @param high.resolution.coast Should the State of Alaska polygon be a high resolution Alaska Department of Natural Resources 1:63360 scale polygon that includes smaller islands and a more detailed coastline? The higher resolution polygon (high.resolution.coast = TRUE) takes longer to load/plot and is recommended for spatial operations performed at high resolution (e.g., masking high resolution rasters). The lower resolution polygon (high.resolution.coast = FALSE) is recommended for general mapping and visualization purposes. Default = FALSE.
#' @return A list containing sf objects land, bathymetry, survey area boundary, survey strata, survey grid (or survey stations for the longline survey), a data frame of feature labels (for the EBS shelf bottom trawl survey), coordinate reference system, plot boundaryies, axis label breaks, and graticule.
#' @import sf
#' @importFrom tools toTitleCase
#' @export
#' @examples \dontrun{
#' library(akgfmaps)
#'
#' # EBS bottom trawl survey layers in Alaska Albers Equal Area projection (EPSG:3338)
#' sebs <- get_base_layers_v3(select.region = "sebs",
#'                            set.crs = "EPSG:3338")
#'
#' ggplot() +
#'   geom_sf(data = sebs$akland) +
#'   geom_sf(data = sebs$survey.strata,
#'           fill = NA,
#'           mapping = aes(color = "Survey strata")) +
#'   geom_sf(data = sebs$survey.grid,
#'           fill = NA,
#'           mapping = aes(color = "Station grid")) +
#'   geom_sf(data = sebs$survey.area,
#'           fill = NA,
#'           mapping = aes(color = "Survey area")) +
#'   geom_sf(data = sebs$graticule, alpha = 0.3, linewidth = 0.5) +
#'   scale_x_continuous(limits = sebs$plot.boundary$x,
#'                      breaks = sebs$lon.breaks) +
#'   scale_y_continuous(limits = sebs$plot.boundary$y,
#'                      breaks = sebs$lat.breaks) +
#'   theme_bw()
#'
#' # EBS bottom trawl survey layers in NAD83 (EPSG:4269) with corner stations and high resolution
#' # coastline. High resolution coastline takes longer to load and plot but is recommended when land
#' # polygons are used for spatial analysis
#'
#' sebs_corners <- get_base_layers_v3(select.region = "sebs",
#'                                    set.crs = "EPSG:4269",
#'                                    include.corners = TRUE,
#'                                    high.resolution.coast = TRUE)
#'
#' ggplot() +
#'   geom_sf(data = sebs_corners$akland) +
#'   geom_sf(data = sebs_corners$survey.strata,
#'           fill = NA,
#'           mapping = aes(color = "Survey strata")) +
#'   geom_sf(data = sebs_corners$survey.grid,
#'           fill = NA,
#'           mapping = aes(color = "Station grid")) +
#'   geom_sf(data = sebs_corners$survey.area,
#'           fill = NA,
#'           mapping = aes(color = "Survey area")) +
#'   geom_sf(data = sebs_corners$graticule, alpha = 0.3, linewidth = 0.5) +
#'   scale_x_continuous(limits = sebs_corners$plot.boundary$x,
#'                      breaks = sebs_corners$lon.breaks) +
#'   scale_y_continuous(limits = sebs_corners$plot.boundary$y,
#'                      breaks = sebs_corners$lat.breaks) +
#'   theme_bw()
#'
#' # EBS slope, Aleutian Islands, and Gulf of Alaska surveys have subarea options
#' ai_west <- get_base_layers(select.region = "ai.west",
#'                            set.crs = "EPSG:3338")
#'
#' ggplot() +
#'   geom_sf(data = ai_west$akland) +
#'   geom_sf(data = ai_west$survey.strata,
#'           fill = NA,
#'           mapping = aes(color = "Survey strata")) +
#'   geom_sf(data = ai_west$survey.grid,
#'           fill = NA,
#'           mapping = aes(color = "Station grid")) +
#'   geom_sf(data = ai_west$survey.area,
#'           fill = NA,
#'           mapping = aes(color = "Survey area")) +
#'   geom_sf(data = ai_west$graticule, alpha = 0.3, linewidth = 0.5) +
#'   scale_x_continuous(limits = ai_west$plot.boundary$x,
#'                      breaks = ai_west$lon.breaks) +
#'   scale_y_continuous(limits = ai_west$plot.boundary$y,
#'                      breaks = ai_west$lat.breaks) +
#'   theme_bw()}

get_base_layers_v3 <- function(select.region,
                            set.crs = "EPSG:4269",
                            use.survey.bathymetry = TRUE,
                            include.corners = NULL,
                            split.land.at.180 = TRUE,
                            fix.invalid.geom = TRUE,
                            high.resolution.coast = FALSE) {

  select.region <- tolower(select.region)

  stopifnot("get_base_layers: include.corners argument must be NULL, TRUE, or FALSE." = c(is.null(include.corners) || is.logical(include.corners)))

  if(!is.null(include.corners) & !any((select.region %in% c("ebs", "bs.all", "sebs", "bs.south", "ebs.ecs")))) {
    message("get_base_layers: Ignoring include.corners since include.corners is only valid when select.region include.cornerss the SEBS")
  }

  if(is.null(include.corners) & any(select.region %in% c("ebs", "bs.all", "sebs", "bs.south", "ebs.ecs"))) {
    message("get_base_layers: Corner stations are no longer included in the default EBS shelf survey.grid design. To include.corners corner stations in the survey.grid, set include.corners = TRUE. Refer to release notes for akgfmaps version 3.5.0 for more info (https://github.com/afsc-gap-products/akgfmaps/blob/master/NEWS).")
    include.corners <- FALSE
  }

  if(select.region[1] %in% c("ebs", "bs.all", "sebs", "bs.south", "ebs.ecs")) {

    grid.file <- ifelse(include.corners, "bs_grid_w_corners.shp", "bs_grid.shp")

  }

  .check_region(select.region = select.region[1], type = "survey")

  ## Automatically set CRS
  if(set.crs == "auto") {
    message("get_base_layers: select.region = 'auto' now uses Alaska Albers Equal Area (EPSG:3338) by default. This replaces the custom equal area projection PROJ4 strings that were used prior to akgfmaps 3.6.0.")
    set.crs <- "EPSG:3338"
  }

  inpfc.strata <- NULL
  survey.grid <- NULL

  # Bathymetry and land shapefiles ---------------------------------------------------------------
  if(select.region[1] %in%
     c("bs.south", "sebs", "bs.all", "ebs", "nbs", "bs.north", "ecs", "ebs.ecs")) {

    akland <- sf::st_read(system.file("extdata", "ak_russia.shp", package = "akgfmaps"),
                          quiet = TRUE)

    akland$COUNTRY <- c("RU", "US")

    akland$STATE_PROVINCE <- c(NA, "Alaska")

    bathymetry <- sf::st_read(system.file("extdata", "npac_0-200_meters.shp", package = "akgfmaps"),
                              quiet = TRUE)

  } else if(select.region %in%
            c("ebs.slope", "bssa1", "bssa2", "bssa3", "bssa4", "bssa5", "bssa6", "ll.ebs", "ll.bssa1",
              "ll.bssa2", "ll.bssa3", "ll.bssa4", "ll.bssa5")) {

    akland <- sf::st_read(system.file("extdata", "ak_russia.shp", package = "akgfmaps"),
                          quiet = TRUE)

    bathymetry <- sf::st_read(system.file("extdata", "npac_0-1000_meters.shp", package = "akgfmaps"),
                              quiet = TRUE)

    akland$COUNTRY <- c("RU", "US")

    akland$STATE_PROVINCE <- c(NA, "Alaska")

  } else if(select.region[1] %in%
            c("ai","ai.west", "ai.central", "ai.east", "goa", "goa.west", "goa.east", "ll.ai",
              "ll.ai.west", "ll.ai.central", "ll.goa", "ll.goa.east", "ll.goa.west",
              "ll.goa.central")) {

    akland <- sf::st_read(system.file("extdata", "alaska_canada_dcw.shp", package = "akgfmaps"),
                          quiet = TRUE)

    akland <- akland[akland$POPYADMIN %in%
                       c("ALBERTA",
                         "BRITISH COLUMBIA",
                         "YUKON TERRITORY",
                         "NORTHWEST TERRITORIES",
                         "ALASKA"), ]

    akland$COUNTRY <- akland$POPYCOUN

    akland$STATE_PROVINCE <- akland$POPYADMIN |>
      tolower() |>
      tools::toTitleCase()

    bathymetry <- sf::st_read(system.file("extdata", "alaska_race.shp", package = "akgfmaps"),
                              quiet = TRUE)

  }

  # Use higher resolution coastline polygon when user sets high.resolution.coast = TRUE
  if(high.resolution.coast) {

    alaska_dnr <-
      sf::st_read(
        system.file("extdata", "Alaska_Coastline.shp", package = "akgfmaps"),
        quiet = TRUE)

    alaska_dnr$COUNTRY <- "US"

    alaska_dnr$STATE_PROVINCE <- "Alaska"

    alaska_dnr <- alaska_dnr[c("COUNTRY", "STATE_PROVINCE", "geometry")]

    akland <- akland[c("COUNTRY", "STATE_PROVINCE", "geometry")]

    akland <- akland[!(akland$STATE_PROVINCE == "Alaska"), ]

    akland <- sf::st_transform(akland,
                               crs = sf::st_crs(alaska_dnr))

    akland <- rbind(akland, alaska_dnr)

  }

  # SEBS--------------------------------------------------------------------------------------------
  if(select.region[1] %in% c("bs.south", "sebs")) {
    survey.area <- sf::st_read(system.file("extdata", "ebs_survey_boundary.shp", package = "akgfmaps"),
                               quiet = TRUE)

    survey.area <- survey.area[survey.area$SURVEY == "EBS_SHELF", ]

    survey.strata <- sf::st_read(system.file("extdata", "ebs_strata.shp", package = "akgfmaps"),
                                 quiet = TRUE)

    survey.strata <- survey.strata[survey.strata$Stratum %in% c(10, 20, 31, 32, 41, 42, 43, 50, 61, 62, 82, 90), ]

    survey.grid <- sf::st_read(system.file("extdata", grid.file, package = "akgfmaps"),
                               quiet = TRUE)

    lon.breaks <- seq(-180, -154, 5)
    lat.breaks <- seq(54, 64, 2)
  }

  # NEBS:NBS+SEBS---------------------------------------------------------------------------------------
  if(select.region[1] %in% c("bs.all", "ebs")) {
    survey.area <- sf::st_read(system.file("extdata", "ebs_survey_boundary.shp", package = "akgfmaps"),
                               quiet = TRUE)
    survey.strata <- sf::st_read(system.file("extdata", "ebs_strata.shp", package = "akgfmaps"),
                                 quiet = TRUE)
    survey.grid <- sf::st_read(system.file("extdata", grid.file, package = "akgfmaps"),
                               quiet = TRUE)

    lon.breaks <- seq(-180, -154, 5)
    lat.breaks <- seq(54, 66, 2)
  }

  # NBS --------------------------------------------------------------------------------------------
  if(select.region[1] %in% c("bs.north", "nbs")) {
    survey.area <- sf::st_read(system.file("extdata", "ebs_survey_boundary.shp", package = "akgfmaps"),
                               quiet = TRUE)

    survey.area <- survey.area[survey.area$SURVEY == "NBS_SHELF", ]

    survey.strata <- sf::st_read(system.file("extdata", "ebs_strata.shp", package = "akgfmaps"),
                                 quiet = TRUE)

    survey.strata <- survey.strata[survey.strata$Stratum %in% c(81,70,71), ]

    survey.grid <- sf::st_read(system.file("extdata", "bs_grid_w_corners.shp", package = "akgfmaps"),
                               quiet = TRUE)

    lon.breaks <- seq(-180, -154, 5)
    lat.breaks <- seq(60, 66, 2)
  }

  # EBS Slope --------------------------------------------------------------------------------------
  if(select.region[1] %in% c("ebs.slope", "bssa1", "bssa2", "bssa3", "bssa4", "bssa5", "bssa6")) {
    survey.area <- sf::st_read(system.file("extdata", "bssa_survey_boundary_2022.shp", package = "akgfmaps"),
                               quiet = TRUE)
    survey.strata <- sf::st_read(system.file("extdata", "bssa1to6_2022.shp", package = "akgfmaps"),
                                 quiet = TRUE)

    lon.breaks <- seq(-180, -155, 5)
    lat.breaks <- seq(52, 64, 2)

    if(select.region[1] %in% c("bssa1", "bssa2", "bssa3", "bssa4", "bssa5", "bssa6")) {
      strata.temp <- survey.strata
      subarea <- c(1,2,3,4,5,6)[match(select.region,  c("bssa1", "bssa2", "bssa3", "bssa4", "bssa5", "bssa6"))]

      survey.strata <- strata.temp[floor(strata.temp$STRATUM/10) %in% (subarea + c(-1,0,1)), ]

      stratum.extent <- strata.temp[floor(strata.temp$STRATUM/10) == subarea, ]

      lon.breaks <- seq(-180, -155, 1)
      lat.breaks <- seq(52, 64, 0.5)
    }


  }


  # Chukchi-----------------------------------------------------------------------------------------
  if(select.region[1] == "ecs") {
    survey.area <- sf::st_read(system.file("extdata", "chukchi_survey_boundary.shp", package = "akgfmaps"),
                               quiet = TRUE)
    survey.strata <- sf::st_read(system.file("extdata", "chukchi_strata.shp", package = "akgfmaps"),
                                 quiet = TRUE)

    lon.breaks <- seq(-180, -154, 5)
    lat.breaks <- seq(66, 76, 2)
  }

  # Chukchi+EBS ------------------------------------------------------------------------------------
  if(select.region[1] == "ebs.ecs") {
    survey.area <- sf::st_read(system.file("extdata", "ebs_chukchi_survey_boundary.shp", package = "akgfmaps"), quiet = TRUE)
    survey.strata <- sf::st_read(system.file("extdata", "ebs_chukchi_strata.shp", package = "akgfmaps"), quiet = TRUE)

    lon.breaks <- seq(-180, -150, 5)
    lat.breaks <- seq(54,78,4)
  }

  # Aleutian Islands -------------------------------------------------------------------------------
  if(select.region[1] == "ai") {
    survey.area <- sf::st_read(system.file("extdata", "ai_area.shp", package = "akgfmaps"), quiet = TRUE)
    survey.strata <- sf::st_read(system.file("extdata", "ai_strata.shp", package = "akgfmaps"), quiet = TRUE)
    survey.grid <- sf::st_read(system.file("extdata", "ai_grid.shp", package = "akgfmaps"), quiet = TRUE)

    survey.strata <- survey.strata[survey.strata$STRATUM < 800 & survey.strata$STRATUM > 0, ]
    survey.grid <- survey.grid[survey.grid$STRATUM < 800 & survey.grid$STRATUM > 0, ]

    inpfc.strata <- get_inpfc_strata(select.region = "ai", set.crs = set.crs)

    lon.breaks <- c(170, 175, -180, -175, -170, -165, -160)
    lat.breaks <- seq(44, 56, 2)
  }

  # Aleutian Islands - East ------------------------------------------------------------------------
  if(select.region[1] == "ai.east") {
    survey.area <- sf::st_read(system.file("extdata", "ai_area.shp", package = "akgfmaps"), quiet = TRUE)
    survey.strata <- sf::st_read(system.file("extdata", "ai_strata.shp", package = "akgfmaps"), quiet = TRUE)
    survey.grid <- sf::st_read(system.file("extdata", "ai_grid.shp", package = "akgfmaps"), quiet = TRUE)

    survey.strata <- survey.strata[survey.strata$STRATUM < 800 & survey.strata$STRATUM > 0, ]
    survey.grid <- survey.grid[survey.grid$STRATUM < 800 & survey.grid$STRATUM > 0, ]

    inpfc.strata <- get_inpfc_strata(select.region = "ai", set.crs = set.crs)

    lon.breaks <- seq(-176, -164, 2)
    lat.breaks <- seq(52, 55, 1)
  }

  # Aleutian Islands - Central ---------------------------------------------------------------------
  if(select.region[1] == "ai.central") {
    survey.area <- sf::st_read(system.file("extdata", "ai_area.shp", package = "akgfmaps"), quiet = TRUE)
    survey.strata <- sf::st_read(system.file("extdata", "ai_strata.shp", package = "akgfmaps"), quiet = TRUE)
    survey.grid <- sf::st_read(system.file("extdata", "ai_grid.shp", package = "akgfmaps"), quiet = TRUE)

    survey.strata <- survey.strata[survey.strata$STRATUM < 800 & survey.strata$STRATUM > 0, ]
    survey.grid <- survey.grid[survey.grid$STRATUM < 800 & survey.grid$STRATUM > 0, ]

    inpfc.strata <- get_inpfc_strata(select.region = "ai", set.crs = set.crs)

    lon.breaks <- c(176, 178, 180, seq(-178, -170, 2))
    lat.breaks <- seq(51,55,2)
  }

  # Aleutian Islands - West ---------------------------------------------------------------------
  if(select.region[1] == "ai.west") {
    survey.area <- sf::st_read(system.file("extdata", "ai_area.shp", package = "akgfmaps"), quiet = TRUE)
    survey.strata <- sf::st_read(system.file("extdata", "ai_strata.shp", package = "akgfmaps"), quiet = TRUE)
    survey.grid <- sf::st_read(system.file("extdata", "ai_grid.shp", package = "akgfmaps"), quiet = TRUE)

    survey.strata <- survey.strata[survey.strata$STRATUM < 800 & survey.strata$STRATUM > 0, ]
    survey.grid <- survey.grid[survey.grid$STRATUM < 800 & survey.grid$STRATUM > 0, ]

    inpfc.strata <- get_inpfc_strata(select.region = "ai", set.crs = set.crs)

    lon.breaks <- seq(168, 180, 2)
    lat.breaks <- seq(50,55,2)
  }


  # Gulf of Alaska ---------------------------------------------------------------------------------
  if(select.region[1] == "goa") {
    survey.area <- sf::st_read(system.file("extdata", "goa_area.shp", package = "akgfmaps"), quiet = TRUE)
    survey.strata <- sf::st_read(system.file("extdata", "goa_strata.shp", package = "akgfmaps"), quiet = TRUE)
    survey.grid <- sf::st_read(system.file("extdata", "goa_grid.shp", package = "akgfmaps"), quiet = TRUE)

    survey.strata <- survey.strata[survey.strata$STRATUM > 0, ]
    survey.grid <- survey.grid[survey.grid$STRATUM > 0, ]

    inpfc.strata <- get_inpfc_strata(select.region = "goa", set.crs = set.crs)

    lon.breaks <- seq(-175, -130, 5)
    lat.breaks <- seq(52,64,2)
  }

  # Gulf of Alaska - West --------------------------------------------------------------------------
  if(select.region[1] == "goa.west") {
    survey.area <- sf::st_read(system.file("extdata", "goa_area.shp", package = "akgfmaps"), quiet = TRUE)
    survey.strata <- sf::st_read(system.file("extdata", "goa_strata.shp", package = "akgfmaps"), quiet = TRUE)
    survey.grid <- sf::st_read(system.file("extdata", "goa_grid.shp", package = "akgfmaps"), quiet = TRUE)

    survey.strata <- survey.strata[survey.strata$STRATUM > 0, ]
    survey.grid <- survey.grid[survey.grid$STRATUM > 0, ]

    inpfc.strata <- get_inpfc_strata(select.region = "goa", set.crs = set.crs)

    lon.breaks <- seq(-174, -144, 2)
    lat.breaks <- seq(52, 64, 2)
  }

  # Gulf of Alaska - East --------------------------------------------------------------------------
  if(select.region[1] == "goa.east") {
    survey.area <- sf::st_read(system.file("extdata", "goa_area.shp", package = "akgfmaps"), quiet = TRUE)
    survey.strata <- sf::st_read(system.file("extdata", "goa_strata.shp", package = "akgfmaps"), quiet = TRUE)
    survey.grid <- sf::st_read(system.file("extdata", "goa_grid.shp", package = "akgfmaps"), quiet = TRUE)

    survey.strata <- survey.strata[survey.strata$STRATUM > 0, ]
    survey.grid <- survey.grid[survey.grid$STRATUM > 0, ]

    inpfc.strata <- get_inpfc_strata(select.region = "goa", set.crs = set.crs)

    lon.breaks <- seq(-160, -124, 2)
    lat.breaks <- seq(52, 64, 2)
  }

  # Longline EBS ------------------------------------------------------------------------------------
  if(select.region[1] %in% c("ll.ebs", "ll.bssa1", "ll.bssa2", "ll.bssa3", "ll.bssa4", "ll.bssa5")) {

    survey.strata <- sf::st_read(
      system.file("extdata", "longline_survey", "LL_survey_Slope_and_Gullies.shp", package = "akgfmaps"),
      quiet = TRUE)

    survey.strata <- survey.strata[survey.strata$REGION == "EBS" & survey.strata$TYPE == "Slope", ]

    survey.grid <- sf::st_read(
      system.file("extdata", "longline_survey", "LL_Survey_Stations_Active2023.shp",
                  package = "akgfmaps"),
      quiet = TRUE)

    survey.grid <- survey.grid[survey.grid$Region == "Eastern Bering Sea", ]

    lon.breaks <- seq(-180, -155, 5)
    lat.breaks <- seq(52, 64, 2)

    if(select.region[1] %in% c("ll.bssa1", "ll.bssa2", "ll.bssa3", "ll.bssa4", "ll.bssa5")) {

      subarea <- c("I", "II", "III", "IV", "V")[match(
        select.region,
        c("ll.bssa1", "ll.bssa2", "ll.bssa3", "ll.bssa4", "ll.bssa5")
      )]
      subarea <- paste0("Bering ", subarea)

      stratum.extent <- survey.strata[survey.strata$AREA_DESC == subarea, ]

      lon.breaks <- seq(-180, -155, 1)
      lat.breaks <- seq(52, 64, 0.5)
    }


  }

  # Longline Aleutian Islands ----------------------------------------------------------------------
  if(select.region[1] == "ll.ai") {
    survey.strata <- sf::st_read(
      system.file("extdata", "longline_survey", "LL_survey_Slope_and_Gullies.shp", package = "akgfmaps"),
      quiet = TRUE)

    survey.strata <- survey.strata[survey.strata$REGION == "AI" & survey.strata$TYPE == "Slope", ]

    survey.grid <- sf::st_read(
      system.file("extdata", "longline_survey", "LL_Survey_Stations_Active2023.shp",
                  package = "akgfmaps"),
      quiet = TRUE)

    survey.grid <- survey.grid[survey.grid$Region == "Aleutian Islands", ]

    lon.breaks <- c(170, 175, -180, -175, -170, -165, -160)
    lat.breaks <- seq(44, 56, 2)
  }

  # Longline Aleutian Islands subareas -------------------------------------------------------------
  if(select.region[1] %in% c("ll.ai.west", "ll.ai.central")) {
    survey.strata <- sf::st_read(
      system.file("extdata", "longline_survey", "LL_survey_Slope_and_Gullies.shp", package = "akgfmaps"),
      quiet = TRUE)

    survey.strata <- survey.strata[survey.strata$REGION == "AI" & survey.strata$TYPE == "Slope", ]

    area.desc <- switch(select.region,
                        "ll.ai.central" = c("NE Aleutians", "NW Aleutians"),
                        "ll.ai.west" = c("SE Aleutians", "SW Aleutians")
    )

    stratum.extent <- survey.strata[survey.strata$AREA_DESC %in% area.desc, ]

    survey.grid <- sf::st_read(
      system.file("extdata", "longline_survey", "LL_Survey_Stations_Active2023.shp",
                  package = "akgfmaps"),
      quiet = TRUE)

    survey.grid <- survey.grid[survey.grid$Region == "Aleutian Islands", ]

    lon.breaks <- switch(select.region,
                         "ll.ai.central" = c(seq(170, 178, 2), seq(-178, -170, 2)),
                         "ll.ai.west" = c(seq(168, 180, 2), seq(-178, -170, 2)),
    )
    lat.breaks <- seq(44, 56, 2)
  }

  # Longline Gulf of Alaska ------------------------------------------------------------------------
  if(select.region[1] %in% "ll.goa") {
    survey.strata <- sf::st_read(
      system.file("extdata", "longline_survey", "LL_survey_Slope_and_Gullies.shp", package = "akgfmaps"),
      quiet = TRUE)

    survey.strata <- survey.strata[survey.strata$REGION == "GOA" & survey.strata$TYPE == "Slope", ]

    survey.grid <- sf::st_read(
      system.file("extdata", "longline_survey", "LL_Survey_Stations_Active2023.shp",
                  package = "akgfmaps"),
      quiet = TRUE)

    survey.grid <- survey.grid[survey.grid$Region == "Gulf of Alaska", ]

    lon.breaks <- seq(-175, -130, 5)
    lat.breaks <- seq(52,64,2)
  }

  # Longline Gulf of Alaska subareas ---------------------------------------------------------------
  if(select.region[1] %in% c("ll.goa.east", "ll.goa.central", "ll.goa.west")) {
    survey.strata <- sf::st_read(
      system.file("extdata", "longline_survey", "LL_survey_Slope_and_Gullies.shp", package = "akgfmaps"),
      quiet = TRUE)

    survey.strata <- survey.strata[survey.strata$REGION == "GOA" & survey.strata$TYPE == "Slope", ]

    area.desc <- switch(select.region,
                        "ll.goa.east" = c("East Yakutat", "Southeast", "West Yakutat"),
                        "ll.goa.central" = c("Kodiak", "Chirikof"),
                        "ll.goa.west" = "Shumagin"
    )

    stratum.extent <- survey.strata[survey.strata$AREA_DESC %in% area.desc, ]

    survey.grid <- sf::st_read(
      system.file("extdata", "longline_survey", "LL_Survey_Stations_Active2023.shp",
                  package = "akgfmaps"),
      quiet = TRUE)

    survey.grid <- survey.grid[survey.grid$Region == "Gulf of Alaska", ]

    lon.breaks <- seq(-175, -130, 2)
    lat.breaks <- seq(52, 64, 2)
  }

  # Format longline survey columns
  if(grepl(pattern = "ll.", x = select.region[1])) {

    names(survey.grid)[
      match(
        c("Station_Nu",
          "Region",
          "Station_Ty",
          "Station_De",
          "Geographic",
          "Geograph_1",
          "NPFMC_Mana",
          "NPFMC_Sabl",
          "NMFS_Manag",
          "FMP_Manage",
          "INPFC_Mana",
          "Habitat_Ty",
          "LAT",
          "LONG",
          "Exploitabl",
          "Active"),
        names(survey.grid)
      )
    ] <- c("STATION_NUMBER",
           "REGION",
           "STATION_TYPE",
           "STATION_DEPTH",
           "GEOGRAPHIC_AREA",
           "GEOGRAPHIC_AREA_NUMBER",
           "NPFMC_MANAGEMENT_AREA",
           "NPFMC_SABLEFISH_AREA",
           "REP_AREA",
           "FMP_MANAGEMENT_AREA",
           "INPFC_MANAGEMENT_AREA",
           "HABITAT_TYPE",
           "LAT",
           "LONG",
           "EXPLOITABLE",
           "ACTIVE")

    survey.area <- survey.strata[, "geometry"] |>
      sf::st_union(by_feature = FALSE) |>
      sf::st_as_sf()

    sf::st_geometry(survey.area) <- "geometry"

    survey.area$REGION <- c(rep("EBS", 6), "AI", rep("GOA", 4))[
      match(select.region,
            c("ll.bssa1",
              "ll.bssa2",
              "ll.bssa3",
              "ll.bssa4",
              "ll.bssa5",
              "ll.ebs",
              "ll.ai",
              "ll.goa",
              "ll.goa.west",
              "ll.goa.central",
              "ll.goa.east"),
      )
    ]

    names(survey.strata)[names(survey.strata) == "F_AREA"] <- "AREA_M2"

  }

  # Set CRS-----------------------------------------------------------------------------------------
  if(tolower(class(set.crs)) != "crs") {
    set.crs <- sf::st_crs(set.crs)
  }

  # Use survey bathymetry --------------------------------------------------------------------------
  if(use.survey.bathymetry) {
    if(select.region[1] %in% c("ai", "ai.west", "ai.central", "ai.east")) {
      bathymetry <- bathymetry[bathymetry$METERS %in% c(100, 300, 500, 700), ]
    } else if(select.region[1] %in% c("goa", "goa.west", "goa.east")) {
      bathymetry <- bathymetry[bathymetry$METERS %in% c(100, 200, 700), ]
    }
  }

  # Make graticule ---------------------------------------------------------------------------------
  graticule <- sf::st_graticule(lat = lat.breaks,
                                lon = lon.breaks,
                                margin = 1e-5)

  # Set CRS for layers -----------------------------------------------------------------------------
  akland <- sf::st_transform(akland, crs = set.crs)
  survey.area <- sf::st_transform(survey.area, crs = set.crs)
  survey.strata <- sf::st_transform(survey.strata, crs = set.crs)
  bathymetry <- sf::st_transform(bathymetry, crs = set.crs)

  if(exists("stratum.extent")) {
    stratum.extent <- sf::st_transform(stratum.extent, crs = set.crs)
  }


  # Set up survey grid -----------------------------------------------------------------------------
  if(!is.null(survey.grid)) {

    if(select.region[1] %in% c("bs.all", "ebs", "bs.south", "sebs", "bs.north", "nbs")) {
      survey.grid$STATIONID[survey.grid$STATIONID == "Z-04"] <- "AZ0504" # Divided station in SEBS

      survey.grid <- survey.grid[survey.grid$STATIONID %in% akgfmaps::get_survey_stations(select.region = select.region[1]), ]
    }

    survey.grid <- sf::st_transform(survey.grid, crs = set.crs)

    # EBS survey grid clipping ---------------------------------------------------------------------
    if(select.region %in% c("bs.all", "ebs", "bs.south", "sebs", "bs.north", "nbs")) {

      grid.intersects <- try(survey.area |>
                               sf::st_union() |>
                               sf::st_intersects(survey.grid), silent = TRUE)

      if("try-error" %in% class(grid.intersects)) {
        sf::sf_use_s2(FALSE)
        grid.intersects <- try(survey.area |>
                                 sf::st_union() |>
                                 sf::st_intersects(survey.grid), silent = TRUE)
      }

      if("try-error" %in% class(grid.intersects)) {
        warning("get_base_layers: Can't mask survey grid using sf.")
      } else {
        survey.grid <- survey.grid[grid.intersects[[1]],]
        survey.mask <- survey.area |> sf::st_union()
        survey.grid <- sf::st_intersection(survey.grid, survey.mask)

        survey.grid <- survey.grid[survey.grid$STATIONID %in% akgfmaps::get_survey_stations(select.region = select.region[1]), ]
      }
    }
  }

  # Set plot boundary ------------------------------------------------------------------------------
  if(select.region %in% c("ai.east", "ai.west", "ai.central", "goa.west", "goa.east")) {

    lat.lon.grid <- sf::st_transform(survey.grid, crs = "EPSG:4269")

    if(select.region[1] == "ai.east") {
      grid_index <- which(sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] > -173.5 &
                            sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] < 0)
    }

    if(select.region[1] == "ai.central") {
      grid_index <- which((sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] <= -173.5 |
                             (sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] > 178.5)) &
                            sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,2] < 53.25)
    }

    if(select.region[1] == "ai.west") {
      grid_index <- which(sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] > 0 &
                            sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] <= 178.5)
    }

    if(select.region[1] == "goa.west") {
      grid_index <- which(sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] < -150)
    }

    if(select.region[1] == "goa.east") {
      grid_index <- which(sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] >= -150)
    }

    sub.area.cells <- lat.lon.grid[grid_index, ]

    sub.area.cells <- sf::st_transform(sub.area.cells, crs = set.crs)

    plot.boundary <- sf::st_bbox(sub.area.cells)
    plot.boundary <- data.frame(x = c(plot.boundary['xmin'], plot.boundary['xmax']),
                                y = c(plot.boundary['ymin'], plot.boundary['ymax']))


  } else if(select.region[1] %in% c("bssa1", "bssa2", "bssa3", "bssa4", "bssa5", "bssa6",
                                    "ll.bssa1", "ll.bssa2", "ll.bssa3", "ll.bssa4", "ll.bssa5",
                                    "ll.goa.west", "ll.goa.central", "ll.goa.east", "ll.ai.central",
                                    "ll.ai.west")) {
    plot.boundary <- sf::st_bbox(stratum.extent)
    plot.boundary <- data.frame(x = c(plot.boundary['xmin'], plot.boundary['xmax']),
                                y = c(plot.boundary['ymin'], plot.boundary['ymax']))

  } else {
    plot.boundary <- sf::st_bbox(survey.area)
    plot.boundary <- data.frame(x = c(plot.boundary['xmin'], plot.boundary['xmax']),
                                y = c(plot.boundary['ymin'], plot.boundary['ymax']))
  }

  # Get place labels--------------------------------------------------------------------------------
  place.labels <- utils::read.csv(file = system.file("extdata", "placenames.csv", package = "akgfmaps"))

  place.labels <- place.labels[place.labels$region == select.region[1], ]
  place.labels <- akgfmaps::transform_data_frame_crs(place.labels, out.crs = set.crs)


  # Attempt to correct any remaining degenerate geometry and dateline wrapping issues --------------

  if(fix.invalid.geom) {
    akland <- fix_geometry(x = akland)
    survey.area <- fix_geometry(x = survey.area)
    survey.grid <- fix_geometry(x = survey.grid)
    survey.strata <- fix_geometry(x = survey.strata)
    bathymetry <- fix_geometry(x = bathymetry)
    inpfc.strata <- fix_geometry(x = inpfc.strata)
  }

  # Split land polygons to fix dateline wrapping for lat/lon  --------------------------------------
  if(sf::st_is_longlat(akland) & split.land.at.180) {

    west <- sf::st_crop(akland, xmin = -179.9995, xmax = -90, ymin = 0, ymax = 90)

    east <- sf::st_crop(akland, xmin = 90, xmax = 179.9995, ymin = 0, ymax = 90)

    akland <- rbind(west, east)

    akland <- akland[which(sf::st_geometry_type(akland$geometry) %in% c("POLYGON", "MULTIPOLYGON")), ]

  }

  return(list(region = select.region,
              akland = akland,
              survey.area = survey.area,
              survey.strata = survey.strata,
              survey.grid = survey.grid,
              bathymetry = bathymetry,
              place.labels = place.labels,
              graticule = graticule,
              crs = set.crs,
              plot.boundary = plot.boundary,
              lon.breaks = lon.breaks,
              lat.breaks = lat.breaks,
              inpfc.strata = inpfc.strata))
}
