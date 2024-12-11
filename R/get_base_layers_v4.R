#' Function to get base layers for bottom trawl and longline survey regions, bathymetry, and land
#'
#' This function retrieves layers that are commonly used for mapping and spatial analysis of AFSC bottom trawl survey and longline survey data.
#'
#' @param select.region Character vector indicating which region to retrieve. Bottom trawl survey options: ebs or bs.all, sebs or bs.south, nbs or bs.north, ecs, ai, ai.west, ai.central, ai.east, goa, goa.west, goa.east, ebs.slope, bssa1, bssa2, bssa3, bssa4, bssa5, bssa6. Longline survey options: ll.ebs, ll.bssa1, ll.bssa2, ll.bssa3, ll.bssa4, ll.bssa5, ll.ai, ll.ai.west, ll.ai.central, ll.goa, ll.goa.west, ll.goa.central, ll.goa.east
#' @param design.year Survey design year for files to retrieve. Returns the most recent year when NULL.
#' @param set.crs Which coordinate reference system should be used? If 'auto', Alaska Albers Equal Area (EPSG:3338) is used.
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
#' sebs <- get_base_layers(select.region = "sebs",
#'                         set.crs = "EPSG:3338")
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
#' sebs_corners <- get_base_layers(select.region = "sebs",
#'                                 set.crs = "EPSG:4269",
#'                                 include.corners = TRUE,
#'                                 high.resolution.coast = TRUE)
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

get_base_layers_v4 <- function(select.region,
                               design.year = NULL,
                               set.crs = "EPSG:4269",
                               use.survey.bathymetry = TRUE,
                               include.corners = FALSE,
                               split.land.at.180 = TRUE,
                               fix.invalid.geom = TRUE,
                               high.resolution.coast = FALSE) {

  # ###
  # select.region <- "ebs"
  # select.region <- c("ai", "goa")
  # select.region = c("bssa1", "bssa2")
  # set.crs = 3338
  # design.year = NULL
  # use.survey.bathymetry = TRUE
  # include.corners = FALSE
  # split.land.at.180 = TRUE
  # fix.invalid.geom = TRUE
  # high.resolution.coast = FALSE
  # ###

  select.region <- tolower(select.region)

  .check_region(select.region = select.region[1], type = "survey")

  ## Automatically set CRS
  if(set.crs == "auto") {
    message("get_base_layers: select.region = 'auto' now uses Alaska Albers Equal Area (EPSG:3338) by default. This replaces the custom equal area projection PROJ4 strings that were used prior to akgfmaps 3.6.0.")
    set.crs <- "EPSG:3338"
  }

  if(length(select.region) > 1 & length(design.year) > 1) {
    stop("get_base_layers: Cannot use multiple select.region and multiple design.year -- only one at a time.")
  }

  inpfc.strata <- NULL
  survey.grid <- NULL
  survey_definition_id <- NULL

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

  survey_definition_id <- unique(survey_definition_id)

  # Bathymetry and land shapefiles ---------------------------------------------------------------
  if(select.region[1] %in%
     c("bs.south", "sebs", "bs.all", "ebs", "nbs", "bs.north", "ecs", "ebs.ecs")) {

    akland <- sf::st_read(system.file("extdata", "ak_russia.shp", package = "akgfmaps"),
                          quiet = TRUE)

    akland$COUNTRY <- c("RU", "US")

    akland$STATE_PROVINCE <- c(NA, "Alaska")

    bathymetry <- sf::st_read(system.file("extdata", "npac_0-200_meters.shp", package = "akgfmaps"),
                              quiet = TRUE)

  } else if(select.region[1] %in%
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

  # Query layers from built-in geopackage ----
  if(!is.null(survey_definition_id)) {

    survey.area <- sf::st_read(
      system.file("extdata", "v4", "afsc_bottom_trawl_surveys.gpkg", package = "akgfmaps"),
      query = paste0("SELECT SURVEY_DEFINITION_ID, SURVEY_NAME,
                      DESIGN_YEAR, AREA_ID, AREA_M2, GEOM AS geometry
                     FROM SURVEY_AREA WHERE SURVEY_DEFINITION_ID IN (",
                     paste(survey_definition_id, collapse = ", "), ")"
                     ),
      quiet = TRUE
      )

    survey.grid <- sf::st_read(
      system.file("extdata", "v4", "afsc_bottom_trawl_surveys.gpkg", package = "akgfmaps"),
      query = paste0("SELECT SURVEY_DEFINITION_ID, DESIGN_YEAR, GRID_ID,
                      STATION, AREA_M2, GEOM AS geometry
                     FROM SURVEY_GRID WHERE SURVEY_DEFINITION_ID IN (",
                     paste(survey_definition_id, collapse = ", "), ")"
                     ),
      quiet = TRUE
      )

    survey.strata <- sf::st_read(
      system.file("extdata", "v4", "afsc_bottom_trawl_surveys.gpkg", package = "akgfmaps"),
      query = paste0("SELECT SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID AS STRATUM,
                      AREA_M2, GEOM AS geometry FROM SURVEY_STRATA WHERE SURVEY_DEFINITION_ID IN (",
                     paste(survey_definition_id, collapse = ", "), ")"
                     ),
      quiet = TRUE
      )

  }

  # Filter by design.year ----

  if(!is.null(design.year)) {

    survey.area <- survey.area[survey.area$DESIGN_YEAR == max(survey.area$DESIGN_YEAR), ]

    survey.grid <- survey.area[survey.grid$DESIGN_YEAR == max(survey.grid$DESIGN_YEAR), ]

    survey.strata <- survey.strata[survey.strata$DESIGN_YEAR == max(survey.strata$DESIGN_YEAR), ]
  } else {
    # Return the current design when design.year is NULL
    survey.area <- survey.area[
      with(survey.area,
           ave(DESIGN_YEAR, SURVEY_DEFINITION_ID, FUN = function(v) v == max(v))) == 1,
      ]

    survey.grid <- survey.grid[
      with(survey.grid,
           ave(DESIGN_YEAR, SURVEY_DEFINITION_ID, FUN = function(v) v == max(v))) == 1,
    ]

    survey.strata <- survey.strata[
      with(survey.strata,
           ave(DESIGN_YEAR, SURVEY_DEFINITION_ID, FUN = function(v) v == max(v))) == 1,
    ]

  }

  # Get INPFC strata for the GOA and AI bottom trawl surveys ---------------------------------------
  if(any(c("ai", "ai.west", "ai.central", "ai.east") %in% select.region)) {
    inpfc.strata <- get_inpfc_strata(select.region = "ai", set.crs = set.crs)
  }

  if(any(c("goa", "goa.west", "goa.east") %in% select.region)) {
    if(is(inpfc.strata, "sf")) {
      inpfc.strata <- rbind(inpfc.strata,
                            get_inpfc_strata(select.region = "goa", set.crs = set.crs))
    } else {
      inpfc.strata <- get_inpfc_strata(select.region = "goa", set.crs = set.crs)
    }
  }

  # Set EBS slope extent when only subareas are selected -------------------------------------------

  if(all(select.region %in% c("bssa1", "bssa2", "bssa3", "bssa4", "bssa5", "bssa6"))) {

    subarea <- c(1,2,3,4,5,6)[match(select.region,  c("bssa1", "bssa2", "bssa3", "bssa4", "bssa5", "bssa6"))]

    stratum.extent <- survey.strata[floor(survey.strata$STRATUM/10) == subarea, ]

  }

  # Longline EBS -----------------------------------------------------------------------------------
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
  if(!is(set.crs, "crs")) {

    set.crs <- sf::st_crs(set.crs)

  }

  # Set CRS for layers -----------------------------------------------------------------------------
  akland <- sf::st_transform(akland, crs = set.crs)
  survey.area <- sf::st_transform(survey.area, crs = set.crs)
  survey.strata <- sf::st_transform(survey.strata, crs = set.crs)
  bathymetry <- sf::st_transform(bathymetry, crs = set.crs)

  # Set plot boundary ------------------------------------------------------------------------------
  if(all(select.region %in% c("ai.east", "ai.west", "ai.central", "goa.west", "goa.east"))) {

    lat.lon.grid <- sf::st_transform(survey.grid, crs = "EPSG:4269")

    grid_index <- numeric()

    if("ai.east" %in% select.region) {
      grid_index <- c(grid_index,
                      which(sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] > -173.5 &
                            sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] < 0)
      )
    }

    if("ai.central" %in% select.region) {
      grid_index <- c(grid_index,
                      which((sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] <= -173.5 |
                             (sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] > 178.5)) &
                            sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,2] < 53.25)
      )
    }

    if("ai.west" %in% select.region) {
      grid_index <- c(grid_index,
                      which(sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] > 0 &
                            sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] <= 178.5)
      )
    }

    if("goa.west" %in% select.region) {
      grid_index <- c(grid_index,
                      which(sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] < -150)
      )
    }

    if("goa.east" %in% select.region) {
      grid_index <- c(grid_index,
                      which(sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] >= -150)
      )
    }

    sub.area.cells <- lat.lon.grid[unique(grid_index), ]

    sub.area.cells <- sf::st_transform(sub.area.cells, crs = set.crs)

    plot.boundary <- sf::st_bbox(sub.area.cells)
    plot.boundary <- data.frame(x = c(plot.boundary['xmin'], plot.boundary['xmax']),
                                y = c(plot.boundary['ymin'], plot.boundary['ymax']))


  }

  # Setup plot boundaries and axis labels ("breaks") -----------------------------------------------
  if(exists("stratum.extent")) {
    stratum.extent <- sf::st_transform(stratum.extent, crs = set.crs)
    plot.boundary <- sf::st_bbox(stratum.extent)
    plot.boundary <- data.frame(x = c(plot.boundary['xmin'], plot.boundary['xmax']),
                                y = c(plot.boundary['ymin'], plot.boundary['ymax']))
    bbox <- sf::st_bbox(stratum.extent)

  } else {
    plot.boundary <- sf::st_bbox(survey.area)
    plot.boundary <- data.frame(x = c(plot.boundary['xmin'], plot.boundary['xmax']),
                                y = c(plot.boundary['ymin'], plot.boundary['ymax']))
    bbox <- sf::st_bbox(survey.area)
  }

  bbox <- sf::st_transform(bbox, crs = "NAD83")

  xmin <- ifelse(sign(bbox['xmin']) == 1, bbox['xmin'], bbox['xmin'] + 360)
  xmax <- ifelse(sign(bbox['xmax']) == 1, bbox['xmax'], bbox['xmax'] + 360)
  x_interval <- akgfmaps:::set_dd_interval(xmax - xmin)

  lon.breaks <- seq(-180, 180 - x_interval, x_interval)

  ymin <- ifelse(sign(bbox['ymin']) == 1, bbox['ymin'], bbox['ymin'] + 180)
  ymax <- ifelse(sign(bbox['ymax']) == 1, bbox['ymax'], bbox['ymax'] + 180)
  y_interval <- akgfmaps:::set_dd_interval(ymax - ymin)

  lat.breaks <- seq(-90, 90, y_interval)

  # Make graticule ---------------------------------------------------------------------------------
  graticule <- sf::st_graticule(lat = lat.breaks,
                                lon = lon.breaks,
                                margin = 1e-5)

  # Get place labels--------------------------------------------------------------------------------
  place.labels <- utils::read.csv(file = system.file("extdata", "placenames.csv", package = "akgfmaps"))

  place.labels <- place.labels[place.labels$region == select.region[1], ]
  place.labels <- akgfmaps::transform_data_frame_crs(place.labels, out.crs = set.crs)


  # Attempt to correct any remaining degenerate geometries and dateline wrapping issues --------------

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

# test <- list(region = select.region,
#      akland = akland,
#      survey.area = survey.area,
#      survey.strata = survey.strata,
#      survey.grid = survey.grid,
#      bathymetry = bathymetry,
#      place.labels = place.labels,
#      graticule = graticule,
#      crs = set.crs,
#      plot.boundary = plot.boundary,
#      lon.breaks = lon.breaks,
#      lat.breaks = lat.breaks,
#      inpfc.strata = inpfc.strata)
#
# ggplot() +
#   geom_sf(data = test$akland) +
#   geom_sf(data = test$survey.area,
#           mapping = aes(color = "Survey area",
#                         fill = "Survey area"),
#           alpha = 0.7) +
#   geom_sf(data = test$survey.strata,
#           mapping = aes(color = "Survey strata"),
#           fill = NA) +
#   geom_sf(data = test$survey.grid,
#           mapping = aes(color = "Survey grid")) +
#   geom_sf(data = test$graticule,
#           alpha = 0.3) +
#   scale_fill_colorblind() +
#   scale_color_colorblind() +
#   scale_x_continuous(limits = test$plot.boundary$x,
#                      breaks = test$lon.breaks) +
#   scale_y_continuous(limits = test$plot.boundary$y,
#                      breaks = test$lat.breaks) +
#   theme_bw()
