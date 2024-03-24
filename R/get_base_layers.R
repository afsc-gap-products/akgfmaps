#' Function to get base layers for plotting
#'
#' This function loads often-used layers used for plotting the eastern Bering Sea continental shelf.
#' @param select.region Character vector indicating which region. Options = ebs or bs.all, sebs or bs.south, nbs or bs.north, ecs, ebs.ecs, ai, ai.west, ai.central, ai.east, goa, goa.west, goa.east, ebs.slope, bssa1, bssa2, bssa3, bssa4, bssa5, bssa6
#' @param set.crs Which coordinate reference system should be used? If 'auto', an Albers Equal Area coordinate reference system is automatically assigned.
#' @param include.corners Logical. Should corner stations be returned in the survey grid? Only for the EBS.
#' @param fix.invalid.geom Should invalid geometries be corrected using st_make_valid() and st_wrap_dateline()?
#' @return A list containing sf objects land, bathymetry, survey area boundary, survey strata, survey grid (optional), a data frame of feature labels, coordinate reference system for all objects, and a suggested boundary.
#' @import sf
#' @export

get_base_layers <- function(select.region,
                            set.crs = "+proj=longlat +datum=NAD83",
                            include.corners = NULL,
                            fix.invalid.geom = TRUE,
                            ...) {

  # Return a warning when use.survey.bathymetry = TRUE

  stopifnot("get_base_layers: include.corners argument must be NULL, TRUE, or FALSE." = c(is.null(include.corners) || is.logical(include.corners)))

  if(!is.null(include.corners) & !(select.region %in% c("ebs", "bs.all", "sebs", "bs.south", "ebs.ecs"))) {
    warning("get_base_layers: Ignoring include.corners since include.corners is only valid when select.region include.cornerss the SEBS")
  }

  if(is.null(include.corners) & select.region %in% c("ebs", "bs.all", "sebs", "bs.south", "ebs.ecs")) {
    warning("get_base_layers: Corner stations are no longer included in the default EBS shelf survey.grid design. To include.corners corner stations in the survey.grid, set include.corners = FALSE. Refer to release notes for akgfmaps version 3.5.0 for more info (https://github.com/afsc-gap-products/akgfmaps/blob/master/NEWS).")
    include.corners <- FALSE
  }

  if(select.region %in% c("ebs", "bs.all", "sebs", "bs.south", "ebs.ecs")) {

    grid.file <- ifelse(include.corners, "bs_grid_w_corners.shp", "bs_grid.shp")

  }

  select.region <- tolower(select.region)

  .check_region(select.region = select.region, type = "survey")

  ## Automatically set CRS
  if(set.crs == "auto") {
    region.crs <- c(
      "+proj=aea +lat_1=55 +lat_2=60 +lat_0=57.5 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=55 +lat_2=60 +lat_0=57.5 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=68.1 +lat_2=70.7 +lat_0=69.4 +lon_0=-162.6 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=60.8 +lat_2=67 +lat_0=63.9 +lon_0=-167 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=50.83 +lat_2=52.67 +lat_0=51.75 +lon_0=-179 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=50.83 +lat_2=52.67 +lat_0=51.75 +lon_0=-179 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=50.83 +lat_2=52.67 +lat_0=51.75 +lon_0=-179 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=50.83 +lat_2=52.67 +lat_0=51.75 +lon_0=-179 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=54.4 +lat_2=57.6 +lat_0=56 +lon_0=-149.25 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=54.4 +lat_2=57.6 +lat_0=56 +lon_0=-149.25 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=54.4 +lat_2=57.6 +lat_0=56 +lon_0=-149.25 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
    set.crs <- region.crs[match(select.region, c("bs.south",
                                                 "sebs",
                                                 "bs.all",
                                                 "ebs",
                                                 "bs.north",
                                                 "nbs",
                                                 "ecs",
                                                 "ebs.ecs",
                                                 "ai",
                                                 "ai.west",
                                                 "ai.central",
                                                 "ai.east",
                                                 "goa",
                                                 "goa.west",
                                                 "goa.east",
                                                 "ebs.slope",
                                                 "bssa1",
                                                 "bssa2",
                                                 "bssa3",
                                                 "bssa4",
                                                 "bssa5",
                                                 "bssa6"))]
  }

  inpfc.strata <- NULL
  survey.grid <- NULL

  # Bathymetry and land shapefiles ---------------------------------------------------------------
  akland <- sf::st_read(system.file("extdata", "Alaska_Coastline.shp", package = "akgfmaps"), quiet = TRUE)

  bathymetry <- sf::st_read(system.file("extdata", "npac_bathy_1998.shp", package = "akgfmaps"), quiet = TRUE)

  if(select.region %in% c("bs.south", "sebs", "bs.all", "ebs", "nbs", "bs.north", "ecs", "ebs.ecs")) {

    akland <- dplyr::bind_rows(akland,
                               sf::st_read(system.file("extdata", "e_russia.shp", package = "akgfmaps"), quiet = TRUE) |>
                                 sf::st_transform(crs = sf::st_crs(akland)))

    sel.depths <- c(50, 100, 200)


  } else if(select.region %in% c("ebs.slope", "bssa1", "bssa2", "bssa3", "bssa4", "bssa5", "bssa6")) {

    akland <- dplyr::bind_rows(akland,
                               sf::st_read(system.file("extdata", "e_russia.shp", package = "akgfmaps"), quiet = TRUE) |>
                                 sf::st_transform(crs = sf::st_crs(akland)))

    sel.depths <- c(100, 200, 400, 600, 1000)

  } else if(select.region %in% c("goa", "goa.west", "goa.east")) {

    akland <- dplyr::bind_rows(akland,
                               sf::st_read(system.file("extdata", "w_canada.shp", package = "akgfmaps"), quiet = TRUE) |>
                                 sf::st_transform(crs = sf::st_crs(akland)))

    sel.depths <- c(100, 300, 500, 700)

  } else if(select.region %in% c(  "ai","ai.west", "ai.central", "ai.east")) {

    akland <- dplyr::bind_rows(akland,
                               sf::st_read(system.file("extdata", "w_canada.shp", package = "akgfmaps"), quiet = TRUE) |>
                                 sf::st_transform(crs = sf::st_crs(akland)))

    sel.depths <- c(100, 200, 700)

  }

  bathymetry <- bathymetry[bathymetry$METERS %in% sel.depths, ]


  # SEBS--------------------------------------------------------------------------------------------
  if(select.region %in% c("bs.south", "sebs")) {

    survey.strata <- sf::st_read(system.file("extdata", "ebs_strata.shp", package = "akgfmaps"),
                                 quiet = TRUE)

    survey.strata <- survey.strata[survey.strata$STRATUM %in% c(10, 20, 31, 32, 41, 42, 43, 50, 61, 62, 82, 90), ]

    survey.grid <- sf::st_read(system.file("extdata", grid.file, package = "akgfmaps"),
                               quiet = TRUE)

    lon.breaks <- seq(-180, -154, 5)
    lat.breaks <- seq(54, 64, 2)
  }

  # NEBS:NBS+SEBS-----------------------------------------------------------------------------------
  if(select.region %in% c("bs.all", "ebs")) {

    survey.strata <- sf::st_read(system.file("extdata", "ebs_strata.shp", package = "akgfmaps"),
                                 quiet = TRUE)

    survey.grid <- sf::st_read(system.file("extdata", grid.file, package = "akgfmaps"),
                               quiet = TRUE)

    lon.breaks <- seq(-180, -154, 5)
    lat.breaks <- seq(54, 66, 2)
  }

  # NBS --------------------------------------------------------------------------------------------
  if (select.region %in% c("bs.north", "nbs")) {

    survey.strata <- sf::st_read(system.file("extdata", "ebs_strata.shp", package = "akgfmaps"),
                                 quiet = TRUE)

    survey.strata <- survey.strata[survey.strata$STRATUM %in% c(81, 70, 71), ]

    survey.grid <- sf::st_read(system.file("extdata", "bs_grid_w_corners.shp", package = "akgfmaps"),
                               quiet = TRUE)

    lon.breaks <- seq(-180, -154, 5)
    lat.breaks <- seq(60, 66, 2)
  }

  # EBS Slope --------------------------------------------------------------------------------------
  if(select.region %in% c("ebs.slope", "bssa1", "bssa2", "bssa3", "bssa4", "bssa5", "bssa6")) {

    survey.strata <- sf::st_read(system.file("extdata", "slope_strata.shp", package = "akgfmaps"),
                                 quiet = TRUE)

    lon.breaks <- seq(-180, -155, 5)
    lat.breaks <- seq(52, 64, 2)

    if(select.region %in% c("bssa1", "bssa2", "bssa3", "bssa4", "bssa5", "bssa6")) {
      strata.temp <- survey.strata
      subarea <- c(1,2,3,4,5,6)[match(select.region,  c("bssa1", "bssa2", "bssa3", "bssa4", "bssa5", "bssa6"))]

      survey.strata <- strata.temp[floor(strata.temp$STRATUM/10) %in% (subarea + c(-1,0,1)), ]

      stratum.extent <- strata.temp[floor(strata.temp$STRATUM/10) == subarea, ]

      lon.breaks <- seq(-180, -155, 1)
      lat.breaks <- seq(52, 64, 0.5)
    }


  }


  # Chukchi-----------------------------------------------------------------------------------------
  if(select.region == "ecs") {

    survey.strata <- sf::st_read(system.file("extdata", "chukchi_strata.shp", package = "akgfmaps"),
                                 quiet = TRUE)

    lon.breaks <- seq(-180, -154, 5)
    lat.breaks <- seq(66, 76, 2)
  }

  # Chukchi+EBS ------------------------------------------------------------------------------------
  if(select.region == "ebs.ecs") {

    survey.strata <- sf::st_read(system.file("extdata", "chukchi_strata.shp", package = "akgfmaps"), quiet = TRUE) |>
      dplyr::bind_rows(sf::st_read(system.file("extdata", "ebs_strata.shp", package = "akgfmaps"), quiet = TRUE))

    survey.strata <- sf::st_read(system.file("extdata", "ebs_chukchi_strata.shp", package = "akgfmaps"), quiet = TRUE)

    lon.breaks <- seq(-180, -150, 5)
    lat.breaks <- seq(54,78,4)
  }

  # Aleutian Islands -------------------------------------------------------------------------------
  if(select.region == "ai") {

    survey.strata <- sf::st_read(system.file("extdata", "ai_strata.shp", package = "akgfmaps"), quiet = TRUE)

    survey.grid <- sf::st_read(system.file("extdata", "ai_grid.shp", package = "akgfmaps"), quiet = TRUE)

    inpfc.strata <- get_inpfc_strata(select.region = "ai", set.crs = set.crs)

    lon.breaks <- c(170, 175, -180, -175, -170, -165, -160)
    lat.breaks <- seq(44, 56, 2)
  }

  # Aleutian Islands - East ------------------------------------------------------------------------
  if(select.region == "ai.east") {

    survey.strata <- sf::st_read(system.file("extdata", "ai_strata.shp", package = "akgfmaps"), quiet = TRUE)

    survey.grid <- sf::st_read(system.file("extdata", "ai_grid.shp", package = "akgfmaps"), quiet = TRUE)

    inpfc.strata <- get_inpfc_strata(select.region = "ai", set.crs = set.crs)

    lon.breaks <- seq(-176, -164, 2)
    lat.breaks <- seq(52, 55, 1)
  }

  # Aleutian Islands - Central ---------------------------------------------------------------------
  if(select.region == "ai.central") {

    survey.strata <- sf::st_read(system.file("extdata", "ai_strata.shp", package = "akgfmaps"), quiet = TRUE)

    survey.grid <- sf::st_read(system.file("extdata", "ai_grid.shp", package = "akgfmaps"), quiet = TRUE)

    inpfc.strata <- get_inpfc_strata(select.region = "ai", set.crs = set.crs)

    lon.breaks <- c(176, 178, 180, seq(-178, -170, 2))
    lat.breaks <- seq(51,55,2)
  }

  # Aleutian Islands - West ---------------------------------------------------------------------
  if(select.region == "ai.west") {

    survey.strata <- sf::st_read(system.file("extdata", "ai_strata.shp", package = "akgfmaps"), quiet = TRUE)

    survey.grid <- sf::st_read(system.file("extdata", "ai_grid.shp", package = "akgfmaps"), quiet = TRUE)

    inpfc.strata <- get_inpfc_strata(select.region = "ai", set.crs = set.crs)

    lon.breaks <- seq(168, 180, 2)
    lat.breaks <- seq(50,55,2)
  }


  # Gulf of Alaska ---------------------------------------------------------------------------------
  if(select.region == "goa") {

    survey.strata <- sf::st_read(system.file("extdata", "goa_strata.shp", package = "akgfmaps"), quiet = TRUE)

    survey.grid <- sf::st_read(system.file("extdata", "goa_grid.shp", package = "akgfmaps"), quiet = TRUE)

    inpfc.strata <- get_inpfc_strata(select.region = "goa", set.crs = set.crs)

    lon.breaks <- seq(-175, -130, 5)
    lat.breaks <- seq(52,64,2)
  }

  # Gulf of Alaska - West --------------------------------------------------------------------------
  if(select.region == "goa.west") {

    survey.strata <- sf::st_read(system.file("extdata", "goa_strata.shp", package = "akgfmaps"), quiet = TRUE)

    survey.grid <- sf::st_read(system.file("extdata", "goa_grid.shp", package = "akgfmaps"), quiet = TRUE)

    inpfc.strata <- get_inpfc_strata(select.region = "goa", set.crs = set.crs)

    lon.breaks <- seq(-174, -144, 2)
    lat.breaks <- seq(52, 64, 2)
  }

  # Gulf of Alaska - East --------------------------------------------------------------------------
  if(select.region == "goa.east") {

    survey.strata <- sf::st_read(system.file("extdata", "goa_strata.shp", package = "akgfmaps"), quiet = TRUE)

    survey.grid <- sf::st_read(system.file("extdata", "goa_grid.shp", package = "akgfmaps"), quiet = TRUE)

    inpfc.strata <- get_inpfc_strata(select.region = "goa", set.crs = set.crs)

    lon.breaks <- seq(-160, -124, 2)
    lat.breaks <- seq(52, 64, 2)
  }

  # Make survey area polygon from union of stratum polygons ----------------------------------------
  survey.area <- survey.strata |>
    dplyr::select(geometry) |>
    dplyr::mutate(ID = 1) |>
    dplyr::summarise(do_union = TRUE)

  # Set CRS-----------------------------------------------------------------------------------------
  if(tolower(class(set.crs)) != "crs") {
    set.crs <- sf::st_crs(set.crs)
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

    if(select.region %in% c("bs.all", "ebs", "bs.south", "sebs", "bs.north", "nbs")) {
      survey.grid$STATIONID[survey.grid$STATIONID == "Z-04"] <- "AZ0504" # Divided station in SEBS

      survey.grid <- survey.grid[survey.grid$STATIONID %in% akgfmaps::get_survey_stations(select.region = select.region), ]
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

        survey.grid <- survey.grid[survey.grid$STATIONID %in% akgfmaps::get_survey_stations(select.region = select.region), ]
      }
    }
  }

  # Set plot boundary ------------------------------------------------------------------------------
  if(select.region %in% c("ai.east", "ai.west", "ai.central", "goa.west", "goa.east")) {

    lat.lon.grid <- sf::st_transform(survey.grid, crs = "EPSG:4269")

    if(select.region == "ai.east") {
      grid_index <- which(sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] > -173.5 &
                            sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] < 0)
    }

    if(select.region == "ai.central") {
      grid_index <- which((sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] <= -173.5 |
                             (sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] > 178.5)) &
                            sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,2] < 53.25)
    }

    if(select.region == "ai.west") {
      grid_index <- which(sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] > 0 &
                            sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] <= 178.5)
    }

    if(select.region == "goa.west") {
      grid_index <- which(sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] < -150)
    }

    if(select.region == "goa.east") {
      grid_index <- which(sf::st_coordinates(sf::st_centroid(lat.lon.grid))[,1] >= -150)
    }

    sub.area.cells <- lat.lon.grid[grid_index, ]

    sub.area.cells <- sf::st_transform(sub.area.cells, crs = set.crs)

    plot.boundary <- sf::st_bbox(sub.area.cells)
    plot.boundary <- data.frame(x = c(plot.boundary['xmin'], plot.boundary['xmax']),
                                y = c(plot.boundary['ymin'], plot.boundary['ymax']))


  } else if(select.region %in% c("bssa1", "bssa2", "bssa3", "bssa4", "bssa5", "bssa6")) {
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

  place.labels <- place.labels[place.labels$region == select.region, ]
  place.labels <- akgfmaps::transform_data_frame_crs(place.labels, out.crs = set.crs)


  # Attempt to correct any remaining degenerate geometry and dateline wrapping issues --------------

  if(fix.invalid.geom) {
    akland <- akgfmaps:::fix_geometry(x = akland)
    survey.area <- akgfmaps:::fix_geometry(x = survey.area)
    survey.grid <- akgfmaps:::fix_geometry(x = survey.grid)
    survey.strata <- akgfmaps:::fix_geometry(x = survey.strata)
    bathymetry <- akgfmaps:::fix_geometry(x = bathymetry)
    inpfc.strata <- akgfmaps:::fix_geometry(x = inpfc.strata)
  }

  return(list(akland = akland,
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
