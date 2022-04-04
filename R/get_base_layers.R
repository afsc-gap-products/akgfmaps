#' Function to get base layers for plotting
#' 
#' This function loads often-used layers used for plotting the eastern Bering Sea continental shelf.
#' @param select.region Character vector indicating which region. Options = ebs or bs.all, sebs or bs.south, nbs or bs.north, ecs, ebs.ecs, ai, ai.west, ai.central, ai.east, goa, goa.west, goa.east
#' @param set.crs Which coordinate reference system should be used? If 'auto', an Albers Equal Area coordinate reference system is automatically assigned.
#' @param use.survey.bathymetry Should survey bathymetry be used?
#' @param return.survey.grid Should a survey grid be returned (default = FALSE)
#' @return A list containing sf objects land, bathymetry, survey area boundary, survey strata, survey grid (optional), a data frame of feature labels, coordinate reference system for all objects, and a suggested boundary.
#' 
#' @export

get_base_layers <- function(select.region, 
                            set.crs = "+proj=longlat +datum=NAD83", 
                            use.survey.bathymetry = TRUE,
                            return.survey.grid = FALSE) {
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
      "+proj=aea +lat_1=54.4 +lat_2=57.6 +lat_0=56 +lon_0=-149.25 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
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
                                                 "goa.east"))]
  }

  # Bathymetry and land shapefiles ---------------------------------------------------------------  
  if(select.region %in% c("bs.south", "sebs", "bs.all", "ebs", "nbs", "bs.north", "ecs", "ebs.ecs")) {
    akland <- sf::st_read(system.file("data", "ak_russia.shp", package = "akgfmaps"), quiet = TRUE)
    bathymetry <- sf::st_read(system.file("data", "npac_0-200_meters.shp", package = "akgfmaps"), quiet = TRUE)
    
  } else if(select.region %in% c("ai","ai.west", "ai.central", "ai.east", "goa", "goa.west", "goa.east")) {
    akland <- sf::st_read(system.file("data", "alaska_canada_dcw.shp", package = "akgfmaps"), quiet = TRUE)
    bathymetry <- sf::st_read(system.file("data", "alaska_race.shp", package = "akgfmaps"), quiet = TRUE)
  }

  
  # SEBS--------------------------------------------------------------------------------------------
  if(select.region %in% c("bs.south", "sebs")) {
    survey.area <- sf::st_read(system.file("data", "ebs_survey_boundary.shp", package = "akgfmaps"), 
                               quiet = TRUE) %>%
      dplyr::filter(SURVEY == "EBS_SHELF")
    survey.strata <- sf::st_read(system.file("data", "ebs_strata.shp", package = "akgfmaps"), 
                                 quiet = TRUE) %>%
      dplyr::filter(Stratum %in% c(10, 20, 31, 32, 41, 42, 43, 50, 61, 62, 82, 90))
    survey.grid <- sf::st_read(system.file("data", "bs_grid_w_corners.shp", package = "akgfmaps"), 
                               quiet = TRUE)
    survey.grid$STATIONID[survey.grid$STATIONID == "Z-04"] <- "AZ0504" # Divided station in SEBS
    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-168.5, -157), 
                                                                   y = c(54, 59.5)), 
                                                        out.crs = set.crs)
    lon.breaks <- seq(-170, -157, 5)
    lat.breaks <- seq(54,60,2)
  }
  
  # NEBS:NBS+SEBS---------------------------------------------------------------------------------------
  if(select.region %in% c("bs.all", "ebs")) {
    survey.area <- sf::st_read(system.file("data", "ebs_survey_boundary.shp", package = "akgfmaps"), 
                               quiet = TRUE)
    survey.strata <- sf::st_read(system.file("data", "ebs_strata.shp", package = "akgfmaps"), 
                                 quiet = TRUE) 
    survey.grid <- sf::st_read(system.file("data", "bs_grid_w_corners.shp", package = "akgfmaps"), 
                               quiet = TRUE) %>%
      dplyr::filter(STATIONID %in% akgfmaps::get_survey_stations(select.region = "ebs"))
    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-177.8, -154.7), 
                                                                   y = c(54, 65.1)), 
                                                        out.crs = set.crs)
    lon.breaks <- seq(-180, -154, 5)
    lat.breaks <- seq(54,66,2)
  }
    
  # NBS---------------------------------------------------------------------------------------
  if (select.region %in% c("bs.north", "nbs")) {
    survey.area <- sf::st_read(system.file("data", "ebs_survey_boundary.shp", package = "akgfmaps"), 
                               quiet = TRUE) %>% 
      dplyr::filter(SURVEY == "NBS_SHELF")
    
    survey.strata <- sf::st_read(system.file("data", "ebs_strata.shp", package = "akgfmaps"), 
                                 quiet = TRUE) %>% 
      dplyr::filter(Stratum %in% c(81,70,71))
    
    survey.grid <- sf::st_read(system.file("data", "bs_grid_w_corners.shp", package = "akgfmaps"),  
                               quiet = TRUE) %>% 
      dplyr::filter(STATIONID %in% akgfmaps::get_survey_stations("nbs"))
    
    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-160, -176.5), 
                                                                   y = c(60, 66)),  
                                                        out.crs = set.crs)
    lon.breaks <- seq(-180, -154, 5)
    lat.breaks <- seq(60, 66, by = 2)
  }    
  
  # Chukchi---------------------------------------------------------------------------------------
  if(select.region == "ecs") {
    survey.area <- sf::st_read(system.file("data", "chukchi_survey_boundary.shp", package = "akgfmaps"), 
                               quiet = TRUE)
    survey.strata <- sf::st_read(system.file("data", "chukchi_strata.shp", package = "akgfmaps"), 
                                 quiet = TRUE)
    survey.grid <- NULL
    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-170, -156), 
                                                                   y = c(65, 73)), 
                                                        out.crs = set.crs)
    lon.breaks <- seq(-180, -154, 5)
    lat.breaks <- seq(66,76,2)
  }
  
  # Chukchi+EBS ------------------------------------------------------------------------------------
  if(select.region == "ebs.ecs") {
    survey.area <- sf::st_read(system.file("data", "ebs_chukchi_survey_boundary.shp", package = "akgfmaps"), quiet = TRUE)
    survey.strata <- sf::st_read(system.file("data", "ebs_chukchi_strata.shp", package = "akgfmaps"), quiet = TRUE)
    survey.grid <- NULL
    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-177, -151), 
                                                                   y = c(54.5, 72.5)), 
                                                        out.crs = set.crs)
    lon.breaks <- seq(-180, -150, 5)
    lat.breaks <- seq(54,78,4)
  }
  
  # Aleutian Islands -------------------------------------------------------------------------------
  if(select.region == "ai") {
    survey.area <- sf::st_read(system.file("data", "ai_area.shp", package = "akgfmaps"), quiet = TRUE)
    survey.strata <- sf::st_read(system.file("data", "ai_strata.shp", package = "akgfmaps"), quiet = TRUE)
    survey.grid <- sf::st_read(system.file("data", "ai_grid.shp", package = "akgfmaps"), quiet = TRUE)
    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(170, -160), 
                                                                   y = c(49, 54.5)), 
                                                        out.crs = set.crs)
    lon.breaks <- c(170, 175, -180, -175, -170, -165, -160)
    lat.breaks <- seq(44, 56, 2)
  }
  
  # Aleutian Islands - East ------------------------------------------------------------------------
  if(select.region == "ai.east") {
    survey.area <- sf::st_read(system.file("data", "ai_area.shp", package = "akgfmaps"), quiet = TRUE)
    survey.strata <- sf::st_read(system.file("data", "ai_strata.shp", package = "akgfmaps"), quiet = TRUE)
    survey.grid <- sf::st_read(system.file("data", "ai_grid.shp", package = "akgfmaps"), quiet = TRUE)
    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-173.5, -165.7), 
                                                                   y = c(51.8, 54.3)), 
                                                        out.crs = set.crs)
    lon.breaks <- seq(-176, -164, 2)
    lat.breaks <- seq(52, 55, 1)
  }
  
  # Aleutian Islands - Central ---------------------------------------------------------------------
  if(select.region == "ai.central") {
    survey.area <- sf::st_read(system.file("data", "ai_area.shp", package = "akgfmaps"), quiet = TRUE)
    survey.strata <- sf::st_read(system.file("data", "ai_strata.shp", package = "akgfmaps"), quiet = TRUE)
    survey.grid <- sf::st_read(system.file("data", "ai_grid.shp", package = "akgfmaps"), quiet = TRUE)
    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(178.5, -173.5), 
                                                                   y = c(50.8, 52.8)), 
                                                        out.crs = set.crs)
    lon.breaks <- c(176, 178, 180, seq(-178, -170, 2))
    lat.breaks <- seq(51,55,2)
  }
  
  # Aleutian Islands - West ---------------------------------------------------------------------
  if(select.region == "ai.west") {
    survey.area <- sf::st_read(system.file("data", "ai_area.shp", package = "akgfmaps"), quiet = TRUE)
    survey.strata <- sf::st_read(system.file("data", "ai_strata.shp", package = "akgfmaps"), quiet = TRUE)
    survey.grid <- sf::st_read(system.file("data", "ai_grid.shp", package = "akgfmaps"), quiet = TRUE)
    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(171.2, 178.5), 
                                                                   y = c(50.7, 53.75)), 
                                                        out.crs = set.crs)
    lon.breaks <- seq(168, 180, 2)
    lat.breaks <- seq(50,55,2)
  }
  
  
  # Gulf of Alaska ---------------------------------------------------------------------------------
  if(select.region == "goa") {
    survey.area <- sf::st_read(system.file("data", "goa_area.shp", package = "akgfmaps"), quiet = TRUE)
    survey.strata <- sf::st_read(system.file("data", "goa_strata.shp", package = "akgfmaps"), quiet = TRUE)
    survey.grid <- sf::st_read(system.file("data", "goa_grid.shp", package = "akgfmaps"), quiet = TRUE)
    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-170.5, -128), 
                                                                   y = c(52, 60)), 
                                                        out.crs = set.crs)
    lon.breaks <- seq(-175, -130, 5)
    lat.breaks <- seq(52,64,2)
  }
  
  # Gulf of Alaska - West --------------------------------------------------------------------------
  if(select.region == "goa.west") {
    survey.area <- sf::st_read(system.file("data", "goa_area.shp", package = "akgfmaps"), quiet = TRUE)
    survey.strata <- sf::st_read(system.file("data", "goa_strata.shp", package = "akgfmaps"), quiet = TRUE)
    survey.grid <- sf::st_read(system.file("data", "goa_grid.shp", package = "akgfmaps"), quiet = TRUE)
    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-170.5, -150), 
                                                                   y = c(52.2, 60.5)), 
                                                        out.crs = set.crs)
    lon.breaks <- seq(-174, -144, 2)
    lat.breaks <- seq(52, 64, 2)
  }
  
  # Gulf of Alaska - East --------------------------------------------------------------------------
  if(select.region == "goa.east") {
    survey.area <- sf::st_read(system.file("data", "goa_area.shp", package = "akgfmaps"), quiet = TRUE)
    survey.strata <- sf::st_read(system.file("data", "goa_strata.shp", package = "akgfmaps"), quiet = TRUE)
    survey.grid <- sf::st_read(system.file("data", "goa_grid.shp", package = "akgfmaps"), quiet = TRUE)
    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-150, -126.1), 
                                                                   y = c(54, 60.5)), 
                                                        out.crs = set.crs)
    lon.breaks <- seq(-160, -124, 2)
    lat.breaks <- seq(52, 64, 2)
  }
  
  # Set CRS-----------------------------------------------------------------------------------------
  if(tolower(class(set.crs)) != "crs") {
    set.crs <- sf::st_crs(set.crs)
  }
  
  # Use survey bathymetry --------------------------------------------------------------------------
  if(use.survey.bathymetry) {
    if(select.region %in% c("ai", "ai.west", "ai.central", "ai.east")) {
      bathymetry <- dplyr::filter(bathymetry, METERS %in% c(100, 300, 500, 700))    
    } else if(select.region %in% c("goa", "goa.west", "goa.east")) {
      bathymetry <- dplyr::filter(bathymetry, METERS %in% c(100, 200, 700))     
    }
  }
  
  # Make graticule ---------------------------------------------------------------------------------
  graticule <- sf::st_graticule(lat = lat.breaks, 
                            lon = lon.breaks, 
                            margin = 1e-5)
  
  # Set CRS for layers -----------------------------------------------------------------------------
  akland <- akland %>% sf::st_transform(crs = set.crs)
  survey.area <- survey.area %>% sf::st_transform(crs = set.crs)
  survey.strata <- survey.strata %>% sf::st_transform(crs = set.crs)
  bathymetry <- bathymetry %>% sf::st_transform(crs = set.crs)
  
  # Set up survey grid -----------------------------------------------------------------------------
  if(!is.null(survey.grid) & return.survey.grid) {
    survey.grid <- survey.grid %>% sf::st_transform(crs = set.crs)
    
    # EBS survey grid clipping ---------------------------------------------------------------------
    if(select.region %in% c("bs.all", "ebs", "bs.south", "sebs", "bs.north", "nbs")) {
      grid.intersects <- survey.area %>% 
        sf::st_union() %>% 
        sf::st_intersects(survey.grid)
      survey.grid <- survey.grid[grid.intersects[[1]],]
      survey.grid$STATIONID[survey.grid$STATIONID == "Z-04"] <- "AZ0504" # Divided station in SEBS
      survey.mask <- survey.area %>% sf::st_union()
      survey.grid <- sf::st_intersection(survey.grid, survey.mask) %>%
        filter(STATIONID %in% akgfmaps::get_survey_stations(select.region = select.region))
    }
  }
  
  # Get place labels--------------------------------------------------------------------------------
  place.labels <- read.csv(file = system.file("data", "placenames.csv", package = "akgfmaps")) %>%
    dplyr::filter(region == select.region) %>%
    akgfmaps::transform_data_frame_crs(out.crs = set.crs)
  
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
              lat.breaks = lat.breaks))
}
