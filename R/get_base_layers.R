#' Function to get base layers for plotting
#' 
#' This function loads often-used layers used for plotting the eastern Bering Sea continental shelf.
#' @param select.region Character vector indicating which region. Options = ebs or bs.all, sebs or bs.south, ecs, ebs.ecs
#' @param set.crs Which coordinate reference system should be used? If 'auto', an Albers Equal Area coordinate reference system is automatically assigned.
#' @param use.survey.bathymetry Should survey bathymetry be used?
#' @return A list containing sf objects land, bathymetry, survey area boundary, survey strata, a data frame of feature labels, coordinate reference system for all objects, and a suggested boundary.
#' 
#' @export


get_base_layers <- function(select.region, 
                            set.crs = "+proj=longlat +datum=NAD83", 
                            use.survey.bathymetry = FALSE) {
  
  ## Automatically set CRS
  if(set.crs == "auto") {
    region.crs <- c(
      "+proj=aea +lat_1=55 +lat_2=60 +lat_0=57.5 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=55 +lat_2=60 +lat_0=57.5 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=68.1 +lat_2=70.7 +lat_0=69.4 +lon_0=-162.6 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      "+proj=aea +lat_1=60.8 +lat_2=67 +lat_0=63.9 +lon_0=-167 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
    set.crs <- region.crs[match(select.region, c("bs.south", "sebs", "bs.all", "ebs", "ecs", "ebs.ecs"))]
  }
  
  # Land shapefile----------------------------------------------------------------------------------
  akland <- sf::st_read(system.file("data", "ak_russia.shp", package = "akgfmaps"), quiet = TRUE)
  
  # Bathymetry shapefile-----------------------------------------------------------------------------
  bathymetry <- sf::st_read(system.file("data", "npac_0-200_meters.shp", package = "akgfmaps"), quiet = TRUE)
  
  # SEBS--------------------------------------------------------------------------------------------
  if(select.region %in% c("bs.south", "sebs")) {
    survey.area <- sf::st_read(system.file("data", "sebs_survey_boundary.shp", package = "akgfmaps"), quiet = TRUE)
    survey.strata <- sf::st_read(system.file("data", "sebs_strata.shp", package = "akgfmaps"), quiet = TRUE) 
    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-177.3, -154.3), 
                                                                   y = c(54.5, 63.15)), 
                                                        out.crs = set.crs)
    graticule <- st_graticule(lat = seq(54,64,2), 
                              lon = seq(-180,-140, 5), 
                              margin = 1e-5)
    lon.breaks <- seq(-180, -154, 5)
    lat.breaks <- seq(54,64,2)
  }
  
  # NEBS+SEBS---------------------------------------------------------------------------------------
  if(select.region %in% c("bs.all", "ebs")) {
    survey.area <- sf::st_read(system.file("data", "ebs_survey_boundary.shp", package = "akgfmaps"), quiet = TRUE)
    survey.strata <- sf::st_read(system.file("data", "ebs_strata.shp", package = "akgfmaps"), quiet = TRUE) 
    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-177.8, -154.7), 
                                                                   y = c(54, 65.1)), 
                                                        out.crs = set.crs)
    graticule <- st_graticule(lat = seq(54,68,2), 
                              lon = seq(-180,-140, 5), 
                              margin = 1e-5)
    lon.breaks <- seq(-180, -154, 5)
    lat.breaks <- seq(54,66,2)
  }
  
  # Chukchi---------------------------------------------------------------------------------------
  if(select.region == "ecs") {
    survey.area <- sf::st_read(system.file("data", "chukchi_survey_boundary.shp", package = "akgfmaps"), quiet = TRUE)
    survey.strata <- sf::st_read(system.file("data", "chukchi_strata.shp", package = "akgfmaps"), quiet = TRUE) 
    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-170, -156), 
                                                                   y = c(65, 73)), 
                                                        out.crs = set.crs)
    graticule <- st_graticule(lat = seq(60,76,2), 
                              lon = seq(-180,-140, 5), 
                              margin = 1e-5)
    lon.breaks <- seq(-180, -154, 5)
    lat.breaks <- seq(66,76,2)
  }
  
  # Chukchi+EBS-------------------------------------------------------------------------------------
  if(select.region == "ebs.ecs") {
    survey.area <- sf::st_read(system.file("data", "ebs_chukchi_survey_boundary.shp", package = "akgfmaps"), 
                               quiet = TRUE)
    survey.strata <- sf::st_read(system.file("data", "ebs_chukchi_strata.shp", package = "akgfmaps"), quiet = TRUE) 
    bathymetry <- sf::st_read(system.file("data", "npac_0-200_meters.shp", package = "akgfmaps"), quiet = TRUE)
    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-177, -151), 
                                                                   y = c(54.5, 72.5)), 
                                                        out.crs = set.crs)
    graticule <- st_graticule(lat = seq(54,78,4), 
                              lon = seq(-180,-140, 5), 
                              margin = 1e-5)
    lon.breaks <- seq(-180, -150, 5)
    lat.breaks <- seq(54,78,4)
  }
  
  # Set CRS-----------------------------------------------------------------------------------------
  if(tolower(class(set.crs)) != "crs") {
    set.crs <- sf::st_crs(set.crs)
  }
  
  akland <- akland %>% sf::st_transform(crs = set.crs)
  survey.area <- survey.area %>% sf::st_transform(crs = set.crs)
  survey.strata <- survey.strata %>% sf::st_transform(crs = set.crs)
  bathymetry <- bathymetry %>% sf::st_transform(crs = set.crs)
  
  
  # Get place labels--------------------------------------------------------------------------------
  place.labels <- read.csv(file = system.file("data", "placenames.csv", package = "akgfmaps")) %>%
    dplyr::filter(region == select.region) %>%
    akgfmaps::transform_data_frame_crs(out.crs = set.crs)
  
  return(list(akland = akland,
              survey.area = survey.area,
              survey.strata = survey.strata,
              bathymetry = bathymetry,
              place.labels = place.labels,
              graticule = graticule,
              crs = set.crs,
              plot.boundary = plot.boundary,
              lon.breaks = lon.breaks,
              lat.breaks = lat.breaks))
}