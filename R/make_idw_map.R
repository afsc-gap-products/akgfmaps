#' Make IDW maps of CPUE for the EBS/NBS
#' 
#' This function can be used to make inverse-distance-weighted plots for the eastern Bering Sea and northern Bering Sea

#' @param x Data frame which contains at minimum: CPUE, LATITUDE, and LONGITUDE. Can be passed as vectors instead (see below). Default value: \code{NA}
#' @param region Character vector indicating which plotting region to use. Options: bs.south, bs.north, bs.all
#' @param grid.cell Numeric vector of length two specifying dimensions of grid cells for extrpolation grid, in degrees c(lon, lat). Default c(0.05, 0.05)
#' @param COMMON_NAME
#' @param LATITUDE
#' @param LONGITUDE
#' @param CPUE_KGHA
#' @param extrap.box Optional. Vector specifying the dimensions of the extrapolation grid. Elements of the vector should be named to specify the minimum and maximum x and y values c(xmn, xmx, ymn, ymx). If not provided, region will be used to set the extrapolation area.
#' @param set.breaks Suggested. Vector of break points to use for plotting. Alternatively, a character vector indicating which break method to use. Default = "jenks"
#' @param grid.cell Optional. Numeric vector of length two, specifying the resolution for the extrapolation grid in degrees. Default c(0.05,0.05)
#' @param set.crs Character vector containing the coordinate reference system for projecting the extrapolation grid.
#' @param key.title Character vector which will appear in the legend above CPUE (kg/ha). Default = "auto" tries to pull COMMON_NAME from input.
#' @param log.transform Character vector indicating whether CPUE values should be log-transformed for IDW. Default = FALSE.
#' @param idw.nmax Maximum number of adjacent stations to use for interpolation. Default = 4
#' 
#' @return Returns a list containing: (1) $plot–a ggplot IDW map, (2) $extrapolation.grid–the extrapolation grid with estimated values, (4) $region–the region, which is used by \code{gapidw::change_fill_color()} for subsequent plot labeling, (4) $n.breaks–the number of level breaks, which is used by \code{gapidw::change_fill_color()} to adjust scale.
#' 
#' @author Sean Rohan \email{sean.rohan@@noaa.gov}

make_idw_map <- function(x = NA, COMMON_NAME = NA, LATITUDE = NA, LONGITUDE = NA, CPUE_KGHA = NA, region = "bs.south", extrap.box = NA, set.breaks = "jenks", grid.cell = c(0.05, 0.05), set.crs = "+proj=longlat +datum=NAD83", proj.crs = "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", key.title = "auto", log.transform = FALSE, idw.nmax = 4) {
  
  if(is.na(x)) {
    x <- data.frame(COMMON_NAME = COMMON_NAME,
                    LATITUDE = LATITUDE,
                    LONGITUDE = LONGITUDE,
                    CPUE_KGHA = CPUE_KGHA)
  }
  
  # Set legend title--------------------------------------------------------------------------------
  if(key.title == "auto") {
    key.title <- x$COMMON_NAME[1]
  }
  
  # Set up mapping region---------------------------------------------------------------------------
  if(is.na(extrap.box)) {
    if(region == "bs.south") {extrap.box = c(xmn = -179.5, xmx = -157, ymn = 54, ymx = 63)}
    if(region == "bs.all") {extrap.box = c(xmn = -179.5, xmx = -157, ymn = 54, ymx = 68)}
  }
  
  # Assign CRS to input data------------------------------------------------------------------------
  x <- sf::st_as_sf(x, coords = c(x = "LONGITUDE", y = "LATITUDE"), crs = sf::st_crs(set.crs)) %>% 
    sf::st_transform(crs = sf::st_crs(proj.crs))
  
  akland <- sf::st_read(system.file("data", "ak_russia.shp", package = "akgfmaps"), quiet = TRUE) %>% 
    sf::st_transform(crs = sf::st_crs(proj.crs))
  
  # SEBS--------------------------------------------------------------------------------------------
  if(region == "bs.south") {
    survey.area <- sf::st_read(system.file("data", "ebs_south_survey_boundary.shp", package = "akgfmaps"), quiet = TRUE) %>% 
      sf::st_transform(crs = sf::st_crs(x))
    bathymetry <- sf::st_read(system.file("data", "ebs_south_bathymetry.shp", package = "akgfmaps"), quiet = TRUE) %>% 
      sf::st_transform(crs = sf::st_crs(x))
  }
  
  # SEBS + NEBS-------------------------------------------------------------------------------------
  if(region == "bs.all") {
    survey.area <- sf::st_read(system.file("data", "ebs_south_and_north_survey_boundary.shp", package = "akgfmaps"), quiet = TRUE) %>% 
      sf::st_transform(crs = sf::st_crs(x))
    bathymetry <- sf::st_read(system.file("data", "ebs_south_and_north_bathymetry.shp", package = "akgfmaps"), quiet = TRUE) %>% 
      sf::st_transform(crs = sf::st_crs(x))
  }
  
  # Inverse distance weighting----------------------------------------------------------------------
  idw_fit <- gstat::gstat(formula = CPUE_KGHA~1, locations = x, nmax = idw.nmax)
  
  # Predict station points--------------------------------------------------------------------------
  stn.predict <- predict(idw_fit, x)
  
  # Generate extrapolation grid---------------------------------------------------------------------
  sp_extrap.raster <- raster::raster(xmn = extrap.box['xmn'],
                                     xmx=extrap.box['xmx'],
                                     ymn=extrap.box['ymn'],
                                     ymx=extrap.box['ymx'],
                                     ncol=(extrap.box['xmx']-extrap.box['xmn'])/grid.cell,
                                     nrow=(extrap.box['ymx']-extrap.box['ymn'])/grid.cell,
                                     crs = crs(set.crs)) %>% 
    projectRaster(crs = crs(x))
  
  # Predict, rasterize, mask------------------------------------------------------------------------
  extrap.grid <- predict(idw_fit, as(sp_extrap.raster, "SpatialPoints")) %>%
  rasterFromXYZ() %>%
    mask(survey.area) %>%
    as.data.frame(xy = TRUE)

  # Format breaks for plotting----------------------------------------------------------------------
  # Automatic break selection based on character vector.
  if(is.character(set.breaks[1])) {
    set.breaks <- tolower(set.breaks)
    set.breaks <- c(-1, round(classInt::classIntervals(x$CPUE_KGHA, n = 5, style = set.breaks)$brks))
  }
  
  # Ensure breaks go to zero------------------------------------------------------------------------
  if(min(set.breaks) > 0) {
    set.breaks <- c(0, set.breaks)
  }
  
  if(min(set.breaks) == 0) {
    set.breaks <- c(-1, set.breaks)
  }
  
  # Ensure breaks span the full range---------------------------------------------------------------
  if(max(set.breaks) < max(stn.predict$var1.pred)){
    set.breaks[length(set.breaks)] <- max(stn.predict$var1.pred) + 1
  }
  
  
  # Trim breaks to significant digits to account for differences in range among species-------------
  dig.lab <- 7
  set.levels <- cut(stn.predict$var1.pred, set.breaks, right = TRUE, dig.lab = dig.lab)
  
  while(length(grep("\\.", set.levels)) > 0) {
    dig.lab <- dig.lab - 1
    set.levels <- cut(stn.predict$var1.pred, set.breaks, right = TRUE, dig.lab = dig.lab)
    
  }
  
  # Cut extrapolation grid to support discrete scale------------------------------------------------
  extrap.grid$discrete_layer <- cut(extrap.grid$layer, set.breaks, right = TRUE, dig.lab = dig.lab)

  # Which breaks need commas?-----------------------------------------------------------------------
  sig.dig <- round(set.breaks[which(nchar(round(set.breaks)) >= 4)])
  
  # Drop brackets, add commas, create 'No catch' level to legend labels-----------------------------
  make_level_labels <- function(vec) {
    vec <- as.character(vec)
    vec[grep("-1", vec)] <- "No catch"
    vec <- sub("\\(", "\\>", vec)
    vec <- sub("\\,", "–", vec)
    vec <- sub("\\]", "", vec)
    for(j in 1:length(sig.dig)) {
      vec <- sub(sig.dig[j], format(sig.dig[j], nsmall=0, big.mark=","), vec)
    }
    return(vec)
  }
  
  # Assign level names to breaks for plotting-------------------------------------------------------
  extrap.grid$discrete_layer <- factor(make_level_labels(extrap.grid$discrete_layer), levels = make_level_labels(levels(set.levels)))
  
  # Number of breaks for color adjustments----------------------------------------------------------
  n.breaks <- length(levels(set.levels))
  
  # Make plot---------------------------------------------------------------------------------------
  if(region %in% c("bs.south", "bs.all")) {
  p1 <- ggplot() +
    geom_tile(data = extrap.grid, aes(x = x, y = y, fill = discrete_layer)) +
    geom_sf(data = bathymetry) + 
    geom_sf(data = survey.area, fill = NA) +
    geom_sf(data = akland, fill = "grey80") +
    geom_sf(data = bathymetry) + 
    geom_sf(data = st_graticule(lat = seq(50,66,2), lon = seq(-180,-140, 5), margin = 1e-5),  color = alpha("grey70", 0.3)) +
    scale_fill_manual(name = paste0(key.title, "\nCPUE (kg/ha)"), 
                      values = c("white", RColorBrewer::brewer.pal(9, name = "Blues")[c(2,4,6,8,9)]), 
                      na.translate = FALSE, # Don't use NA
                      drop = FALSE) + # Keep all levels in the plot
    scale_x_continuous(breaks = seq(-180, -154, 5)) + 
    scale_y_continuous() + 
    coord_sf(xlim = c(-1.4e6, -1.3e5),
             ylim = c(5e5, 1.8e6)) +
    theme(panel.border = element_rect(color = "black", fill = NA),
          panel.background = element_rect(fill = NA, color = "black"),
          legend.key = element_rect(fill = NA, color = "grey70"),
          legend.position = c(0.12, 0.18),
          axis.title = element_blank(),
          axis.text = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          plot.background = element_rect(fill = NA, color = NA))
  }
  
  return(list(plot = p1, 
              extrapolation.grid = extrap.grid,
              region = region,
              n.breaks = n.breaks,
              key.title = key.title))
}
