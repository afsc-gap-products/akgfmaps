#' Reproject a GEBCO raster to bathymetry, slope, and aspect rasters
#' 
#' Reprojects GEBCO raster to a new extent, resolution, and coordinate reference system. Option to also calculate slope and aspect from the rasters.
#' 
#' @param x Either a filepath to a GEBCO netCDF bathymetry file or a raster object.
#' @param z_varname Name of the variable name for elevation/depth in the GEBCO raster.
#' @param z_direction Direction for depth (negative one if positive depth is down)
#' @param raster_template Raster that is used as the template for projecting the gebco_raster. In other words, the GEBCO bathymetry will be projected to the extent, resolution, and coordinate reference system of raster_template. Must provide raster_template OR extent, resolution, and CRS.
#' @param extent Extent of for a raster_template as 4L named numeric vector with names xmn, xmx, ymn, ymx (e.g. extent = c(xmn = -2075672, xmx = -149672.2, ymn = 427321.3, ymx = 2027321)). Must be provided if raster_template is not provided.
#' @param resolution Resolution for a raster_template as a 2L numeric vector where values denote the x and y grid cell resolution. Must be provided if raster_template is not provided
#' @param set_crs Coordinate reference system for the output. Uses the same crs as raster_template if not provided; must be provided if raster_template argument is not provided.
#' @param return_slope_aspect Logical. Should slope and aspect also be returned in the output?
#' @param interpolation_method Method for interpolating raster during reprojection, passed to raster::projecRaster(method)
#' @param slope_nn Number of nearest neighbor cells to use for calculating slope (must be 4 or 8). Passed to raster::terrain(neighbors).
#' @param slope_units Character vector denoting units for slope and aspect ("degrees" or "radians"). Passed to raster::terrain(unit)
#' @return A rasterLayer object with bathymetry when return_slope_aspect argument is FALSE. If return_slope_argument is TRUE, returns a rasterLayer with seafloor_bathymetry, slope, and aspect.
#' @export

reproject_gebco <- function(x, 
                            z_varname = "elevation",
                            z_direction = -1,
                            raster_template = NULL,
                            extent = NULL, 
                            resolution = NULL, 
                            set_crs = NULL,
                            return_slope_aspect = FALSE,
                            interpolation_method = "bilinear",
                            slope_nn = 8,
                            slope_units = "degrees") {
  
  stopifnot("gebco_to_bs: Provide either raster_template or extent and resolution arguments, not both." = (is.null(raster_template) | (is.null(extent) & is.null(resolution))))
  
  if(is.null(raster_template)) {
    
    stopifnot("gebco_to_bs: If template raster is not provided, extent argument must be a named numeric vector of length 4" = length(extent) == 4)
    
    stopifnot("gebco_to_bs: If template raster is not provided, resolution argument must be a numeric vector of length 2 denoting the x and y cell dimensions" = length(resolution) == 4)
    
    stopifnot("gebco_to_bs: If template raster is not provided, extent argument must be a 4L numeric vector with names xmn, xmx, ymn, ymx" = all(c("xmn", "xmx", "ymn", "ymx") %in% extent))
    
    raster_template <- raster::raster(xmn = extent['xmn'],
                                      xmx = extent['xmx'],
                                      ymn = extent['ymn'],
                                      ymx = extent['ymax'], 
                                      resolution = resolution, 
                                      crs = set_crs)
  } else {
    stopifnot("gebco_to_bs: raster_template must be a raster or terra object." = (class(raster_template) %in% c("raster", "RasterLayer", "terra")))
    
    if(!is.null((set_crs))) {
      raster_template <- raster::projectRaster(from = raster_template, crs = set_crs)
    }
    
  }
  
  # Load GEBCO raster if a filepath was provided
  if(class(x) == "character") {
    x <- raster::raster(x, 
                        varname = z_varname, # Elevation (up = positive)
                        crs = "EPSG:4326") # WGS84
  }
  
  if(any(c(x@extent['xmin'], x@extent['xmax']) > 180)) {
    message("reproject_gebco: Raster longitudes greater than 180 degrees detected. Rotating raster from 0 to 360 coordinates to -180 to 180 coordinates.")
    x <- raster::rotate(x)
  }
  
  
  # Project GEBCO to raster template extent, resolution, and coordinate reference system
  output <- raster::projectRaster(from = x, 
                                  to = raster_template, 
                                  method = interpolation_method)
  output <- output * z_direction
  names(output) <- "seafloor_depth"
  
  if(return_slope_aspect) {
    output_slope <- raster::terrain(
      output, 
      opt = 'slope', 
      unit = slope_units, 
      neighbors = slope_nn
    )
    
    output_aspect <- raster::terrain(
      output, 
      opt = 'aspect', 
      unit = slope_units, 
      neighbors = slope_nn
    )
    
    output <- raster::stack(output, output_slope, output_aspect)
  }
  return(output)
}
  

  