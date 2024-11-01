#' Reproject a GEBCO raster to bathymetry, slope, and aspect rasters
#'
#' Reprojects GEBCO raster to a new extent, resolution, and coordinate reference system. Option to also calculate slope and aspect from the rasters.
#'
#' @param x Either a filepath to a GEBCO netCDF bathymetry file or a raster object.
#' @param z_varname Name of the variable name for elevation/depth in the GEBCO raster.
#' @param z_direction Direction for depth (negative one if positive depth is down)
#' @param raster_template A terra SpatRaster that is used as the template for projecting the gebco_raster. In other words, the GEBCO bathymetry will be projected to the extent, resolution, and coordinate reference system of raster_template. Must provide raster_template OR extent, resolution, and CRS.
#' @param extent Extent of for a raster_template as an sf object or 4L named numeric vector with names xmin, xmax, ymin, ymax (e.g. extent = c(xmn = -2075672, xmx = -149672.2, ymn = 427321.3, ymx = 2027321)). Must be provided if raster_template is not provided.
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

  # Create raster template based on the Bounding Box of an sf object
  if(is(extent, "sf")) {

    stopifnot("gebco_to_bs: Resolution argument must be a numeric vector of length 2 denoting the x and y cell dimensions if extent is an sf." = length(resolution) %in% c(1,2))

    # Add 10 km buffer
    bbox <- sf::st_bbox(sf::st_buffer(extent, dist = 1e4))

    if(is.null(set_crs)) {
      set_crs <- sf::st_crs(extent)
    }

    raster_template <- terra::rast(xmin = bbox['xmin'],
                                   xmax = bbox['xmax'],
                                   ymin = bbox['ymin'],
                                   ymax = bbox['ymax'],
                                   res = resolution,
                                   crs = terra::crs(extent))

  }

  if(is.null(raster_template)) {

    stopifnot("gebco_to_bs: Extent argument must be a named numeric vector of length 4 when template_raster is not provided and extent is not an sf class." = length(extent) == 4)

    stopifnot("gebco_to_bs: Resolution argument must be a numeric vector of length 2 denoting the x and y cell dimensions if template_raster is not provided." = length(resolution) == 2)

    stopifnot("gebco_to_bs: If template_raster is not provided, extent argument must be a 4L numeric vector with names xmn, xmx, ymn, ymx" = all(c("xmn", "xmx", "ymn", "ymx") %in% extent))

    raster_template <- terra::rast(xmin = extent['xmin'],
                                   xmax = extent['xmax'],
                                   ymin = extent['ymin'],
                                   ymax = extent['ymax'],
                                   res = resolution,
                                   crs = set_crs)

    raster_template <- terra::project(from = raster_template,
                                      crs = set_crs)

  }

  stopifnot("gebco_to_bs: raster_template must be a raster or terra object." = class(raster_template) %in% c("SpatRaster", "SpatVector"))

  # Load GEBCO raster if a filepath was provided
  if(class(x) == "character") {
    x <- terra::rast(x,
                     lyrs = z_varname) # Elevation (up = positive)

    terra::crs(x) <- "EPSG:4326"

  }

  x_ext <- as.list(terra::ext(x))

  if(any(c(x_ext[['xmin']], x_ext[['xmax']]) > 180)) {
    message("reproject_gebco: Raster longitudes greater than 180 degrees detected. Rotating raster from 0 to 360 coordinates to -180 to 180 coordinates.")
    x <- terra::rotate(x)
  }


  # Project GEBCO to raster template extent, resolution, and coordinate reference system
  output <- terra::project(x = x,
                           y = raster_template,
                           method = interpolation_method)
  output <- output * z_direction
  names(output) <- "seafloor_depth"

  if(return_slope_aspect) {

    output_slope <- terra::terrain(
      output,
      v = 'slope',
      unit = slope_units,
      neighbors = slope_nn
    )

    output_aspect <- raster::terrain(
      output,
      v = 'aspect',
      unit = slope_units,
      neighbors = slope_nn
    )

    output <- c(output, output_slope, output_aspect)
  }
  return(output)
}


