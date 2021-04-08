#' Convert spatial data to raster and mask using a shapefile
#' 
#' Function converts gridded spatial data with coordinates (eg. SpatialGrid) to a raster, then applies a spatial mask.
#' 
#' @param sgrid Gridded spatial data
#' @param amask Object to use as an area mask (simple features object or one of many sp objects)

rasterize_and_mask <- function(sgrid, amask) {
  
  if(!(class(sgrid)[1] %in% c("RasterLayer", "RasterBrick"))) {
    sgrid <- raster::raster(sgrid)
  }
  
  return(raster::mask(sgrid, amask))
}