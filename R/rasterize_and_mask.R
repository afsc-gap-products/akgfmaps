#' Convert spatial data to raster and mask using a shapefile
#' 
#' Function converts gridded spatial data with coordinates (eg. SpatialGrid) to a raster, then applies a spatial mask.
#' 
#' @param sgrid Gridded spatial data
#' @param amask Object to use as an area mask (simple features object or one of many sp objects)
#' @param touches Passed to terra::mask(). Must be set to FALSE to replicate raster::mask()
#' @export

rasterize_and_mask <- function(sgrid, amask, touches = FALSE) {
  
  if(any(c("RasterLayer", "RasterBrick") %in% class(sgrid))) {
    return(raster::mask(sgrid, amask))
  }
  
  if(!(class(sgrid)[1] %in% c("SpatRaster", "SpatRasterDataset", "SpatRasterCollection"))) {
    sgrid <- terra::rast(sgrid)
  }
  return(terra::mask(sgrid, amask, touches = touches))
  
}