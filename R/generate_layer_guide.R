#' Generate a pdf file showing layers
#' 
#' Generates a pdf file named 'akgfmaps_layer_guide.pdf' with plots of layers, file names, and default coordinate reference systems for all of the shapefiles that are included in the package.
#' 
#' @param out.dir Output directory for the pdf. Defaults to working directory.

generate_layer_guide <- function(out.dir = "") {
  
  library(ggplot2)
  library(ggthemes)
  library(sf)
  
  out_loc <- paste0(out.dir, "akgfmaps_layer_guide.pdf")
  
  shp_dir <- dir(system.file("data", package = "akgfmaps"))
  shp_title <- shp_dir[grepl(".shp", shp_dir) & !grepl(".xml", shp_dir)]
  shp_dir <- shp_dir[grepl(".shp", shp_dir) & !grepl(".xml", shp_dir)]
  
  pdf(out_loc, onefile = TRUE, width = 8, height = 8)
  for(i in 1:length(shp_dir)) {
    shp_layer <- sf::st_read(system.file("data", shp_dir[i], package = "akgfmaps"), quiet = TRUE)
    
    print(ggplot() + geom_sf(data = shp_layer) + 
            theme_few() + 
            ggtitle(paste0("File: ", shp_dir[i], "; CRS: ", sf::st_crs(shp_layer))))
  }
  dev.off()
}