#' Generate a pdf file showing layers
#' 
#' Generates a pdf file with plots of layers, file names, and default coordinate reference systems for all of the shapefiles that are included in the package.
#' 
#' @param out_loc Filepath for output. Must be a pdf.

generate_layer_guide <- function(out_loc = "akgfmaps_layer_guide.pdf") {
  
  library(ggplot2)
  library(ggthemes)
  library(sf)
  
  if(!grepl(".pdf", out_loc)) {
    stop("out_loc must be a .pdf file!")
  }
  
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