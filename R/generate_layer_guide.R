#' Generate a pdf file showing layers
#' 
#' Generates a pdf file with plots of layers, file names, and default coordinate reference systems for all of the shapefiles that are included in the package.
#' 
#' @param out_loc Filepath for output. Must be a pdf.
#' @export

generate_layer_guide <- function(out_loc = "akgfmaps_layer_guide.pdf") {
  
  if(!grepl(".pdf", out_loc)) {
    stop("out_loc must be a .pdf file!")
  }
  
  shp_dir <- dir(system.file("extdata", package = "akgfmaps"))
  shp_title <- shp_dir[grepl(".shp", shp_dir) & !grepl(".xml", shp_dir)]
  shp_dir <- shp_dir[grepl(".shp", shp_dir) & !grepl(".xml", shp_dir)]
  
  grDevices::pdf(out_loc, onefile = TRUE, width = 8, height = 8)
  for(i in 1:length(shp_dir)) {
    shp_layer <- sf::st_read(system.file("extdata", shp_dir[i], package = "akgfmaps"), quiet = TRUE)
    
    print(ggplot2::ggplot() + 
            ggplot2::geom_sf(data = shp_layer) + 
            ggthemes::theme_few() + 
            ggplot2::ggtitle(paste0("File: ", shp_dir[i], "; CRS: ", sf::st_crs(shp_layer))))
  }
  grDevices::dev.off()
}