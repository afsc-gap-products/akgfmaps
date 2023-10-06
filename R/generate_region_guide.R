#' Generate blank region catalogue
#' 
#' Generates a pdf file with plots of layers returned by get_base_layers().
#' @param out_loc Filepath for output. Must be a pdf.
#' @param select.regions Which regions to include in the output? Default selects all supported regions.
#' @export

generate_region_guide <- function(out_loc = "akgfmaps_region_layers.pdf", 
                                  select.regions = NULL) {
  
  if(!is.null(select.regions)) {
    for(ii in 1:length(select_regions)) {
      .check_region(select.region = select.regions[ii], type = "survey")
    }
  }
  
  if(!grepl(".pdf", out_loc)) {
    stop("out_loc must be a .pdf file!")
  }
  
  if(is.null(select.regions)) {
    select.regions <- c(
      "bs.south", "sebs", "bs.all", "ebs", "bs.north", "nbs", "ecs", "ebs.ecs", "ai", "ai.west", 
      "ai.central", "ai.east", "goa", "goa.west", "goa.east", "ebs.slope", "bssa1", "bssa2",
      "bssa3", "bssa4", "bssa5", "bssa6")
  }
  grDevices::pdf(file = out_loc, width = 8, height = 8, onefile = TRUE)
  for(i in 1:length(select.regions)) {
    map_layers <- akgfmaps::get_base_layers(select.region = select.regions[i], set.crs = "auto")
    
    print(
      ggplot2::ggplot() +
        ggplot2::geom_sf(data = map_layers$bathymetry) +
        ggplot2::geom_sf(data = map_layers$akland) +
        ggplot2::geom_sf(data = map_layers$survey.area, fill = NA, color = "red", size = ggplot2::rel(1)) +
        ggplot2::geom_sf(data = map_layers$graticule, alpha = 0.3) +
        ggplot2::coord_sf(xlim = map_layers$plot.boundary$x,
                     ylim = map_layers$plot.boundary$y) +
        ggplot2::scale_x_continuous(breaks = map_layers$lon.breaks) +
        ggplot2::scale_y_continuous(breaks = map_layers$lat.breaks) +
        ggplot2::ggtitle(paste0("Region: ", select.regions[i], ", survey area")) +
        ggplot2::theme_bw()
    )
    
    print(
      ggplot2::ggplot() +
        ggplot2::geom_sf(data = map_layers$bathymetry) +
        ggplot2::geom_sf(data = map_layers$akland) +
        ggplot2::geom_sf(data = map_layers$survey.strata, fill = NA, color = "red", size = rel(1)) +
        ggplot2::geom_sf(data = map_layers$graticule, alpha = 0.3) +
        ggplot2::coord_sf(xlim = map_layers$plot.boundary$x,
                 ylim = map_layers$plot.boundary$y) +
        ggplot2::scale_x_continuous(breaks = map_layers$lon.breaks) +
        ggplot2::scale_y_continuous(breaks = map_layers$lat.breaks) +
        ggplot2::ggtitle(paste0("Region: ", select.regions[i], ", survey strata")) +
        ggplot2::theme_bw()
    )
  }
  grDevices::dev.off()
}