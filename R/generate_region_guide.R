#' Generate blank region catalogue
#' 
#' Generates a pdf file with plots of layers returned by get_base_layers().
#' @param out_loc Filepath for output. Must be a pdf.
#' @param select.regions Which regions to include in the output? Default selects all supported regions.
#' @export

generate_region_guide <- function(out_loc = "akgfmaps_region_layers.pdf", 
                                  select.regions = NA) {
  
  if(!grepl(".pdf", out_loc)) {
    stop("out_loc must be a .pdf file!")
  }
  
  if(is.na(select.regions)) {
    select.regions <- c("ecs", "ebs", "ebs.ecs", "sebs", "ai", "ai.west", "ai.central", "ai.east", "goa", "goa.west", "goa.east")
  }
  pdf(file = out_loc, width = 8, height = 8, onefile = TRUE)
  for(i in 1:length(select.regions)) {
    map_layers <- akgfmaps::get_base_layers(select.region = select.regions[i], set.crs = "auto")
    
    print(
      ggplot() +
            geom_sf(data = map_layers$bathymetry) +
            geom_sf(data = map_layers$akland) +
            geom_sf(data = map_layers$survey.area, fill = NA, color = "red", size = rel(1)) +
            geom_sf(data = map_layers$graticule, alpha = 0.3) +
            coord_sf(xlim = map_layers$plot.boundary$x,
                     ylim = map_layers$plot.boundary$y) +
        scale_x_continuous(breaks = map_layers$lon.breaks) +
        scale_y_continuous(breaks = map_layers$lat.breaks) +
        ggtitle(paste0("Region: ", select.regions[i], ", survey area")) +
        theme_bw()
    )
    
    print(
      ggplot() +
        geom_sf(data = map_layers$bathymetry) +
        geom_sf(data = map_layers$akland) +
        geom_sf(data = map_layers$survey.strata, fill = NA, color = "red", size = rel(1)) +
        geom_sf(data = map_layers$graticule, alpha = 0.3) +
        coord_sf(xlim = map_layers$plot.boundary$x,
                 ylim = map_layers$plot.boundary$y) +
        scale_x_continuous(breaks = map_layers$lon.breaks) +
        scale_y_continuous(breaks = map_layers$lat.breaks) +
        ggtitle(paste0("Region: ", select.regions[i], ", survey strata")) +
        theme_bw()
    )
  }
  dev.off()
}