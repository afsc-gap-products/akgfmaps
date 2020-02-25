#' Add labels to IDW plots
#' 
#' Select which place labels should be included on the map based on category and region. Currently supports regions: 'bs.all' and 'bs.south'
#' See data/placenames for the default list of locations.
#'   
#' @param in.dat A list containing the ggplot object returned by \code{make_idw_map()} or \code{change_fill_color}.
#' @param lab.select A vector indicating which types of labels to include on the map.
#' @param new.places A data frame containing new locations and labels to add to the map.
#' @param lab.replace Logical indicating whether new.places should be added to the default labels or replace the default labels.
#' @param transform.new.places Logical indicating whether the data frame passed to new.places should be transformed to match the map projection.
#' @param add.scale.bar A character vector indicating where to add the scale bar. "br" for bottom right, "tl" for top left, etc. Default = "br"
#' 
#' @return Returns input list with IDW plot labels added.
#' 
#' @author Sean Rohan \email{sean.rohan@@noaa.gov}

add_idw_plot_labels <- function(in.dat, lab.select = c("islands", "bathymetry", "convention line", "mainland", "peninsula"), 
                                new.places = NULL, 
                                lab.replace = FALSE,
                                transform.new.places = TRUE,
                                add.scale.bar = "br") {
  # Load labels and plotting locations
  placenames <- read.csv(file = system.file("data", 
                                            file = "placenames.csv", 
                                            package = "akgfmaps", 
                                            mustWork = TRUE), 
                         stringsAsFactors = FALSE) %>% 
    transform_data_frame_crs(out.crs = sf::st_crs(in.dat$extrapolation.grid)) # Transform placenames based on extrapolation grid CRS

  # Find new and default place names
  if(!is.null(new.places)) {
    if(!all(c("x", "y") %in% names(new.places))) {stop("new.places must include a longitude column named 'x' and latitude column named 'y'")}
    
    if(transform.new.places) {new.places <- new.places %>% transform_data_frame_crs(out.crs = sf::st_crs(in.dat$extrapolation.grid))}
    
    if(lab.replace) {
      placenames <- new.places
    } else {
      placenames <- dplyr::bind_rows(placenames, new.places)
    }
  }
  
  placenames <- subset(placenames, region == in.dat$region & type %in% lab.select)
  
  # Make plot
  if(in.dat$region %in% c("bs.all")) {
    in.dat$plot <- in.dat$plot + 
      geom_text(data = subset(placenames, type == "mainland"), aes(x = x, y = y, label = lab), size = 14, group = 99) +
      geom_shadowtext(data = subset(placenames, type == "peninsula"), 
                      aes(x = x, y = y, label = lab), size = 8, 
                      angle = 28, bg.color = "white", color = "black", group = 99) +
      geom_shadowtext(data = subset(placenames, type %in% c("bathymetry", "islands")), 
                      aes(x = x, y = y, label = lab), 
                      bg.color = "white", color = "black", size = 3.88, group = 99) +
      geom_shadowtext(data = subset(placenames, type == "convention line"), aes(x = x, y = y, label = lab), 
                      angle = 29.5, size = 3,
                      bg.color = "white", color = "black", group = 99)
  }
  
  if(!is.na(add.scale.bar)) {
    in.dat$plot <- in.dat$plot + ggspatial::annotation_scale(location = add.scale.bar)
  }
  return(in.dat)
}
