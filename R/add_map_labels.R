#' Add labels to IDW plots
#' 
#' Select which place labels should be included on the map based on category and region. Currently supports regions: 'bs.all' and 'bs.south'. See data/placenames for the default list of locations.
#'   
#' @param x A list containing the ggplot object returned by \code{make_idw_map()} or \code{change_fill_color()}. Alternatively, a gg object.
#' @param lab.select A vector indicating which types of labels to include on the map.
#' @param region Vector indicating the plotting region. Only specify if x is a gg object.
#' @param new.places A data frame containing new locations and labels to add to the map.
#' @param lab.replace Logical indicating whether new.places should be added to the default labels or replace the default labels.
#' @param transform.new.places Logical indicating whether the data frame passed to new.places should be transformed to match the map projection.
#' @param add.scale.bar A character vector indicating where to add the scale bar. "br" for bottom right, "tl" for top left, etc. Default = "br"
#' @return Returns input list with IDW plot labels added.
#' @author Sean Rohan \email{sean.rohan@@noaa.gov}
#' @export

add_map_labels <- function(x, 
                           lab.select = c("islands", "bathymetry", "convention line", "mainland", "peninsula"), 
                           region = NA, 
                           new.places = NULL, 
                           lab.replace = FALSE, 
                           transform.new.places = TRUE, 
                           add.scale.bar = "br") {
  
  if("list" %in% class(x)) {
    in.dat <- x
    
    # Load labels and plotting locations--------------------------------------------------------------
    placenames <- utils::read.csv(file = system.file("data", 
                                              file = "placenames.csv", 
                                              package = "akgfmaps", 
                                              mustWork = TRUE), 
                           stringsAsFactors = FALSE) %>% 
      akgfmaps::transform_data_frame_crs(out.crs = sf::st_crs(in.dat$crs)) # Transform placenames based on extrapolation grid CRS
    
    # Find new and default place names----------------------------------------------------------------
    if(!is.null(new.places)) {
      if(!all(c("x", "y") %in% names(new.places))) {stop("new.places must include a longitude column named 'x' and latitude column named 'y'")}
      
      if(transform.new.places) {new.places <- new.places %>% 
        transform_data_frame_crs(out.crs = sf::st_crs(in.dat$crs))}
      
      if(lab.replace) {
        placenames <- new.places
      } else {
        placenames <- dplyr::bind_rows(placenames, new.places)
      }
    }
    
    placenames <- subset(placenames, region == in.dat$region & type %in% lab.select)
    
    # Make plot
    if(in.dat$region %in% c("bs.all", "ebs")) {
      in.dat$plot <- in.dat$plot + 
        ggplot2::geom_text(data = subset(placenames, type == "mainland"), 
                           aes(x = x, y = y, label = lab), 
                           size = 14, 
                           group = 99) +
        shadowtext::geom_shadowtext(data = subset(placenames, type == "peninsula"), 
                        aes(x = x, y = y, label = lab), 
                        size = 8, 
                        angle = 28, 
                        bg.color = "white", 
                        color = "black", 
                        group = 99) +
        shadowtext::geom_shadowtext(data = subset(placenames, type %in% c("bathymetry", "islands")), 
                        aes(x = x, 
                            y = y, 
                            label = lab), 
                        bg.color = "white", color = "black", size = 3.88, group = 99) +
        shadowtext::geom_shadowtext(data = subset(placenames, type == "convention line"), 
                                    aes(x = x, y = y, label = lab), 
                                    angle = 29.5, 
                                    size = 3,
                                    bg.color = "white", 
                                    color = "black", 
                                    group = 99)
    }
    
    if(!is.na(add.scale.bar)) {
      in.dat$plot <- in.dat$plot + ggspatial::annotation_scale(location = add.scale.bar)
    }
  } else if("gg" %in% class(x)){
    # Add labels to ggplot object ------------------------------------------------------------------
    placenames <- subset(new.places, region == region & type %in% lab.select)
    if(region %in% c("bs.all", "bs.south", "sebs")) {
      in.dat <- x + 
        ggplot2::geom_text(data = subset(placenames, type == "mainland"), 
                           aes(x = x, y = y, label = lab), 
                           size = 14, 
                           group = 99) +
        shadowtext::geom_shadowtext(data = subset(placenames, type == "peninsula"), 
                                    aes(x = x, y = y, label = lab), 
                                    size = 8, 
                                    angle = 40, 
                                    bg.color = "white", 
                                    color = "black", 
                                    group = 99) +
        shadowtext::geom_shadowtext(data = subset(placenames, type %in% c("bathymetry", "islands")), 
                                    aes(x = x, y = y, label = lab), 
                                    bg.color = "white", 
                                    color = "black", 
                                    size = 3.88, 
                                    group = 99) +
        shadowtext::geom_shadowtext(data = subset(placenames, type == "convention line"), 
                                    aes(x = x, y = y, label = lab), 
                                    angle = 42, 
                                    size = 3,
                                    bg.color = "white", 
                                    color = "black", 
                                    group = 99)
    }
  }
  return(in.dat)
}
