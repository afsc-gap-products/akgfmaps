#' Create map file
#' 
#' Creates a .png map with the assigned file prefix in the designated file path (i.e. directory)
#' 
#' @param in.dat List containing ggplot object
#' @param file.prefix Character vector indicating what name should precede the file name. If not provided, uses \code{key.title} from the output of \code{make_idw_map}
#' @param file.path File path where the png should be saved. By default, Saves to working directory.
#' @param try.change_text_size Logical indicating whether labels of the input list should be resized using predefined scaling factors.
#' @param width Width of the .png. Passed to \code{png}.
#' @param height Height of the .png. Passed to \code{png}.
#' @param units Units of width and height. Passed to \code{png}.
#' @param res Resolution of the output. Passed to \code{png}.
#' @param bg Color of the background. Passed to \code{png}.


create_map_file <- function(in.dat,
                            file.prefix = NA, 
                            file.path = NA, 
                            try.change_text_size = FALSE,
                            width = 12, 
                            height = 9, 
                            units = "in", 
                            res = 300, 
                            bg = "transparent") {
  
  size.changed <- FALSE
  # Create file.prefix if not provided
  if(is.na(file.prefix)) {
    file.prefix <- in.dat$key.title
  }
  
  # Empty file.path if not provided
  if(is.na(file.path)) {
    file.path <- ""
  }
  
  # Rescale text for predefined sizes
  if(try.change_text_size) {
    if(width == 12 & height == 9 & units == "in") {in.dat <- change_text_size(in.dat = in.dat, size.mult = 1.2); size.changed <- TRUE}
    if(width == 8 & height == 6 & units == "in") {in.dat <- change_text_size(in.dat = in.dat, size.mult = 0.8); size.changed <- TRUE}
  } else {
    size.changed <- TRUE
  }
  
  # Did the size change
  if(!size.changed) {
    stop("try.change_text_size failed. Size change for provided dimensions (width, height, units) is not defined.")
  }
  
  print(paste("Creating map", paste0(file.path, file.prefix, "_", width, height, ".png" )))
  
  png(file = paste0(file.path, file.prefix, "_", width, "x", height, ".png" ), 
      width = width, 
      height = height, 
      units = units, 
      res = res, 
      bg = bg)
  print(in.dat$plot)
  dev.off()
}
