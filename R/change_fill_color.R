#' Change color scale on plots
#' 
#' Function to quickly change color palette for plots
#' 
#' @param x A list containing the ggplot object returned by \code{make_idw_map()}
#' @param new.scheme Fill scale to use for plotting. Can pass a palette. Default = NA opens colorspace selector
#' @param ... Pass additional arguments to ggplot2::scale_fill_xx functions.
#' @return Returns a ggplot object with the fill scale replaced
#' @author Sean Rohan \email{sean.rohan@@noaa.gov}
#' @export

change_fill_color <- function(x, 
                              new.scheme, 
                              show.plot = TRUE, ...) {
  in.dat <- x
  # Extratc legend title
  key.title <- in.dat$plot$scales$scales[[1]]$name
  if(is.na(new.scheme)) {
    new.pal <- colorspace::choose_palette()
    new.scheme <- new.pal(n = in.dat$n.breaks)
  }
  if(new.scheme %in% c("grey", "gray")) {
    in.dat$plot <- in.dat$plot + scale_fill_grey(name = key.title, start = 1, end = 0, na.translate = FALSE, drop = FALSE)
  } else {
    if(new.scheme == "blue") {new.scheme <- colorspace::sequential_hcl(n=in.dat$n.breaks, h1=260, c1=80, l1=30, l2=100, p1=1.5)}
    if(new.scheme == "purple") {new.scheme <- colorspace::sequential_hcl(n=in.dat$n.breaks, h1=270, c1=70, l1=25, l2=100, p1=1.3)}
    if(new.scheme == "red") {new.scheme <- colorspace::sequential_hcl(n=in.dat$n.breaks, h1=10, c1=85, l1=25, l2=100, p1=1.3)}
    if(new.scheme == "green") {new.scheme <- colorspace::sequential_hcl(n=in.dat$n.breaks, h1=135, c1=45, l1=35, l2=100, p1=1.3)}
    if(new.scheme == "aqua") {new.scheme <- colorspace::sequential_hcl(n=in.dat$n.breaks, h1=-168, c1=45, l1=45, l2=100, p1=1.3)}
    if(new.scheme == "magenta") {new.scheme <- colorspace::sequential_hcl(n=in.dat$n.breaks, h1=-37, c1=78, l1=36, l2=100, p1=1.3)}
    if(new.scheme == "tan") {new.scheme <- colorspace::sequential_hcl(n=in.dat$n.breaks, h1=60, c1=68, l1=52, l2=100, p1=1.3)}
    if(new.scheme == "blue2") {new.scheme <- colorspace::sequential_hcl(n=in.dat$n.breaks, h1=260, h2=220, c1=45, c2=5, cmax=180, l1=25, l2=100, p1=1.2, p2=1.3)}
    if(new.scheme == "green2") {new.scheme <- colorspace::sequential_hcl(n=in.dat$n.breaks, h1=125, h2=200, c1=30, c2=5, cmax=180, l1=25, l2=100, p1=1.4, p2=1.6)}
    if(new.scheme == "purple2") {new.scheme <- colorspace::sequential_hcl(n=in.dat$n.breaks, h1=275, h2=270, c1=55, c2=5, cmax=180, l1=20, l2=100, p1=1.3, p2=1.3)}
    if(new.scheme == "red2") {new.scheme <- colorspace::sequential_hcl(n=in.dat$n.breaks, h1=0, h2=35, c1=65, c2=5, cmax=180, l1=20, l2=100, p1=1.1, p2=1.3)}
    in.dat$plot <- in.dat$plot + scale_fill_manual(name = key.title, values = rev(new.scheme), na.translate = FALSE, drop = FALSE)
  }


    if(show.plot) {
      print(in.dat$plot)
    }
    
    return(in.dat)
  }