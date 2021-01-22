#' Function to change the size of text in plots
#' 
#' @param x ggplot object that requires text resizing
#' @param size.mult Size multiplier for rescaling text.
#' @author Sean K. Rohan, \email{sean.rohan@@noaa.gov}
#' @export

change_text_size <- function(x, 
                             size.mult, 
                             scale.theme = 1.125) {
  
  in.dat <- x
  
  # Vector to store class categories
  geom.class <- vector(length = length(in.dat$plot$layers))
  
  # Index text layers
  for(i in 1:length(geom.class)) {
    geom.class[i] <- class(in.dat$plot$layers[[i]]$geom)[1] %in% c("GeomText", "GeomShadowText")
  }
  
  # Resize text layers
  for(i in 1:length(geom.class)) {
    if(geom.class[i]) {
      in.dat$plot$layers[[i]]$aes_params$size <- in.dat$plot$layers[[i]]$aes_params$size*size.mult
    }
  }
  
  in.dat$plot$theme$axis.text$size <- in.dat$plot$theme$axis.text$size * size.mult * scale.theme
  in.dat$plot$theme$legend.text$size <- in.dat$plot$theme$axis.text$size * size.mult * scale.theme
  in.dat$plot$theme$legend.title$size <- in.dat$plot$theme$axis.text$size * size.mult * scale.theme

  
  return(in.dat)
  
}