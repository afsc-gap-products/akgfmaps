#' Algorithmic options for number of breaks to use for plotting
#' 
#' Function to aid in determining what breaks to use for plotting. Wrapper function for \code{classint::classIntervals}.
#' 
#' @param CPUE Vector of CPUE data
#' @param n.breaks Number of breaks to use
#' @param styles Character vector with names, or names of methods to try.
#' @return Returns a data frame showing breaks for each style.
#' @author Sean Rohan \email{sean.rohan@@noaa.gov}
#' @export

eval_plot_breaks <- function(CPUE, 
                             n.breaks, 
                             styles = c("equal", "pretty", "quantile", "kmeans", "hclust", "fisher", "jenks", "dpih")) {
  eval.style <- data.frame(style = styles)
  
  eval.style.mat <- matrix(ncol = n.breaks+1, nrow = nrow(eval.style))
  eval.style <- cbind(eval.style, as.data.frame(eval.style.mat))
  
  for(i in 1:nrow(eval.style)) {
    eval.style[i, c(2:(n.breaks+2))] <- round(classInt::classIntervals(CPUE, n = n.breaks, style = eval.style$style[i])$brks)
  }
  
  
  eval.style.plot <- reshape::melt(eval.style)
  eval.style.plot$variable <- sub("V", "", eval.style.plot$variable)
  
  return(eval.style)
}
