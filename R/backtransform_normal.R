#' Backtransformation of normal score transform
#' 
#' Backtransformation of normal score transform.
#' 
#' @param scores Numeric vector of scores to be transformed
#' @param z_score List returned by akgfmaps::normal_transform().
#' @param tails Character vector indicating which transformation/extrapoltion method to use. "none" (default): No extrapolation. Scores outside the original range get transformed to min or max.; "equal": Calculate magnitude of standard deviations around the original mean, and extrapolate linearly.; "separate": Calculates a new standard deviation for values above and below the mean, then extrpolate.
#' @export

backtransform_normal <- function(scores, 
                                 z_score, 
                                 tails = "none", 
                                 make_plot = TRUE) {
  
  # No extrapolation
  if(tails == 'none') {   
    x_min <- min(z_score$transform_df$x)
    x_max <- max(z_score$transform_df$x)
  }
  
  # Separate extrapolation for tails
  if(tails == "separate") { 
    x_mean <- mean(z_score$transform_df$x)
    x_small <- z_score$transform_df$x < x_mean
    x_large <- z_score$transform_df$x > x_mean
    sd_small <- sqrt(sum((z_score$transform_df$x[x_small]-x_mean)^2)/
                       (length(z_score$transform_df$x[x_small])-1))
    sd_large <- sqrt(sum((z_score$transform_df$x[x_large]-x_mean)^2)/
                       (length(z_score$transform_df$x[x_large])-1))
    x_min <- mean(z_score$transform_df$x) + (min(scores) * sd_small)
    x_max <- mean(z_score$transform_df$x) + (max(scores) * sd_large)
    
    if(x_min > min(z_score$transform_df$x)) {
      x_min <- min(z_score$transform_df$x)
    }
    if(x_max < max(z_score$transform_df$x)) {
      x_max <- max(z_score$transform_df$x)
    }
  }
  
  # Extrapolate linearly
  if(tails == "equal") {
    x_mean <- mean(z_score$transform_df$x)
    sd_x <- sd(z_score$transform_df$x)
    x_min <- mean(z_score$transform_df$x) + (min(scores) * sd_x)
    x_max <- mean(z_score$transform_df$x) + (max(scores) * sd_x)
    
    if(x_min > min(z_score$transform_df$x)) {
      x_min <- min(z_score$transform_df$x)
    }
    if(x_max < max(z_score$transform_df$x)) {
      x_max <- max(z_score$transform_df$x)
    }
  }
  
  sc_min <- min(scores)
  sc_max <- max(scores)
  x <- c(x_min, z_score$transform_df$x, x_max)
  nsc <- c(sc_min, z_score$transform_df$z, sc_max)
  
  if(make_plot) {
    plot(nsc, x, main = "Transform Function")
  }
  
  # Estimate backtransformation function transform function
  backtrans <- approxfun(nsc,x) 
  val <- backtrans(scores)
  
  return(val)
}