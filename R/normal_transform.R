#' Normal score transform
#' 
#' Normal score transformation of a vector to mean zero, variance one. 
#' 
#' @param x Numeric vector of values to transform
#' @return A list containing a vector of Z scores and a data frame containing the sorted vector and associated Z-scores.
#' @export

normal_transform <- function(x) {
  z_score <- qqnorm(x, plot.it = FALSE)$x
  transform_df <- data.frame(x = sort(x),
                             z = sort(z_score))
  return(list(z_score = z_score, 
              transform_df = transform_df))
}