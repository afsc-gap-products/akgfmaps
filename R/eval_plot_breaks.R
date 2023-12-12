#' Algorithmic options for number of breaks to use for plotting
#'
#' Function to aid in determining what breaks to use for plotting. Wrapper function for \code{classint::classIntervals}.
#'
#' @param CPUE Vector of CPUE data
#' @param n.breaks Number of breaks to use
#' @param styles Character vector with names, or names of methods to try.
#' @param log.scale.plot If TRUE, CPUE values are transformed as log10(CPUE+1) for plotting. Default = FALSE
#' @return Returns a data frame showing breaks for each style.
#' @author Sean Rohan \email{sean.rohan@@noaa.gov}
#' @export
#' @importFrom stats reshape

eval_plot_breaks <- function(CPUE,
                             n.breaks,
                             styles = c("equal", "pretty", "quantile", "kmeans", "hclust", "fisher", "jenks", "dpih"),
                             log.scale.plot = FALSE) {

  eval_style <- data.frame(style = styles)

  eval_style.mat <- matrix(ncol = n.breaks+1, nrow = nrow(eval_style))
  eval_style <- cbind(eval_style, as.data.frame(eval_style.mat))

  for(i in 1:nrow(eval_style)) {
    eval_style[i, c(2:(n.breaks+2))] <- round(
      classInt::classIntervals(CPUE,
                               n = n.breaks,
                               style = eval_style$style[i])$brks
      )
  }

  brks_long <- stats::reshape(
    data = eval_style,
    varying = list(names(eval_style)[2:ncol(eval_style)]),
    times = names(eval_style)[2:ncol(eval_style)],
    idvar = "style",
    v.names = "value",
    timevar = "name",
    direction = "long"
  )

  n_panels <- min(c(length(styles), 3))

  if(log.scale.plot) {
   CPUE <- log10(CPUE+1)
   brks_long$value <- log10(brks_long$value+1)
   x_label <- "log10(CPUE+1)"
  } else {
    x_label <- "CPUE"
  }

  print(eval_style)

  CPUE <- CPUE[CPUE > 0]

  cpue_df <- as.data.frame(CPUE)

  print(ggplot() +
          stat_ecdf(data = cpue_df,
                    mapping = aes(x = CPUE)) +
          geom_vline(data = brks_long,
                     mapping = aes(xintercept = value,
                                   color = name)) +
          facet_wrap(~style, ncol = n_panels) +
          scale_x_continuous(name = x_label) +
          scale_y_continuous(name = "ECDF (non-zero values)") +
          scale_color_viridis_d(name = "Break", option = "H") +
          theme_bw())

  readline(prompt = "Press 'Enter' to view next plot")

  print(ggplot() +
          geom_density(data = cpue_df,
                    mapping = aes(x = CPUE)) +
          geom_vline(data = brks_long,
                     mapping = aes(xintercept = value,
                                   color = name)) +
          facet_wrap(~style, ncol = n_panels) +
          scale_x_continuous(name = x_label) +
          scale_y_continuous(name = "Density (non-zero values)") +
          scale_color_viridis_d(name = "Break", option = "H") +
          theme_bw())

  return(eval_style)
}
