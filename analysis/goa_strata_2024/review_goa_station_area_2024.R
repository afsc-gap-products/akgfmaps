# Plot data after updating/installing the package

library(akgfmaps)

# 1984
goa_1984 <- get_base_layers(select.region = "goa", design.year = 1984, set.crs = 3338)

# 2024
goa_2024 <- get_base_layers(select.region = "goa", design.year = 2024, set.crs = 3338)

# 2025
goa_2025 <- get_base_layers(select.region = "goa", design.year = 2025, set.crs = 3338)

make_plot <- function(x) {

  p1 <-
    ggplot() +
    geom_sf(data = x$survey.grid, fill = NA, linewidth = 0.1) +
    geom_sf(data = x$graticule, alpha = 0.5) +
    scale_x_continuous(limits = x$plot.boundary$x,
                       breaks = x$lon.breaks) +
    scale_y_continuous(limits = x$plot.boundary$y,
                       breaks = x$lat.breaks) +
    theme_bw() +
    theme(panel.grid = element_blank())

  p2 <-
    ggplot() +
    geom_sf(data = x$survey.strata, mapping = aes(fill = factor(STRATUM)), color = NA) +
    geom_sf(data = x$graticule, alpha = 0.5) +
    scale_x_continuous(limits = x$plot.boundary$x,
                       breaks = x$lon.breaks) +
    scale_y_continuous(limits = x$plot.boundary$y,
                       breaks = x$lat.breaks) +
    theme_bw() +
    scale_fill_viridis_d(option = "H") +
    theme(panel.grid = element_blank(),
          legend.position = "none")

  p3 <-
    ggplot() +
    geom_sf(data = x$survey.area, fill = NA) +
    geom_sf(data = x$bathymetry,
            mapping = aes(color = factor(DEPTH_M))) +
    geom_sf(data = x$graticule, alpha = 0.5) +
    scale_x_continuous(limits = x$plot.boundary$x,
                       breaks = x$lon.breaks) +
    scale_y_continuous(limits = x$plot.boundary$y,
                       breaks = x$lat.breaks) +
    scale_colour_viridis_d() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "inside",
          legend.direction = "horizontal",
          legend.position.inside = c(0.5, 0.1),
          legend.title = element_blank())

  return(cowplot::plot_grid(p1, p2, p3, nrow = 3))

}

make_plot(goa_1984)

make_plot(goa_2024)

make_plot(goa_2025)
