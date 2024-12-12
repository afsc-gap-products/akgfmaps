# Test longline survey strata

devtools::install_github("afsc-gap-products/akgfmaps@dev2")

library(akgfmaps)
library(ggthemes)

test_gbl <- function(select.region, set.crs, ...) {

  layers <- get_base_layers(select.region = select.region,
                            set.crs = set.crs,
                            ...)

  p1 <- ggplot() +
    geom_sf(data = layers$akland) +
    geom_sf(data = layers$survey.area,
            mapping = aes(color = "Survey area",
                          fill = "Survey area"),
            alpha = 0.7) +
    geom_sf(data = layers$survey.strata,
            mapping = aes(color = "Survey strata"),
            fill = NA) +
    geom_sf(data = layers$survey.grid,
            mapping = aes(color = "Survey grid")) +
    geom_sf(data = layers$graticule,
            alpha = 0.3) +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    scale_x_continuous(limits = layers$plot.boundary$x,
                       breaks = layers$lon.breaks) +
    scale_y_continuous(limits = layers$plot.boundary$y,
                       breaks = layers$lat.breaks) +
    theme_bw()

  return(list(plot = p1,
              layers = layers))

}


ll_ebs <- test_gbl(select.region = "ll.ebs", set.crs = "EPSG:3338")
ll_ebs$plot
ll_ebs$layers$survey.grid
ll_ebs$layers$survey.strata$TYPE
ll_ebs$layers$survey.grid$HABITAT_TYPE

head(ll_ebs$layers$survey.area)
head(ll_ebs$layers$survey.strata)
head(ll_ebs$layers$survey.grid)

ll_goa <- test_gbl(select.region = "ll.goa", set.crs = "EPSG:3338")
ll_goa$plot
ll_goa$layers$survey.grid
ll_goa$layers$survey.strata$TYPE
ll_goa$layers$survey.grid$HABITAT_TYPE

ll_ai_nad83 <- test_gbl(select.region = "ll.ai", set.crs = "EPSG:4269")
ll_ai_nad83$plot

ll_ai_aea <- test_gbl(select.region = "ll.ai", set.crs = "EPSG:3338")
ll_ai_aea$plot

ll_ai_west <- test_gbl(select.region = "ll.ai.west", set.crs = "EPSG:3338")
ll_ai_west$plot

ll_ai_central <- test_gbl(select.region = "ll.ai.central", set.crs = "EPSG:3338")
ll_ai_central$plot

ll_bssa1 <- test_gbl(select.region = "ll.bssa1", set.crs = "EPSG:3338")
ll_bssa1$plot

ll_bssa2 <- test_gbl(select.region = "ll.bssa2", set.crs = "EPSG:3338")
ll_bssa2$plot

ll_bssa3 <- test_gbl(select.region = "ll.bssa3", set.crs = "EPSG:3338")
ll_bssa3$plot

ll_bssa4 <- test_gbl(select.region = "ll.bssa4", set.crs = "EPSG:3338")
ll_bssa4$plot

ll_bssa5 <- test_gbl(select.region = "ll.bssa5", set.crs = "EPSG:3338")
ll_bssa5$plot
