# Test akgfmaps V4 functions

library(akgfmaps)
library(ggthemes)

test_gbl_v4 <- function(select.region, set.crs, ...) {

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
            mapping = aes(color = "Survey grid"),
            fill = NA) +
    geom_sf(data = layers$graticule,
            alpha = 0.3) +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    scale_x_continuous(limits = layers$plot.boundary$x,
                       breaks = layers$lon.breaks) +
    scale_y_continuous(limits = layers$plot.boundary$y,
                       breaks = layers$lat.breaks) +
    theme_bw()

  p_stratum <- ggplot() +
    geom_sf(data = layers$akland) +
    geom_sf(data = layers$survey.strata,
            mapping = aes(fill = factor(STRATUM)),
            color = NA) +
    geom_sf(data = layers$graticule,
            alpha = 0.3) +
    scale_fill_viridis_d() +
    scale_color_brewer() +
    scale_x_continuous(limits = layers$plot.boundary$x,
                       breaks = layers$lon.breaks) +
    scale_y_continuous(limits = layers$plot.boundary$y,
                       breaks = layers$lat.breaks) +
    theme_bw()

  return(list(plot_all = p1,
              plot_stratum = p_stratum,
              layers = layers))

}

goa_layers <- test_gbl_v4(select.region = "goa", set.crs = "EPSG:3338")
goa_layers$plot_all
goa_layers$plot_stratum

goa_layers_1984 <- test_gbl_v4(select.region = "goa", set.crs = "EPSG:3338", design.year = 1984)
goa_layers_1984$plot_all
goa_layers_1984$plot_stratum

ai_layers <- test_gbl_v4(select.region = "ai", set.crs = "EPSG:3338")
ai_layers$plot
ai_layers$plot_all

ai_sebs_layers <- test_gbl_v4(select.region = c("ai", "sebs"), set.crs = "EPSG:3338")
ai_sebs_layers$plot

ai_sebs_layers <- test_gbl_v4(select.region = c("ai", "sebs", "goa"), set.crs = "EPSG:3338")
ai_sebs_layers$plot

ebs_layers <- test_gbl_v4(select.region = "ebs", set.crs = "EPSG:3338")
ebs_layers$plot

ai_west_layers <- test_gbl_v4(select.region = c("ai.west", "ai.central"), set.crs = "EPSG:3338")
ai_west_layers$plot

ai_west_layers$p2

ai_goa_layers <- test_gbl_v4(select.region = c("ai.east", "goa.west"), set.crs = "EPSG:3338")

ai_goa_layers$p2

ggplot() +
  geom_sf(data = ai_goa_layers$layers$inpfc.strata,
          mapping = aes(fill = INPFC_STRATUM)) +
  coord_sf(xlim = ai_goa_layers$layers$plot.boundary$x,
           ylim = ai_goa_layers$layers$plot.boundary$y)
