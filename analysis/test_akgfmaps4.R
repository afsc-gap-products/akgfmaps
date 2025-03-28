library(akgfmaps)

make_map <- function(x) {

  p1 <- ggplot() +
    geom_sf(data = x$akland) +
    geom_sf(data = x$survey.area, fill = NA) +
    geom_sf(data = x$bathymetry) +
    geom_sf(data = x$survey.grid, fill = NA) +
    geom_sf(data = x$survey.strata, fill = NA, linetype = 2, color = "red") +
    geom_sf(data = x$graticule) +
    coord_sf(xlim = x$plot.boundary$x,
             ylim = x$plot.boundary$y) +
    theme_bw() +
    theme(legend.title = element_blank())

  return(p1)
}

sebs_layers <- akgfmaps::get_base_layers(select.region = "sebs", set.crs = "EPSG:3338")
goa_layers <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "EPSG:3338")
ai_layers <- akgfmaps::get_base_layers(select.region = "ai", set.crs = "EPSG:3338")
nbs_layers <- akgfmaps::get_base_layers(select.region = "nbs", set.crs = "EPSG:3338")
slope_layers <- akgfmaps::get_base_layers(select.region = "ebs.slope", set.crs = "EPSG:3338")
bssa1_layers <- akgfmaps::get_base_layers(select.region = "bssa1", set.crs = "EPSG:3338")
wai_layers <- akgfmaps::get_base_layers(select.region = "ai.east", set.crs = "EPSG:3338")


make_map(x = sebs_layers)
make_map(x = nbs_layers)
make_map(x = slope_layers)
make_map(x = goa_layers)
make_map(x = ai_layers)
make_map(x = bssa1_layers)
make_map(x = wai_layers)

layers_1 <- akgfmaps::get_base_layers(select.region = "sebs", set.crs = "WGS84", split.land.at.180 = TRUE)

layers_2 <- akgfmaps::get_base_layers(select.region = "sebs", set.crs = "WGS84")

ggplot() +
  geom_sf(data = layers_1$akland)

ggplot() +
  geom_sf(data = layers_2$akland)

ggplot() +
  geom_sf(data = sf::st_shift_longitude(layers_2$akland))
