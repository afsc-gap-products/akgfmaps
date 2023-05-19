## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(akgfmaps)

SEBS <- akgfmaps::get_base_layers(select.region = "bs.south", set.crs = "auto")
class(SEBS)
names(SEBS)


## ----fig.width=8, fig.height = 6----------------------------------------------

(SEBS_plot <- ggplot() +
  geom_sf(data = SEBS$akland) +
  geom_sf(data = SEBS$bathymetry) +
  geom_sf(data = SEBS$survey.area, fill = NA) +
  geom_sf(data = SEBS$graticule, color = "grey70", alpha = 0.5) +
  coord_sf(xlim = SEBS$plot.boundary$x, 
           ylim = SEBS$plot.boundary$y) +
  scale_x_continuous(name = "Longitude", 
                     breaks = SEBS$lon.breaks) + 
  scale_y_continuous(name = "Latitude", 
                     breaks = SEBS$lat.breaks) + 
  theme_bw())


## ----fig.width=8, fig.height = 6----------------------------------------------
my_label <- data.frame(x = -165, y = 58, label = "Thar be\ndragons")

SEBS_plot + geom_text(data = my_label, 
                      mapping = aes(x = x, 
                                    y = y, 
                                    label = label), 
                      color = "red")


## ----fig.width=8, fig.height = 6----------------------------------------------
my_label <- data.frame(x = -165, y = 58, label = "Thar be\ndragons") |> 
  akgfmaps::transform_data_frame_crs(in.crs = "+proj=longlat", out.crs = SEBS$crs)

SEBS_plot + geom_text(data = my_label, 
                      mapping = aes(x = x, 
                                    y = y, 
                                    label = label), 
                      color = "red")


## ----fig.width=8, fig.height = 6----------------------------------------------
(SEBS_plot |> add_map_labels(region = "bs.south", new.places = SEBS$place.labels))


