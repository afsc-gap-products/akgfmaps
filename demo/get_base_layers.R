# Created by Sean Rohan on November 24, 2023

library(akgfmaps)

# Get layers for the EBS survey area with Alaska Albers Equal area projection (EPSG:3338). Refer to function documentation for valid survey regions.
map_layers <- akgfmaps::get_base_layers(select.region = "bs.south", 
                                  set.crs = "EPSG:3338")

(map_layers_plot <- ggplot() +
   geom_sf(data = map_layers$akland) +
   geom_sf(data = map_layers$bathymetry) +
   geom_sf(data = map_layers$survey.area, fill = NA) +
   geom_sf(data = map_layers$graticule, color = "grey70", alpha = 0.5) +
   coord_sf(xlim = map_layers$plot.boundary$x, 
            ylim = map_layers$plot.boundary$y) +
   scale_x_continuous(name = "Longitude", 
                      breaks = map_layers$lon.breaks) + 
   scale_y_continuous(name = "Latitude", 
                      breaks = map_layers$lat.breaks) + 
   theme_bw())


## Add a label at 58 N, -165 W by specifying coordinates and converting to
# Alaska AEA projection
my_label <- data.frame(x = -165, 
                       y = 58, 
                       label = "Thar be\ndragons") |> 
  akgfmaps::transform_data_frame_crs(in.crs = "+proj=longlat", 
                                     out.crs = map_layers$crs)

map_layers_plot + 
  geom_text(data = my_label, 
            mapping = aes(x = x, 
                          y = y, 
                          label = label), 
            color = "red")