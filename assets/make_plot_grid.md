# Return coordinates for plotting gridded data using make_2d_grid()

This example show how to use the make_2d_grid() function to produce grid cell centroid
coordinates that facilitate plotting data from 2d grids that are used for spatial
analyses (e.g. using VAST or sdmTMB). Otherwise this would require post-processing 
manipulation of gridded data.

The plotting coordinate columns (lon_plot and lat_plot) should only be used for 
visualization.

``` r
library(akgfmaps)

ebs_layers <- akgfmaps::get_base_layers(select.region = "sebs",
                                        set.crs = "EPSG:3338")

ebs_grid <- akgfmaps:::make_2d_grid(obj = ebs_layers$survey.strata,
                                    resolution = res,
                                    bbox = vast_bbox,
                                    output_type = "point",
                                    include_tile_center = TRUE)

p1 <- ggplot() +
      geom_tile(data = ebs_grid,
                mapping = aes(x = lon_plot,
                              y = lat_plot,
                              fill = Stratum))
                
p1
```

![](/assets/ex_2d_grid_with_plot_centroids.png)
