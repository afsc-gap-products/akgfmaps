# coldpool data sets
# Sean Rohan <sean.rohan@noaa.gov>
# Survey-Centric R Users Group
# February 27, 2024
# https://github.com/afsc-gap-products/coldpool
# coldpool version 3.3-1


# Built-in data sets in coldpool
library(coldpool)

# Cold pool index, mean bottom temperature, etc.
head(coldpool::cold_pool_index)

?cold_pool_index

# SEBS bottom temperature as a SpatRast
ebs_bt <- terra::unwrap(coldpool::ebs_bottom_temperature)

plot(ebs_bt[[1:6]])

# EBS + NBS bottom temperature as a SpatRast
ebs_nbs_bt <- terra::unwrap(coldpool::nbs_ebs_bottom_temperature)

plot(ebs_nbs_bt)

# Calculating mean bottom temperature in outer shelf strata
map_layers <- akgfmaps::get_base_layers(select.region = "ebs", set.crs = "EPSG:3338")

outer_shelf <- map_layers$survey.strata |>
  dplyr::filter(Stratum %in% c(50, 61, 62, 90))

outer_shelf_bt <- terra::mask(ebs_nbs_bt, outer_shelf) 

terra::global(outer_shelf_bt, "mean", na.rm = TRUE)


# Plotting outer shelf bottom temperature
outer_shelf_bt_tile <- outer_shelf_bt |>
  as.data.frame(xy = TRUE) |>
  tidyr::pivot_longer(cols = 3:9, names_to = "year", values_to = "bt")

ggplot() +
  geom_tile(data = outer_shelf_bt_tile,
            mapping = aes(x = x, y = y, fill = bt)) +
  geom_sf(data = map_layers$akland) +
  geom_sf(data = map_layers$survey.strata, fill = NA) +
  scale_x_continuous(limits = map_layers$plot.boundary$x, 
                     breaks = map_layers$lon.breaks) +
  scale_y_continuous(limits = map_layers$plot.boundary$y, 
                     breaks = map_layers$lat.breaks) +
  scale_fill_viridis_c(name = expression('BT'~(degree*C)), option = "H") +
  facet_wrap(~year) +
  theme_bw() +
  theme(axis.title = element_blank())

