# Example: akgfmaps v3 layers using get_base_layers_v3()
# SCRUG: akgfmaps, January 22, 2025
# Created by Sean Rohan (GitHub: sean-rohan-noaa)

# Through the end of 2025, get_base_layers_v3() will provide access to the old interface and layers.
# The function is limited to the functionality and layers that were available in akgfmaps v 3.6.2
#
# Recommendation: Don't use this version unless you have to-- it won't be updated and will be phased
# out.

library(akgfmaps)

# Arguments differ between function versions
?get_base_layers
?get_base_layers_v3

# Retrieve and plot GOA and EBS/NBS strata
goa_1984_v3 <- akgfmaps::get_base_layers_v3(
  select.region = "goa",
  set.crs = "EPSG:3338",
  high.resolution.coast = FALSE
)

ebs_nbs_2024_v3 <- akgfmaps::get_base_layers_v3(
  select.region = "ebs",
  set.crs = "EPSG:3338",
  high.resolution.coast = FALSE
)

ggplot() +
  geom_sf(data = goa_1984_v3$akland) +
  geom_sf(data = goa_1984_v3$survey.strata,
          mapping = aes(
            fill = factor(STRATUM)
          )
  ) +
  scale_x_continuous(limits = goa_1984_v3$plot.boundary$x,
                     breaks = goa_1984_v3$lon.breaks) +
  scale_y_continuous(limits = goa_1984_v3$plot.boundary$y,
                     breaks = goa_1984_v3$lat.breaks) +
  theme_bw() +
  scale_fill_viridis_d(option = "H") +
  theme(legend.position = "none")

ggplot() +
  geom_sf(data = ebs_nbs_2024_v3$akland) +
  geom_sf(data = ebs_nbs_2024_v3$survey.strata,
          mapping = aes(
            fill = factor(Stratum)
          )
  ) +
  scale_x_continuous(limits = ebs_nbs_2024_v3$plot.boundary$x,
                     breaks = ebs_nbs_2024_v3$lon.breaks) +
  scale_y_continuous(limits = ebs_nbs_2024_v3$plot.boundary$y,
                     breaks = ebs_nbs_2024_v3$lat.breaks) +
  scale_fill_viridis_d(option = "H") +
  theme_bw() +
  theme(legend.position = "none")

# Note that the field names do not match within or between regions.
head(goa_1984_v3$survey.area)
head(goa_1984_v3$survey.strata)
head(goa_1984_v3$survey.grid)

head(ebs_nbs_2024_v3$survey.area)
head(ebs_nbs_2024_v3$survey.strata)
head(ebs_nbs_2024_v3$survey.grid)
