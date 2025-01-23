# Example: Using the old get_base_layers() function
# SCRUG, akgfmaps, January 22, 2025
# Created by Sean Rohan (GitHub: sean-rohan-noaa)

# Through the end of 2025, an option will be provided to use the old interface and layers using
# get_base_layers_v3(). The function is limited to the functionality and layers that were avaialble
# in akgfmaps v 3.6.2
#
# The old version has the following limitations:
# - No access to design years (e.g., the most recent GOA BT survey stratum shapefile will be 1984)
# - Cannot select multiple regions at the same time
# - Field names differ among shapefiles
# - No correspondence with GAP_PRODUCTS
# - Access to longline

# Recommendation: Don't use this version unless you can't replace with the new version due to time
# and reliance on fields in specific shapefiles.

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
