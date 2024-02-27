# Making maps with akgfmaps
# Sean Rohan <sean.rohan@noaa.gov>
# Survey-Centric R Users Group
# February 27, 2024
# https://github.com/afsc-gap-products/akgfmaps
# akgfmaps version 3.4.2

library(akgfmaps)

# get_base_layers() --------------------------------------------------------------------------------
#
# Make a list that includes survey area, survey strata, survey grid, bathymetry, and graticule layers,
# along with recommended plot limits and latitude/longitude breaks for the regions.
# See function documentation ?akgfmaps::get_base_layers() for region options.

nbs_layers <- akgfmaps::get_base_layers(select.region = "nbs",
                                        set.crs = "EPSG:3338") # Alaska Albers Equal area

ai_layers <- akgfmaps::get_base_layers(select.region = "ai.east",
                                        set.crs = "EPSG:3338") # Alaska Albers Equal area

names(nbs_layers)

# Plot with ggplot using simple features
p1 <- ggplot() +
  geom_sf(data = nbs_layers$akland) +
  geom_sf(data = nbs_layers$bathymetry) +
  geom_sf(data = nbs_layers$survey.grid,
          fill = NA) +
  scale_x_continuous(limits = nbs_layers$plot.boundary$x) +
  scale_y_continuous(limits = nbs_layers$plot.boundary$y)

# Plotting noodle bryozon CPUE in the NBS

?akgfmaps::NOODLES

names(akgfmaps::NOODLES)

# Convert from a data.frame to a simple features object then project to Alaska Albers Equal Area
noodles_sf <- akgfmaps::NOODLES |>
  sf::st_as_sf(coords = c("LONGITUDE", y = "LATITUDE"),
               crs = "WGS84")  |> # WGS84 (GPS latitude/longitude in decimal degrees)
  sf::st_transform(crs = "EPSG:3338") |>
  dplyr::mutate(Present = CPUE_KGHA > 0) # Plot presence/absence

# spatially join survey grid polygons to noodle CPUE points data
noodles_grid <- sf::st_join(nbs_layers$survey.grid,
                            noodles_sf)

ggplot() +
  geom_sf(data = nbs_layers$akland) +
  geom_sf(data = nbs_layers$bathymetry) +
  geom_sf(data = noodles_grid,
          mapping = aes(fill = Present)) +
  facet_wrap(~YEAR) +
  scale_x_continuous(limits = nbs_layers$plot.boundary$x) +
  scale_y_continuous(limits = nbs_layers$plot.boundary$y) +
  scale_fill_manual(values = c("NA", "deepskyblue2"),
                    guide = "none") +
  theme_bw()

# get_nmfs_areas() ---------------------------------------------------------------------------------
#
# NMFS Statistical Reporting areas for Alaska, provided by AKRO.

nmfs_stat_areas <- get_nmfs_areas(set.crs = "EPSG:3338")

ggplot() +
  geom_sf(data = nmfs_stat_areas) +
  geom_sf_text(data = nmfs_stat_areas,
               mapping = aes(label = REP_AREA))


# make_idw_map() -----------------------------------------------------------------------------------
#
# Make an inverse distance weighted plot for NOODLE bryozoans in 2017

noodles_2017 <- dplyr::filter(akgfmaps::NOODLES, YEAR == 2017)

noodles_idw <- akgfmaps::make_idw_map(x = noodles_2017,
                                      region = "nbs",
                                      in.crs = "WGS84",
                                      out.crs = "EPSG:3338",
                                      extrapolation.grid.type = "sf", # The default is 'stars' but I recommend using sf or sf.simple for most applications
                                      set.breaks = "jenks" # The default is "jenks", but I recommend specifying breaks manually.
                                      )

names(noodles_idw)
names(noodles_idw$map_layers) # contains layers that could be used to build your own plot

# The default plot
noodles_idw$plot

# You could use the layers to build your own plot
ggplot() +
  geom_sf(data = noodles_idw$extrapolation.grid,
          mapping = aes(fill = var1.pred), color = NA) +
  geom_sf(data = noodles_idw$map_layers$survey.grid,
          color = "white",
          fill = NA) +
  scale_fill_viridis_d(name = "CPUE") +
  theme_minimal()

# The list also includes gridded CPUE as continuous values in a stars object
ggplot() +
  geom_stars(data = noodles_idw$continuous.grid,
          mapping = aes(fill = var1.pred)) +
  scale_fill_viridis_c(name = "CPUE",
                       na.value = NA)

# make_idw_stack() ---------------------------------------------------------------------------------
#
# Making a 'stack' of inverse distance weighted predictions based on a grouping variable. In this case, year.

# Manual breaks identified through trial and error
noodles_manual_breaks <- akgfmaps::make_idw_stack(x = akgfmaps::NOODLES,
                                                  region = "nbs",
                                                  in.crs = "WGS84",
                                                  out.crs = "EPSG:3338",
                                                  extrapolation.grid.type = "sf",
                                                  set.breaks = c(0, 10, 20, 100, 300, Inf),
                                                  grouping.vars = "YEAR"
)

ggplot() +
  geom_sf(data = nbs_layers$akland) +
  geom_sf(data = nbs_layers$bathymetry) +
  geom_sf(data = noodles_manual_breaks$extrapolation.stack,
          mapping = aes(fill = var1.pred),
          color = NA) +
  facet_wrap(~YEAR) +
  scale_x_continuous(limits = nbs_layers$plot.boundary$x) +
  scale_y_continuous(limits = nbs_layers$plot.boundary$y) +
  scale_fill_viridis_d(name = "CPUE",
                       na.value = NA) +
  theme_bw()


