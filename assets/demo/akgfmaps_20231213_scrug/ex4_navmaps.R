# Making navigation charts with navmaps
# Sean Rohan <sean.rohan@noaa.gov>
# Survey-Centric R Users Group
# December 13, 2023
# https://github.com/afsc-gap-products/navmaps
# navmaps version 1.1.1

library(navmaps)

# Set software type in the global environment
navmaps::set_software("timezero")
# navmaps::set_software("opencpn")

# Writing directly to Globe access databases requires 32-bit R (<= 4.0.3) and 32-bit Access
# navmaps::set_software("globe")

# But you can still write to globe-formatted csvs and import manually
# navmaps::set_software("globe", globe_type = "csv")

# Polygon ------------------------------------------------------------------------------------------
# Requires fill, color, name, and description fields
# Data from the northern Bering Sea survey grid in the akgfmaps package

# Load data from the akgfmaps package
nbs_layers <- akgfmaps::get_base_layers(select.region = "nbs") # Data need to be GPS decimal degrees (WGS84)

survey_grid <- nbs_layers$survey.grid |> sf::st_make_valid()

survey_grid$color <- navmaps_pal(values = "green",  # Assigns the closest valid color for the software
                                  software_format = SOFTWARE,
                                  file_type = FILE_TYPE_POLYGON)
survey_grid$fill <- 0

sf_to_nav_file(
  x = survey_grid ,
  file = paste0("survey_grid_", SOFTWARE, ".", FILE_TYPE_POLYGON),
  name_col = "STATIONID",
  description_col = "STATIONID",
  color_col = "color",
  fill_col = "fill",
  software_format = SOFTWARE
)


# Point --------------------------------------------------------------------------------------------
# Requires shape, color, name, and description fields
# Station allocation from the 2022 Gulf of Alaska survey

# There is a function in the package that formats allocation files how GOA/AI folks seem to prefer
# (?make_station_allocation), along with a few other functions that are specific to GAP surveys.

# Read in the csv
goa_allocation <- read.csv("GOA2023_Station_allocation_520_EW.csv")

head(goa_allocation)

# Convert to sf
goa_allocation <- sf::st_as_sf(goa_allocation,
                               coords = c("longitude", "latitude"),
                               crs = "WGS84")

# Add color
goa_allocation$color <- factor(goa_allocation$vessel,
                               labels = navmaps::navmaps_pal(values = c("red", "yellow"),
                                                             software_format = SOFTWARE,
                                                             file_type = FILE_TYPE_POINT))

# Add shape

# Examine shape options for software
# I recommend picking shapes that are supported in all software -- I might add defaults in a later release
globe_sym_pal(n = Inf, type = "names")
tz_sym_pal(n = Inf, type = "names")
opencpn_sym_pal(n = Inf, type = "names")

goa_allocation$shape <- factor(goa_allocation$vessel,
                               labels = navmaps::navmaps_sym_pal(values = c("circle1", "triangle1"),
                                                             software_format = SOFTWARE,
                                                             file_type = FILE_TYPE_POINT))

# Add a stratum description
goa_allocation$description <- paste0("Stratum ", goa_allocation$stratum)

sf_to_nav_file(
  x = goa_allocation ,
  file = paste0("station_allocation_", SOFTWARE, ".", FILE_TYPE_POINT),
  name_col = "id",
  description_col = "description",
  color_col = "color",
  shape_col = "shape",
  software_format = SOFTWARE
)


# Linestring ---------------------------------------------------------------------------------------
# Requires color, name, and description fields
# Historical tow paths provided for a 2023 EBS shelf/slope comparison project

slope_tows <- sf::st_read("slope_tows.shp")

# Set tow path color
slope_tows$color <-  navmaps::navmaps_pal(values = c("darkgreen"),
                                          software_format = SOFTWARE,
                                          file_type = FILE_TYPE_POINT)

# Make a depth description
slope_tows$description <- paste0("Depth: ", slope_tows$bttm_dp)

sf_to_nav_file(
  x = slope_tows,
  file = paste0("slope_tows_", SOFTWARE, ".", FILE_TYPE_LINESTRING),
  name_col = "statind",
  description_col = "description",
  color_col = "color",
  software_format = SOFTWARE
)



# Reading from a Globe Microsoft Access database (Globe) -------------------------------------------
# This will only work with 32-bit R and 32-bit Access drivers.

# Loading 2023 Alaska Knight leg 4 track (some outliers)

survey_track <- navmaps::globe_to_sf(dsn = "2023_EBS_leg4.mdb", tablename = "Track")

ggplot() +
  geom_sf(data = sf::st_wrap_dateline(nbs_layers$akland)) +
  geom_sf(data = nbs_layers$bathymetry) +
  geom_sf(data = nbs_layers$survey.grid, fill = NA) +
  geom_sf(data = survey_track, color = "red") +
  scale_x_continuous(limits = nbs_layers$plot.boundary$x) +
  scale_y_continuous(limits = nbs_layers$plot.boundary$y) +
  theme_bw()
