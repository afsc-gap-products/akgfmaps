# akgfmaps (2.3.1) demonstration
# December 8, 2022
# Sean Rohan <sean.rohan@noaa.gov>

# Install the latest version of the akgfmaps package
library(devtools)

install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)


# Load the akgfmaps and cowplot packages
library(akgfmaps)

# Slide 7 ---- 
# get_base_layers()
sebs_layers <- get_base_layers(select.region = "sebs",
                  set.crs = "auto")

names(sebs_layers)
class(sebs_layers$akland)


# Slide 8 ---- 
# Retrieve layers in Alaska Albers Equal area projection (EPSG:3338) and make a ggplot map
sebs_layers <- 
  get_base_layers(select.region = "sebs",
                  set.crs = "EPSG:3338")

ggplot() +
  geom_sf(data = sebs_layers$bathymetry) +
  geom_sf(data = sebs_layers$survey.area, 
          fill = NA,
          color = "red") +
  geom_sf(data = sebs_layers$akland) +
  geom_sf(data = sebs_layers$graticule,
          color = alpha("black", 0.8)) +
  scale_x_continuous(breaks = sebs_layers$lon.breaks,
                     limits = sebs_layers$plot.boundary$x) +
  scale_y_continuous(breaks = sebs_layers$lat.breaks,
                     limits = sebs_layers$plot.boundary$y) +
  theme_bw()

# Slide 9 ----
# Function documentation
?get_base_layers

# Slide 10 ----
# Generate .pdf files that show layers (this can take a few minutes)
generate_layer_guide()
generate_region_guide()

# Shapefiles are stored in package data, which can be accessed directly
list.files(system.file("extdata", package = "akgfmaps"),
           full.names = TRUE)


# Slide 12 ----
head(akgfmaps::YFS2017)


# Slide 13-14 ----
# Using make_idw_map() to interpolate YFS CPUE at 5x5 km resolution and return the results as a stars object
yfs_stars <- akgfmaps::YFS2017 %>%
  make_idw_map(region = "ebs",
               set.breaks = c(0, 25, 50, 100, 300),
               grid.cell = c(10000, 10000))

names(yfs_stars)

yfs_stars$plot + 
  theme(legend.position = "right")

# Slide 15-16 ----
# Using make_idw_map() to interpolate YFS CPUE at 5x5 km resolution and return the results as sf objects
yfs_sf <- akgfmaps::YFS2017 %>% 
  make_idw_map(region = "ebs",
               set.breaks = "jenks",
               in.crs = "+proj=longlat",
               extrapolation.grid.type = "sf",
               out.crs = "EPSG:3338",
               grid.cell = c(10000, 10000))

# sf.simple converts extrapolation grid to a POLYGON/MULTIPOLYGON geometry, masks sf to the survey area extent, then reduces the number of polygon vertices to simplify the shapes.
yfs_sf_simple <- akgfmaps::YFS2017 %>% 
  make_idw_map(region = "ebs",
               set.breaks = "jenks",
               in.crs = "+proj=longlat",
               extrapolation.grid.type = "sf.simple",
               out.crs = "EPSG:3338",
               grid.cell = c(10000, 10000))

yfs_sf$plot + 
  theme(legend.position = "none") +
  ggtitle("sf")

yfs_sf_simple$plot + 
  theme(legend.position = "none")  +
  ggtitle("sf.simple")



# Slide 17 ----
# Make a map with jenks breaks, a purple color scheme, labels, and scale bar and write it to a file
akgfmaps::YFS2017 %>%
  make_idw_map(region = "bs.all", 
               set.breaks = "jenks",
               extrapolation.grid.type = "sf",
               out.crs = "EPSG:3338",
               grid.cell = c(10000, 10000)) %>%
  add_map_labels(lab.select = c("islands", 
                                "bathymetry", 
                                "mainland")) %>%  
  change_fill_color(new.scheme = "purple2", 
                    show.plot = TRUE) %>% 
  create_map_file(file.prefix = "purple2_yfs",
                  file.path = NA, 
                  try.change_text_size = TRUE,
                  width = 12,
                  height = 9,
                  units = "in",
                  res = 300,
                  bg = "transparent")



# akgfmaps (2.3.1) demonstration
# December 8, 2022
# Sean Rohan <sean.rohan@noaa.gov>

# Install the latest version of the akgfmaps package
library(devtools)

install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)


# Load the akgfmaps and cowplot packages
library(akgfmaps)

# Slide 7 ---- 
# get_base_layers()
sebs_layers <- get_base_layers(select.region = "sebs",
                               set.crs = "auto")

names(sebs_layers)
class(sebs_layers$akland)
plot(sebs_layers$akland)


# Slide 8 ---- 
# Retrieve layers in Alaska Albers Equal area projection (EPSG:3338) and make a ggplot map
sebs_layers <- 
  get_base_layers(select.region = "sebs",
                  set.crs = "EPSG:3338")

sf::st_crs(sebs_layers$akland)

ggplot() +
  geom_sf(data = sebs_layers$bathymetry) +
  geom_sf(data = sebs_layers$survey.area, 
          fill = NA,
          color = "red") +
  geom_sf(data = sebs_layers$akland) +
  geom_sf(data = sebs_layers$graticule,
          color = alpha("black", 0.8)) +
  scale_x_continuous(breaks = sebs_layers$lon.breaks,
                     limits = sebs_layers$plot.boundary$x) +
  scale_y_continuous(breaks = sebs_layers$lat.breaks,
                     limits = sebs_layers$plot.boundary$y) +
  theme_bw()

# Slide 9 ----
# Function documentation
?get_base_layers

# Slide 10 ----
# Generate .pdf files that show layers (this can take a few minutes)
generate_layer_guide()
generate_region_guide()

# Shapefiles are stored in package data, which can be accessed directly
list.files(system.file("extdata", package = "akgfmaps"),
           full.names = TRUE, pattern = ".shp")


# Slide 12 ----
head(akgfmaps::YFS2017)


# Slide 13-14 ----
# Using make_idw_map() to interpolate YFS CPUE at 5x5 km resolution and return the results as a stars object
yfs_stars <- akgfmaps::YFS2017 %>%
  make_idw_map(region = "ebs",
               set.breaks = c(0, 25, 50, 100, 300),
               grid.cell = c(10000, 10000))

names(yfs_stars)

yfs_stars$plot + 
  theme(legend.position = "right")

# Slide 15-16 ----
# Using make_idw_map() to interpolate YFS CPUE at 5x5 km resolution and return the results as sf objects
yfs_sf <- akgfmaps::YFS2017 %>% 
  make_idw_map(region = "ebs",
               set.breaks = "jenks",
               in.crs = "+proj=longlat",
               extrapolation.grid.type = "sf",
               out.crs = "EPSG:3338",
               grid.cell = c(10000, 10000))

# sf.simple converts extrapolation grid to a POLYGON/MULTIPOLYGON geometry, masks sf to the survey area extent, then reduces the number of polygon vertices to simplify the shapes.
yfs_sf_simple <- akgfmaps::YFS2017 %>% 
  make_idw_map(region = "ebs",
               set.breaks = "jenks",
               in.crs = "+proj=longlat",
               extrapolation.grid.type = "sf.simple",
               out.crs = "EPSG:3338",
               grid.cell = c(10000, 10000))

yfs_sf$plot + 
  theme(legend.position = "none") +
  ggtitle("sf")

yfs_sf_simple$plot + 
  theme(legend.position = "none")  +
  ggtitle("sf.simple")



# Slide 17 ----
# Make a map with jenks breaks, a purple color scheme, labels, and scale bar and write it to a file
akgfmaps::YFS2017 %>%
  make_idw_map(region = "bs.all", 
               set.breaks = "jenks",
               extrapolation.grid.type = "sf",
               out.crs = "EPSG:3338",
               grid.cell = c(10000, 10000)) %>%
  add_map_labels(lab.select = c("islands", 
                                "bathymetry", 
                                "mainland")) %>%  
  change_fill_color(new.scheme = "purple2", 
                    show.plot = TRUE) %>% 
  create_map_file(file.prefix = "purple2_yfs",
                  file.path = NA, 
                  try.change_text_size = TRUE,
                  width = 12,
                  height = 9,
                  units = "in",
                  res = 300,
                  bg = "transparent")

### Overtime

# Plot YFS data as points

sebs_layers <- 
  get_base_layers(select.region = "sebs",
                  set.crs = "EPSG:3338")

head(akgfmaps::YFS2017)

yfs_sf <- akgfmaps::YFS2017 |>
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
               crs = "EPSG:4326") |>
  sf::st_transform(crs = sebs_layers$crs)

ggplot() +
  geom_sf(data = sebs_layers$bathymetry) +
  geom_sf(data = sebs_layers$survey.area, 
          fill = NA,
          color = "red") +
  geom_sf(data = sebs_layers$akland) +
  geom_sf(data = sebs_layers$graticule,
          color = alpha("black", 0.8)) +
  geom_sf(data = yfs_sf,
          mapping = aes(color = CPUE_KGHA)) +
  scale_x_continuous(breaks = sebs_layers$lon.breaks,
                     limits = sebs_layers$plot.boundary$x) +
  scale_y_continuous(breaks = sebs_layers$lat.breaks,
                     limits = sebs_layers$plot.boundary$y) +
  scale_color_viridis_c(name = "CPUE") +
  theme_bw()


# Plot a temperature raster from the coldpool package

bt_2022 <- raster::subset(coldpool::ebs_bottom_temperature,
                          subset = "sebs_ste_2022_gear_temperature")


bt_2022_df <- bt_2022 |>
  raster::rasterToPoints() |>
  as.data.frame()

ggplot() +
  geom_sf(data = sebs_layers$bathymetry) +
  geom_sf(data = sebs_layers$survey.area, 
          fill = NA,
          color = "red") +
  geom_sf(data = sebs_layers$akland) +
  geom_sf(data = sebs_layers$graticule,
          color = alpha("black", 0.8)) +
  geom_tile(data = bt_2022_df,
            mapping = aes(x = x, y = y, fill = sebs_ste_2022_gear_temperature)) +
  scale_x_continuous(breaks = sebs_layers$lon.breaks,
                     limits = sebs_layers$plot.boundary$x) +
  scale_y_continuous(breaks = sebs_layers$lat.breaks,
                     limits = sebs_layers$plot.boundary$y) +
  scale_fill_viridis_c(name = "BT", option = "B") +
  theme_bw()