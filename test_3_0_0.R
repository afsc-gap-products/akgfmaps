# Testing updated functions for akgfmaps version 3.0
# Updates for 3.0:
# - Removed raster and sp as dependencies and eplaced raster functions with terra functions ahead of the 2023 retirement of rgdal and sp. Functions that previously generated RasterLayer and RasterBrick objects (raster package) now produce SpatRaster objects (terra package).
# - Added a make_idw_stack() function that creates multilayer SpatRaster objects where interpolations are conducted by grouping variables (such as YEAR). Intended to reduce unnecessary run-times and post-hoc manipulation when generating maps for multiple years.
# - Updated vignettes and demos to reflect changes.


library(akgfmaps)

test1 <- make_idw_map(
  x = akgfmaps::YFS2017,
  COMMON_NAME = "yellowfin sole",
  LATITUDE = NA,
  LONGITUDE = NA,
  CPUE_KGHA = NA,
  region = "bs.south",
  extrap.box = NULL,
  extrapolation.grid.type = "stars",
  set.breaks = "jenks",
  grid.cell = c(1e4, 1e4),
  in.crs = "+proj=longlat",
  out.crs = "EPSG:3338",
  key.title = "auto",
  log.transform = FALSE,
  idw.nmax = 4,
  use.survey.bathymetry = TRUE,
  return.continuous.grid = TRUE
)

test1$plot

test2 <- make_idw_map(
  x = akgfmaps::YFS2017,
  COMMON_NAME = "yellowfin sole",
  LATITUDE = NA,
  LONGITUDE = NA,
  CPUE_KGHA = NA,
  region = "bs.south",
  extrap.box = NULL,
  extrapolation.grid.type = "sf",
  set.breaks = "jenks",
  grid.cell = c(1e4, 1e4),
  in.crs = "+proj=longlat",
  out.crs = "EPSG:3338",
  key.title = "auto",
  log.transform = FALSE,
  idw.nmax = 4,
  use.survey.bathymetry = TRUE,
  return.continuous.grid = TRUE
)

test2$plot

test3 <- make_idw_map(
  x = akgfmaps::YFS2017,
  COMMON_NAME = "yellowfin sole",
  LATITUDE = NA,
  LONGITUDE = NA,
  CPUE_KGHA = NA,
  region = "bs.south",
  extrap.box = NULL,
  extrapolation.grid.type = "sf.simple",
  set.breaks = "jenks",
  grid.cell = c(1e4, 1e4),
  in.crs = "+proj=longlat",
  out.crs = "EPSG:3338",
  key.title = "auto",
  log.transform = FALSE,
  idw.nmax = 4,
  use.survey.bathymetry = TRUE,
  return.continuous.grid = TRUE
)

test3$plot

plot(test1$continuous.grid)
plot(test2$continuous.grid)
plot(test3$continuous.grid)


# Testing 2D grid generation
test4 <- make_2d_grid(obj = test3$map_layers$survey.area, 
                      resolution = c(1e4, 1e4), 
                      output_type = "raster", 
                      bbox = NULL, 
                      model = "semi-open")

test5 <- make_2d_grid(obj = test3$map_layers$survey.area, 
                      resolution = c(1e4, 1e4), 
                      output_type = "polygon", 
                      bbox = NULL, 
                      model = "semi-open")

test6 <- make_2d_grid(obj = test3$map_layers$survey.area, 
                      resolution = c(1e4, 1e4), 
                      output_type = "point", 
                      bbox = NULL, 
                      model = "semi-open")


plot(test4)
plot(test5)
plot(test6)

test_input <- dplyr::bind_rows(
  akgfmaps::YFS2017 |>
    dplyr::mutate(YEAR = 2017),
  akgfmaps::YFS2017 |>
    dplyr::mutate(YEAR = 1999,
                  CPUE_KGHA = CPUE_KGHA * 0.1)
)

test7 <- make_idw_stack(x = test_input,
               COMMON_NAME = NA,
               LATITUDE = NA,
               LONGITUDE = NA,
               CPUE_KGHA = NA,
               region = "bs.south",
               extrap.box = NULL,
               extrapolation.grid.type = "stars",
               set.breaks = "jenks",
               grouping.vars = "YEAR",
               grid.cell = c(5000,5000),
               in.crs = "+proj=longlat",
               out.crs = "EPSG:3338",
               log.transform = FALSE,
               idw.nmax = 4,
               use.survey.bathymetry = TRUE)

plot(test7$extrapolation.stack['1999'])
plot(test7$extrapolation.stack['2017'])

test8 <- make_idw_stack(x = test_input,
                        COMMON_NAME = NA,
                        LATITUDE = NA,
                        LONGITUDE = NA,
                        CPUE_KGHA = NA,
                        region = "bs.south",
                        extrap.box = NULL,
                        extrapolation.grid.type = "sf",
                        set.breaks = "jenks",
                        grouping.vars = "YEAR",
                        grid.cell = c(5000,5000),
                        in.crs = "+proj=longlat",
                        out.crs = "EPSG:3338",
                        log.transform = FALSE,
                        idw.nmax = 4,
                        use.survey.bathymetry = TRUE)

ggplot() +
  geom_sf(data = test8$extrapolation.stack,
          mapping = aes(fill = var1.pred)) +
  facet_grid(~YEAR)

names(test8$extrapolation.stack)

test9 <- make_idw_stack(x = test_input,
                        COMMON_NAME = NA,
                        LATITUDE = NA,
                        LONGITUDE = NA,
                        CPUE_KGHA = NA,
                        region = "bs.south",
                        extrap.box = NULL,
                        extrapolation.grid.type = "sf.simple",
                        set.breaks = "jenks",
                        grouping.vars = "YEAR",
                        grid.cell = c(5000,5000),
                        in.crs = "+proj=longlat",
                        out.crs = "EPSG:3338",
                        log.transform = FALSE,
                        idw.nmax = 4,
                        use.survey.bathymetry = TRUE)

ggplot() +
  geom_sf(data = test9$extrapolation.stack,
          mapping = aes(fill = var1.pred)) +
  facet_grid(~YEAR)

map_layers <- akgfmaps::get_base_layers(select.region = "nbs", set.crs = "EPSG:3338")


test10 <- reproject_gebco(x = system.file("./extdata/test_files/gebco_2023_n66.0_s60.0_w-177.0_e-159.5.nc", package = "akgfmaps"),
                          z_varname = "elevation",
                          z_direction = -1,
                          raster_template = NULL,
                          extent =  map_layers$survey.area,
                          resolution = 1e4,
                          set_crs = NULL,
                          return_slope_aspect = FALSE,
                          interpolation_method = "bilinear",
                          slope_nn = 8,
                          slope_units = "degrees")

# Note that default raster::mask() and terra::mask() functions have different behaviors
ggplot() +
  geom_stars(data = terra::mask(x = test10, 
                                mask = map_layers$survey.area,
                                touches = FALSE) |>
               stars::st_as_stars()) +
  geom_sf(data = map_layers$survey.area, 
          color = "black",
          fill = NA) +
  ggtitle(label = "terra::mask(touches = FALSE)") +
  scale_fill_distiller(palette = "Spectral", na.value = NA)

ggplot() +
  geom_stars(data = terra::mask(x = test10, 
                                mask = map_layers$survey.area,
                                touches = TRUE) |>
               stars::st_as_stars()) +
  geom_sf(data = map_layers$survey.area, 
          color = "black",
          fill = NA) +
  ggtitle(label = "terra::mask(touches = TRUE)") +
  scale_fill_distiller(palette = "Spectral", na.value = NA)

