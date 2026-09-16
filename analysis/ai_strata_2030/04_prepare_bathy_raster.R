##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Prepare Bathymetry Raster
##  Mark's bathymetry raster encompasses a large portion of Alaska and it's a 
##  large file. To save on computation time, this step is just cropping the 
##  part of the raster that we're interested in, which is the Aleutian survey
##  foorprint subsetted to the depths shallow than 500 m.
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())

## Import libraries
library(sf)
library(terra)
library(akgfmaps)

shared_dir <- "G:/My Drive/Aleutian Island BTS Redesign/shapefiles/"

total_ai_hull <- terra::vect(x = paste0(shared_dir, 
                                        "intermediate_objects/ai_hull.gpkg"))

## Import Mark's AI bathymetry raster (100 m resolution) and project it to 
## NAD83 / Alaska Albers (EPSG:3338) 
## Mask the bathymetry raster using our created ai_hull and NULL out 
## raster cell values outside of the survey depth range (0-500 m)
ai_bathy <- terra::rast(paste0(shared_dir,
                               "From Mark/ai_grid_100m/")) |>
  terra::project("EPSG:3338") |>
  terra::crop(y = total_ai_hull, mask = FALSE)
ai_bathy[(ai_bathy > 500) | (ai_bathy <= 0)] <- NA

## Save output
terra::writeRaster(x = ai_bathy,
                   filename = paste0(shared_dir, 
                                     "intermediate_objects/ai_bathy.tif"), 
                   overwrite = TRUE)
