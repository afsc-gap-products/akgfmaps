##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Create a hull around the survey area to use as a mask for the bathy raster
##  
##  The Aleutian Islands bottom trawl survey design is separated into four 
##  subareas from W -> E (WAI, CAI, EAI, and Southern Bering Sea). The areas
##  are separated by these longitudes: 
##  177 E: Divider between WAI and CAI
##  177 W: Divider between CAI and EAI
##  170 W: Divider between EAI and SBS
##
##  Each subarea is further divided into a western and eastern portion, also
##  by these longitudes:
##  175 E: Divider between the western and eastern portions of the WAI
##  180  : Divider between the western and eastern portions of the CAI
##  174 W: Divider between the western and eastern portions of the EAI
##  168 W: Divider between the western and eastern portions of the SBS
##
##  In the CAI, Petrel Bank is a separate area.
##
##  We also want to add the area near unimak pass now abandonded by the 
##  GOA survey to the SBS and possibly remove the part of the SBS covered by 
##  the BS slope survey area. 
##
##  The polygon created in the next section will be used as a mask to "stamp 
##  out" the portion of the bathymetry raster that we are interested. I put a 
##  spatial buffer on the edge to account for the possibility of the spatial
##  footprint expanding. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())

## Import libraries
library(sf)
library(terra)
library(akgfmaps)

## Setup directories to put the outputted spatial objects
shared_dir <- "G:/My Drive/Aleutian Island BTS Redesign/shapefiles/"

for (iobj in c("bss_footprint", "bs_footprint", "goa_footprint_1984", 
               "goa_footprint_2025")) {
  assign(x = iobj, 
         value = terra::vect(x = paste0(shared_dir, "survey_footprint/",
                                        iobj, ".gpkg")))
}; rm(iobj)

current_ai_strata <- 
  terra::vect(x = paste0(shared_dir, "historical_objects/current_ai_strata.gpkg"))

## Define longitudes for each subarea (see comments above)
longs <- c(175, 177, 180, -177, -174, -170, -168)

## Build vector lines in lat/lon (EPSG:4326)
lines_list <- lapply(seq_along(longs), function(i) {
  lats <- seq(50, 55, length.out = 100)
  coords <- cbind(longs[i], lats)
  terra::vect(coords, type = "lines", crs = "EPSG:4326")
})

## Combine and project to EPSG:3338
lines_vect <- do.call(rbind, lines_list)
lines_vect_3338 <- terra::project(lines_vect, "EPSG:3338")

## First create a hull around the ai survey footprint, excluding the strata
## on Petrel Bank and the SBS and add a 12 km buffer. 
ai_hull <- 
  current_ai_strata[
    !current_ai_strata$STRATUM %in% c(311:314, 711, 712, 721, 722, 793, 794)
  ] |> 
  terra::hull(type = "concave_ratio", param = 0.05) |> 
  terra::buffer(width = 12000) 

## Separately isolate the Petrel Bank area and add a 3 km buffer
petrel_bank <- current_ai_strata[current_ai_strata$STRATUM %in% 311:314] |> 
  terra::aggregate() |> 
  terra::fillHoles() |>
  terra::buffer(width = 3000) 

## Separately isolate the SBS area, add a 5 km buffer
sbs_hull <- 
  current_ai_strata[ 
    current_ai_strata$STRATUM %in% c(711, 712, 721, 722, 793, 794)
  ] |> 
  terra::buffer(width = 5000)

## The total hull is created by aggregating the initial ai_hull, sbs_hull, and 
## the wGOA bit, removing the bits of this resultant polygon that overlap with
## the bs, and bss survey footprints, intersecting the longitudinal
## lines to create the subareas, and then finally adding the petrel bank area.
## Plot this to see what this looks like.
total_ai_hull <- rbind(ai_hull, sbs_hull, goa_footprint_1984) |> 
  terra::aggregate() |>
  terra::erase(y = bs_footprint) |>
  terra::erase(y = bss_footprint) |>
  terra::erase(y = lines_vect_3338 |> terra::buffer(width = 0.0001)) |> 
  terra::disagg() |>
  rbind(petrel_bank)

total_ai_hull$INPFC_AREA <-
  c("Southern Bering Sea", "Southern Bering Sea", 
    "Eastern Aleutians", "Eastern Aleutians", 
    "Central Aleutians", "Central Aleutians", 
    "Western Aleutians", "Western Aleutians", 
    "Southern Bering Sea", "Southern Bering Sea", 
    "Central Aleutians")
total_ai_hull$STRATUM_NAME <-
  c("W Southern Bering Sea", "E Southern Bering Sea",
    "E Eastern Aleutians", "W Eastern Aleutians",
    "E Central Aleutians", "W Central Aleutians",
    "E Western Aleutians", "W Western Aleutians",
    "E Southern Bering Sea", "E Southern Bering Sea",
    "Petrel Bank")

## Aggregate Unimak polygon with the Southern Bering Sea polygons
total_ai_hull <- terra::aggregate(total_ai_hull, 
                                  by = "STRATUM_NAME", 
                                  count = FALSE)

## Plot
plot(total_ai_hull, col = RColorBrewer::brewer.pal(name = "Paired", n = 12) )
plot(lines_vect_3338, add = TRUE)
text(total_ai_hull |> terra::centroids() |> terra::crds(),
     total_ai_hull$STRATUM_NAME, cex = 0.5)

## Save 
terra::writeVector(x = total_ai_hull, 
                   filename = paste0(shared_dir, 
                                     "intermediate_objects/ai_hull.gpkg"),
                   overwrite = TRUE)
