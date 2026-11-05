##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project: Update Aleutian Islands strata with updated bathymetry
## Author:  Zack Oyafuso (zack.oyafuso@noaa.gov)
## Notes:   The AI bottom trawl survey footprint is first sectioned into four
##          INPFC areas: Western AI (WAI), Central AI (CAI), Eastern AI (EAI), 
##          Southern Bering Sea (SBS). Within each INPFC area, there are further
##          subareas delinations by longitude (East/West) and/or depending on 
##          whether the area faces the Bering Sea (North) or Pacific Ocean
##          (South). Within each subarea, strata are defined by four depth
##          bins: 1-100 m; 101-200 m; 201-300 m; 301-500 m.
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())

library(terra); library(sf); library

## Google drive folder that stores all the work related to the redesign
shared_dir <- "G:/My Drive/Aleutian Island BTS Redesign/shapefiles/"

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Section 1: Import base spatial objects
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Import the subsetted AI bathymetry raster
ai_bathy <- terra::rast(paste0(shared_dir, 
                               "intermediate_objects/ai_bathy.tif"))

## Import the created AI hull that we'll use as a mask on the bathy raster
ai_hull <- terra::vect(x = paste0(shared_dir, 
                                  "intermediate_objects/ai_hull_w_passes.gpkg"))

## Import base AI spatial layers from akgfmaps
akland <- terra::vect(x = paste0(shared_dir, 
                                 "coastline/akland.gpkg"))

## Import the western portion of the GOA DESIGN_YEAR 2025 survey area. We will
## use this to remove the Pacific side of the SBS area.
goa_footprint_2025 <- 
  terra::vect(x = paste0(shared_dir, 
                         "survey_footprint/goa_footprint_2025.gpkg"))

## Import current stratum table
current_ai_strata_tbl <- 
  read.csv(file = paste0(shared_dir,
                         "historical_objects/current_ai_strata_tbl.csv"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Section 2: Create the first level of stratification: depth ranges. 
##  Polygonize the bathymetry raster based on the four depth ranges:
##  1-100 m; 101-200 m; 201-300 m; 301-500 m, resulting in four polygons 
##  Every subarea has the same depth ranges as strata, so it is cleaner to 
##  start with the depth ranges. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##Classify bathymetry raster cells into the four depth bins
depth_cuts <- matrix(data = c(0,   100, 1,  
                              100, 200, 2,
                              200, 300, 3,
                              300, 500, 4), 
                     ncol = 3, 
                     byrow = TRUE)
depth_polygons <- terra::classify(x = ai_bathy, rcl = depth_cuts) |>
  terra::as.polygons()
names(x = depth_polygons) <- "value"

## Smooth polygon edges along a range of smoothness values and save
# for (ikeep in c(0.05, 0.1, 0.25, 0.5)) {
#   rmapshaper::ms_simplify(depth_polygons |> sf::st_as_sf(), keep = ikeep) |>
#     terra::vect() |>
#     terra::intersect(y = ai_hull) |>
#     terra::writeVector(filename = paste0(shared_dir, 
#                                          "test_objects/ai_bathy_smooth_", 
#                                          ikeep * 100, ".gpkg"), 
#                        overwrite = TRUE) 
# }
## In ArcPro, look at these different layers and choose a level of smoothness
## that "looks good". Choosing 0.25 as an example
depth_polygons_smoothed <- 
  rmapshaper::ms_simplify(depth_polygons |> sf::st_as_sf(), keep = 0.25) |>
  terra::vect()

## The goal is to "snap" the strata right to the coastline, so this step 
## assigns the areas that are adjacent to land but not covered by the bathymetry
## raster

shallow_stratum_snapped_to_coastline <- 
  ## Dissolve the inner boundaries of the depth_polygons_smooothed
  depth_polygons_smoothed |> terra::aggregate() |>
  ## Extract the internal holes of the resultant polygon (this is what the 
  ## inverse = TRUE argument is doing). This exposes both the islands as well 
  ## as the parts of the survey area around the coastline not covered by the 
  ##bathymetry raster
  terra::fillHoles(inverse = TRUE) |>
  ## Merge this nearshore bits with the shallowest depth stratum polygon
  terra::union(y = depth_polygons_smoothed[depth_polygons_smoothed$value == 1]) |>
  ## and dissolve the inner boundaries
  terra::aggregate()
shallow_stratum_snapped_to_coastline$value <- 1

## Replace the shallowest stratum in depth_polygons_smoothed with the 
## shallow_stratum_snapped_to
depth_polygons_smoothed <- 
  rbind(depth_polygons_smoothed[depth_polygons_smoothed$value != 1],
        shallow_stratum_snapped_to_coastline) 

## Save to view in ArcPro
terra::writeVector(x = depth_polygons_smoothed, 
                   filename = paste0(shared_dir, "intermediate_objects/",
                                     "depth_polygons_smoothed.gpkg"),
                   overwrite = TRUE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  There are a bunch of specks of strata drawn due to the literal nature of 
##  the raster-derived creation of the strata. This next step is purely for
##  aesthetic improvement in which specks < 5 km2 are dissolved into the 
##  adjacent polygon with the highest boundary overlap. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

depth_polygons_wo_specks <- depth_polygons_smoothed

for (i in 1:4) {
  # Extract stratum i
  temp_strata <- depth_polygons_wo_specks[depth_polygons_wo_specks$value == i]
  
  # Extract holes/specks and split into singlepart polygons
  specks <- temp_strata |>
    terra::fillHoles(inverse = TRUE) |>
    terra::disagg() 
  
  # Filter specks < 5 km^2
  specks_lt_5km2 <- specks[terra::expanse(specks) / 1e6 < 5, ]
  
  # Skip loop iteration if there are no small specks to dissolve
  if (length(x = specks_lt_5km2) == 0) next
  
  # Remove the specks from the target vector so x and y are distinct
  targets <- terra::erase(x = depth_polygons_wo_specks, y = specks_lt_5km2)
  
  # Dissolve small specks into adjacent targets with max shared boundary
  depth_polygons_wo_specks <- terra::combineGeoms(
    x = targets,
    y = specks_lt_5km2,
    boundary = TRUE,
    dissolve = TRUE
  )
}

terra::writeVector(x = depth_polygons_wo_specks,
                   filename = paste0(shared_dir, "intermediate_objects/",
                                     "depth_polygons_wo_specks.gpkg"),
                   overwrite = TRUE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Take the depth_polygons_wo_specks, erase the land (this snaps the shallow
##   strata right at the coastline), intersect with the ai_hull, then erase
##   the area in the 2025 goa footprint, retaining the unimak area. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
depth_polygons_by_subarea <-
  depth_polygons_wo_specks |>
  terra::erase(y = akland) |>
  terra::intersect(y = ai_hull |> terra::fillHoles()) |>
  terra::erase(y = goa_footprint_2025) 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  There are still isolated "specks" remaining from the main stratum polygons 
##  (this is easier to see in ArcPro). Remove these areas by isolating specks
##  < 5 km2. These areas would be too small to land a station anyway, so it 
##  makes sense to remove these isolated specks. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
diagg_polygons_gte_km2 <- depth_polygons_by_subarea |>
  terra::aggregate() |>
  terra::disagg()
diagg_polygons_gte_km2 <- 
  diagg_polygons_gte_km2[terra::expanse(x = diagg_polygons_gte_km2) / 1e6 >= 5]

writeVector(x = diagg_polygons_gte_km2,
            filename = paste0(shared_dir, "intermediate_objects/",
                              "diagg_polygons_gte_km2.gpkg"),
            overwrite = TRUE)

## Remove specks < 5 km2 by using diagg_polygons_gte_km2 as a mask 
ai_strata_2030 <-
  terra::crop(x = depth_polygons_by_subarea, y = diagg_polygons_gte_km2) 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Lastly, merge some depth strata that are not separated by an W-E boundary.
##  There are three cases
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## SBS: the 201-300 m and 301-500 m strata each span the entire SBS
ai_strata_2030 <-
  rbind(
    ai_strata_2030[!(ai_strata_2030$INPFC_AREA == "Southern Bering Sea" &
                      ai_strata_2030$value %in% 3:4)],
    
    ai_strata_2030[ai_strata_2030$INPFC_AREA == "Southern Bering Sea" &
                    ai_strata_2030$value %in% 3:4] |>
      terra::aggregate(by = c("INPFC_AREA", 'value'), 
                       count = F) |>
      tidyterra::mutate(STRATUM_NAME = "Combined Southern Bering Sea",
                        INPFC_AREA = "Combined Southern Bering Sea") 
  )

## CAI: the northern part of the CAI (not in Petrel Bank) span the entirety of  
##      the northern part of the CAI
ai_strata_2030 <-
  rbind(
    ai_strata_2030[!(ai_strata_2030$STRATUM_NAME %in% c("NE Central Aleutians", 
                                                      "NW Central Aleutians"))],
    
    ai_strata_2030[ai_strata_2030$STRATUM_NAME %in% c("NE Central Aleutians", 
                                                    "NW Central Aleutians")] |>
      terra::aggregate(by = c("INPFC_AREA", 'value'), 
                       count = F) |>
      tidyterra::mutate(STRATUM_NAME = "N Central Aleutians",
                        INPFC_AREA = "N Central Aleutians") 
  )

## EAI: the 201-300 m and 301-500 m strata on the Bering side of the EAI
##       span the entire Bering side of the EAI

ai_strata_2030 <-
  rbind(
    ai_strata_2030[!(ai_strata_2030$STRATUM_NAME %in% c("NE Eastern Aleutians",
                                                      "NW Eastern Aleutians") &
                      ai_strata_2030$value %in% 4)],
    
    ai_strata_2030[ai_strata_2030$STRATUM_NAME %in% c("NE Eastern Aleutians",
                                                    "NW Eastern Aleutians") &
                    ai_strata_2030$value %in% 4] |>
      terra::aggregate(by = c("INPFC_AREA", 'value'), 
                       count = F) |>
      tidyterra::mutate(STRATUM_NAME = "Combined Eastern Aleutian Islands",
                        INPFC_AREA = "Combined Eastern Aleutian Islands") 
  )

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Format the dataframe of ai_strata_2030 to exactly what's on GAP_PRODUCTS.AREA
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ai_strata_2030 <-
  merge(x = ai_strata_2030,
        y = data.frame(DEPTH_MIN_M = c(1, 101, 201, 301),
                       DEPTH_MAX_M = c(100, 200, 300, 500),
                       value = 1:4),
        by = "value") |>
  tidyterra::mutate(AREA_NAME = STRATUM_NAME,
                    DESCRIPTION = STRATUM_NAME,
                    AREA_KM2 = terra::expanse(x = ai_strata_2030, unit = "km"),
                    SURVEY_DEFINITION_ID = 52,
                    AREA_TYPE = 'STRATUM',
                    DESIGN_YEAR = 2030) |>
  merge(y = current_ai_strata_tbl |>
          subset(select = c(AREA_ID, AREA_NAME, DEPTH_MIN_M, DEPTH_MAX_M)),
        by = c("AREA_NAME", "DEPTH_MIN_M", "DEPTH_MAX_M")) |>
  tidyterra::arrange(AREA_ID) |>
  tidyterra::select(c(DESIGN_YEAR, SURVEY_DEFINITION_ID, AREA_ID, AREA_TYPE, 
                    AREA_NAME, DESCRIPTION, AREA_KM2, DEPTH_MIN_M, DEPTH_MAX_M))

## Finally, save the new strata to 
writeVector(x = ai_strata_2030,
            filename = paste0(shared_dir, 
                              "final_objects/ai_strata_2030.gpkg"),
            overwrite = TRUE)

# ai_strata_2030 |>
#   tidyterra::select(AREA_ID, AREA_NAME, AREA_KM2) |>
#   as.data.frame() |>
#   merge(y = current_ai_strata_tbl |>
#           tidyterra::select(AREA_ID, AREA_NAME, AREA_KM2),
#         by = c("AREA_ID", "AREA_NAME"), suffixes = c("_2030", "_1991")) |>
#   tidyterra::mutate(AREA_KM2_DIFF = AREA_KM2_2030 - AREA_KM2_1991,
#                     AREA_KM2_DIFF_PERC = round(x = AREA_KM2_DIFF / AREA_KM2_1991 * 100, 
#                                                digits = 2) )

