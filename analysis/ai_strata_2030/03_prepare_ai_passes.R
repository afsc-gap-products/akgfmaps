##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Create a single line that connects all the pass lines from Mark for the 
##  Central and Eastern Aleutian Islands subareas. This line will be used to 
##  break up the Central and Eastern subareas into Pacific (south) and Bering
##  (north) sides.
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())

## Google drive folder that stores all the work related to the redesign
shared_dir <- "G:/My Drive/Aleutian Island BTS Redesign/shapefiles/"

## Import the created AI hull that we'll use as a mask on the bathy raster
ai_hull <- 
  terra::vect(x = paste0(shared_dir, "intermediate_objects/ai_hull.gpkg"))

eastern_boundary <- 
  terra::vect(x = paste0(shared_dir, "eastern_boundary/boundary.shp"))

modified_passes <- 
  terra::vect(x = paste0(shared_dir, 
                         "AI_Passes/current_ai_passes.shp"))
cai_eai_hull <-
  ai_hull[ai_hull$INPFC_AREA %in% c("Central Aleutians", "Eastern Aleutians") &
            ai_hull$STRATUM_NAME != "Petrel Bank"] |>
  split(f = modified_passes)
plot(cai_eai_hull,
     col = c("red", "orange", "yellow", "green",
             "blue", "purple", "brown", "black"))
plot(modified_passes, lwd = 2, add = TRUE)

cai_eai_hull$STRATUM_NAME <- c("SE Central Aleutians", "NE Central Aleutians", 
                               "SE Eastern Aleutians", "NE Eastern Aleutians",
                               "SW Central Aleutians", "NW Central Aleutians",
                               "SW Eastern Aleutians", "NW Eastern Aleutians")

ai_hull_w_passes <- 
  rbind(ai_hull[!(ai_hull$INPFC_AREA %in% c("Central Aleutians", 
                                          "Eastern Aleutians") & 
              ai_hull$STRATUM_NAME != "Petrel Bank")],
        cai_eai_hull) |>
  terra::erase(y = eastern_boundary)

plot(ai_hull_w_passes)
text(centroids(ai_hull_w_passes), 
     ai_hull_w_passes$STRATUM_NAME, cex = 0.5,
     font = 2)

## Save
writeVector(x = ai_hull_w_passes,
            filename = paste0(shared_dir, 
                              "intermediate_objects/ai_hull_w_passes.gpkg"),
            overwrite = TRUE)
