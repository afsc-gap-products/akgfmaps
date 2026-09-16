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

## Import the CAI and EAI Pass data from Mark, and remove these passes that do
## not split the area into north/south: Yoke, Asuksak, Great Sitkin and Igitkin
ai_passes <-
  terra::vect(x = paste0(shared_dir,
                         "From Mark/AI_Passes/AI_Passes.shp")) |>
  terra::crop(y = ai_hull[ai_hull$INPFC_AREA %in% c("Central Aleutians", 
                                                    "Eastern Aleutians")])
ai_passes <- ai_passes[!ai_passes$Name %in% c("Yoke", "Asuksak", 
                                              "Great Sitkin", "Igitkin")]


# Function to reorient individual lines so  vertices flow strictly west to east
orient_west_to_east <- function(lines) {
  
  # Get geometry table of coordinates for each line segment
  g <- terra::geom(x = lines) 
  
  # Loop over each line segment and reverse order if the segment runs from 
  # E -> W
  reoriented_lines <- 
    lapply( 
      X = unique(x = g[, "geom"]), 
      FUN = function(id) { 
        # Extract coordinates for current line feature
        pts <- g[g[, "geom"] == id, c("x", "y"), drop = FALSE] 
        
        # If the segment runs from E -> W, i.e., starting lon is greater than 
        # ending lon, reverse coordinate row order to flip direction to W -> E
        if (pts[1, "x"] > pts[nrow(x = pts), "x"]) 
          pts <- pts[nrow(x = pts):1, , drop = FALSE]
        
        # Return as single line SpatVector
        terra::vect(pts, type = "lines", crs = terra::crs(lines)) 
      } 
    ) 
  # Merge all individual reoriented SpatVector lines back into a single object
  do.call(what = rbind, args = reoriented_lines)
}

## Function to sort SpatVector by starting x-coordinate (longitude)
sort_by_longitude <- function(lines, decreasing = FALSE) {
  
  # Extract geometry matrix containing feature IDs, x, and y coordinates
  coords <- terra::geom(lines) 
  
  # Extract the starting x-coordinate (longitude) for each line feature
  starts <- stats::aggregate(x ~ geom, data = coords, FUN = head, n = 1) 
  
  # Reorder SpatVector features based on sorted longitude index
  ord <- order(starts$x, decreasing = decreasing)
  
  # Subset and return the SpatVector reordered by starting longitude
  return(lines[ord, ]) 
  
}

# Function to connect consecutive line segments across gaps
connect_gaps <- function(lines) {
  
  # Extract endpoint coordinatess 
  coords <- terra::geom(x = lines) 
  starts <- stats::aggregate(cbind(x, y) ~ geom, 
                             data = coords, 
                             FUN = head, 
                             n = 1) 
  ends <- stats::aggregate(cbind(x, y) ~ geom, 
                           data = coords, 
                           FUN = tail, 
                           n = 1) 
  
  
  # Pair each line's end point to the next line's start point
  n <- nrow(x = ends) # Count the total number of line features
  
  # Return the input object unchanged if there are fewer than 2 lines to connect
  if (n < 2) return(lines) 
  
  # Select the end points of lines 1 through (N - 1) as gap start points
  gap_starts <- ends[1:(n - 1), c("x", "y")] 
  # Select the start points of lines 2 through N as gap end points
  gap_ends <- starts[2:n, c("x", "y")]
  
  
  # Create connecting bridging lines. # Initialize an empty list to store the 
  # bridging line geometries
  bridge_list <- vector("list", n - 1) 
  
  for (i in 1:(n - 1)) { # Iterate through each consecutive line pair
    # Combine the i-th gap start and end coordinates into a 2x2 matrix
    pts <- rbind(as.matrix(gap_starts[i, ]), 
                 as.matrix(gap_ends[i, ])) 
    
    # Convert the coordinate matrix into a SpatVector line with matching CRS
    bridge_list[[i]] <- terra::vect(pts, type = "lines", 
                                    crs = terra::crs(x = lines)) 
  }
  
  # Combine the original lines and all new bridge segments into a single object
  all_lines <- do.call(rbind, c(list(lines), bridge_list)) 
  
  
  # Merge connected geometries into a single feature
  connected_line <- terra::aggregate(all_lines, by = NULL) 
  
  return(connected_line) # Return the single merged continuous line
  
}

## Create single pass line and write to gpkg
single_pass_line <-
  ai_passes |>
  orient_west_to_east() |>
  sort_by_longitude() |>
  connect_gaps()
terra::crs(single_pass_line) <- "EPSG:3338" 

writeVector(x = single_pass_line,
            filename = paste0(shared_dir, 
                              "intermediate_objects/single_pass_line.gpkg"),
            overwrite = TRUE)



## Stop. Create a copy of single_pass_line.gpkg. Go into ArcPro and modify 
## that copy and make sure the line  passes through islands properly. Once that 
## is finalized, proceed to the next step

modified_passes <- 
  terra::vect(x = paste0(shared_dir, 
                         "intermediate_objects/single_pass_line_modified.gpkg"))
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
        cai_eai_hull)

plot(ai_hull_w_passes)
text(centroids(ai_hull_w_passes), 
     ai_hull_w_passes$STRATUM_NAME, cex = 0.5,
     font = 2)

## Save
writeVector(x = ai_hull_w_passes,
            filename = paste0(shared_dir, 
                              "intermediate_objects/ai_hull_w_passes.gpkg"),
            overwrite = TRUE)
