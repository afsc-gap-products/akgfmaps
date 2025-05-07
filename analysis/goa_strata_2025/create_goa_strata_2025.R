##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Create 2025 GOA Stratum Polygons
## Author:        Zack Oyafuso (zack.oyafuso@noaa.gov)
## Description:   Create new depth-based stratum boundaries from Mark 
##                Zimmerman's latest 2023 bathymetry compilation and 
##                create new stratum polygons for the 2025 GOA survey.  
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Restart R Session before running
rm(list = ls())

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import Packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(terra)
library(akgfmaps)
library(sf)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import input data ----
##   Make sure to connect to VPN or NOAA Internal Network
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## `depth_mods` is a table that specifies depth boundaries for the new strata
## for each NMFS reporting area. Note that the depth strata are different
## across NMFS reporting areas
depth_mods <-
  read.csv(file = "analysis/goa_strata_2025/depth_modifications_2025.csv")

## `bathy` is the most recent 2023 bathymetric compilation of the Gulf of 
## Alaska. Provided from Mark Zimmerman et al. (2025)
bathy <-
  terra::rast("//AKC0SS-n086/AKC_PubliC/Dropbox/Zimm/GEBCO/GOA/goa_bathy")

## `goa_base` are basic shape layers from the akgfmaps package
goa_base <- akgfmaps::get_base_layers(select.region = "goa", 
                                      set.crs = terra::crs(bathy),
                                      design.year = 1984)

## The `goa_domain` is a dissolved version of `old_goa_strata`
goa_domain <- goa_base$survey.strata |> 
  terra::vect() |>
  terra::aggregate()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import NMFS Areas ----
##   NMFS Management area is a 2025 stratum variable and is different from the
##   historically used INPFC areas. Reproject `nmfs` shape object to the same
##   projection as the `bathy` raster and add management area names. The NMFS
##   areas extend much deeper than the survey domain (see plot(nmfs))
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nmfs <- 
  terra::vect(x = akgfmaps::get_nmfs_areas(set.crs = terra::crs(x = bathy)))
nmfs <- nmfs[nmfs$REP_AREA %in% c(610, 620, 630, 640, 650), "REP_AREA"]
nmfs_area_names <- c("610" = "Shumagin",
                     "620" = "Chirikof",
                     "630" = "Kodiak",
                     "640" = "West Yakutat",
                     "650" = "Southeast Outside")
nmfs$NMFS_AREA <- nmfs_area_names[paste(nmfs$REP_AREA)]
nmfs <- terra::intersect(x = nmfs, y = goa_domain)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Use `goa_domain` to mask the `bathy` raster
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bathy <- terra::crop(x = bathy, y = nmfs)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Create stratum polygons ----
##   For each management area, create new strata based on depth specifications
##   and append to strata_list
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
strata_list <- list()
for (idistrict in nmfs_area_names) { ## Loop over area -- start
  
  ## Mask bathymetry raster to just the nmfs management area and goa_domain
  district_outline <- nmfs[nmfs$NMFS_AREA == idistrict, ]
  district_bathy <- terra::crop(x = bathy, 
                                y = district_outline, 
                                mask = TRUE)
  
  ## Define modified stratum depth boundaries
  depth_boundary <- subset(x = depth_mods,
                           subset = NMFS_AREA == idistrict,
                           select = c("DEPTH_MIN_M", "DEPTH_MAX_M"))
  
  depth_boundary$DEPTH_MAX_M[depth_boundary$DEPTH_MAX_M == 1000] <- 10000
  depth_boundary$DEPTH_MIN_M[depth_boundary$DEPTH_MIN_M == 1] <- -10000
  
  ## Discretize the `bathy`` raster: Define each raster cell based on the
  ## defined stratum depth boundaries in `depth_mods` and create an arbitrary
  ## integer label for each stratum.
  depth_cuts <- as.matrix(cbind(from = depth_boundary$DEPTH_MIN_M - 1, 
                                to = depth_boundary$DEPTH_MAX_M,
                                becomes = 1:nrow(x = depth_boundary)))
  colnames(x = depth_cuts) <- NULL
  
  district_bathy <- terra::classify(x = district_bathy, rcl = depth_cuts, 
                                    others = NA, 
                                    include.lowest=TRUE)
  
  ## Convert discretized raster to polygon based on those discrete values
  strata_poly_agg <- terra::as.polygons(x = district_bathy) |>  
    terra::intersect(y = district_outline)
  
  strata_poly_agg$area <- terra::expanse(x = strata_poly_agg) / 1e6
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   When bathymetry rasters are polygonized, some "artifacts" are created.
  ##   One is that very small polygons ("specks") are created as a result of 
  ##   the complex surface of the bathymetry. For convenience, these specks, 
  ##   defined arbitrarily as any polygon < 25km2, are dissolved into the 
  ##   stratum of their neighbors. This is an iterative process, hence the 
  ##   while () loop.
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  still_needs_work <- T
  while (still_needs_work) {
    
    ## Disaggregate the stratum polygons and calculate the area
    strata_poly_disagg <- terra::disagg(x = strata_poly_agg)
    strata_poly_disagg$area <- terra::expanse(x = strata_poly_disagg) / 1e6
    
    ## Find the neighbors of each stratum polygon
    neighbors <- terra::adjacent(x = strata_poly_disagg, 
                                 type = "intersects", 
                                 symmetrical = T, pairs = F)
    
    ## A speck is defined as a polygon < 25 km2 that has neighboring polygons
    ## to dissolve into
    speck_idx <- which(x = strata_poly_disagg$area < 25 
                       & rowSums(x = neighbors) != 0)
    
    cat(paste(idistrict, length(x = speck_idx), "specks being worked on\n"))
    
    ## Dissolve each speck into the neighboring stratum with the largest area
    speck_report <- data.frame()
    
    for( temp_speck in speck_idx ) {
      
      ## Identify the neighboring stratum polygons of temp_speck
      adj_polys <- which(neighbors[temp_speck, ] == T)
      
      ## The parent_poly is the neighbor with the highest area
      parent_poly <- adj_polys[which.max(x = strata_poly_disagg$area[adj_polys])]
      
      ## Reassign the stratum of the temp_speck to that of the parent_poly.
      ## and record the change.  
      speck_report <-
        rbind(speck_report,
              data.frame(
                speck = temp_speck,
                speck_stratum = strata_poly_disagg$GOA_bathy[temp_speck],
                parent_poly = parent_poly,
                parent_stratum = strata_poly_disagg$GOA_bathy[parent_poly])
        )
      
      strata_poly_disagg$GOA_bathy[temp_speck] <- 
        strata_poly_disagg$GOA_bathy[parent_poly]
    }
    
    ## Check whether all speck was assigned to a different stratum.
    ## If yes, than we're done, otherwise still_needs_work = TRUE
    still_needs_work <- 
      !all(speck_report$speck_stratum == speck_report$parent_stratum)
    
    if (!still_needs_work) {
      
      # Any 
      orphans <- which(rowSums(x = neighbors) == 0)
      strata_poly_disagg <- strata_poly_disagg[-orphans]
    }
    
    #Reaggregate the stratum polygons by stratum or "GOA_bathy"
    strata_poly_agg <- terra::aggregate(x = strata_poly_disagg,
                                        by = "GOA_bathy",
                                        fun = "sum",
                                        count = F)
    strata_poly_agg$area <- terra::expanse(x = strata_poly_agg) / 1e6
    
  }
  
  ## Create dataframe of stratum information
  # strata_poly[, names(x = depth_mods)] <-
  strata_poly_agg[, names(x = depth_mods)] <-
    subset(x = depth_mods, subset = NMFS_AREA == idistrict)
  
  ## Calculate the total area and perimeter of the strata.
  strata_poly_agg$AREA_KM2 <- terra::expanse(x = strata_poly_agg, unit = "km")
  strata_poly_agg$PERIM_KM <- terra::perim(x = strata_poly_agg) / 1000
  
  ## Append to strata_list
  strata_list <- c(strata_list, 
                   list(strata_poly_agg[, c("NMFS_AREA", "REP_AREA", "STRATUM", 
                                            "DEPTH_MIN_M", "DEPTH_MAX_M", 
                                            "AREA_KM2", "PERIM_KM")]))
  
  print(paste("Finished with the", idistrict, "region"))
} ## Loop over district -- end

##   Merge strata into one object
strata_list <- do.call(what = rbind, args = strata_list)

plot(nmfs, col = "black", border = F)
plot(strata_list, col = "white", border = F, add = TRUE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bits <- terra::symdif(x = terra::aggregate(x = strata_list), y = nmfs)
bits <- terra::disagg(bits)
bits <- bits[round(expanse(bits)) != 0, ]

strata_list_full <- strata_list[0, ]

for (inmfs in nmfs$NMFS_AREA) {
  strata_list2 <- rbind(strata_list[strata_list$NMFS_AREA == inmfs], 
                        bits[bits$NMFS_AREA == inmfs, ])
  strata_list2$area <- terra::expanse(x = strata_list2) / 1e6
  
  neighbors <- terra::adjacent(x = strata_list2, 
                               type = "intersects", 
                               symmetrical = T, pairs = F)
  
  speck_idx <- which(strata_list2$area < 25 & 
                       rowSums(x = neighbors) != 0)
  speck_report <- data.frame()
  cat(paste(length(x = speck_idx), "specks being worked on\n"))
  
  for( temp_speck in speck_idx ) {
    adj_polys <- which(neighbors[temp_speck, ] == T)
    parent_poly <- adj_polys[which.max(x = strata_list2$area[adj_polys])]
    
    speck_report <- 
      rbind(speck_report, 
            data.frame(
              speck = temp_speck,
              speck_stratum = NA,
              parent_poly = parent_poly,
              parent_stratum = strata_list2$STRATUM[parent_poly])
      )
    
    strata_list2$STRATUM[temp_speck] <- 
      strata_list2$STRATUM[parent_poly]
    
    if (which(temp_speck == speck_idx)%%1000 == 0) {
      cat(paste("Finsished with", which(temp_speck == speck_idx),
                "of", length(speck_idx), "specks.\n") )
    }
    
  }
  
  # Get rid of orphans
  strata_list2 <- strata_list2[-which(rowSums(x = neighbors) == 0)]
  
  ## Aggregate polygons by stratum
  strata_list2 <- terra::aggregate(x = strata_list2,
                                   by = "STRATUM",
                                   fun = "sum",
                                   count = F,
                                   na.rm = TRUE)
  names(x = strata_list2) <- 
    gsub(x = names(x = strata_list2), pattern = "sum_", replacement = "")
  
  strata_list_full <- rbind(strata_list_full,
                            strata_list2)
}

## Recalculate area
strata_list_full$AREA_KM2 <- terra::expanse(x = strata_list_full) / 1e6
strata_list_full$PERIM_KM <- terra::perim(x = strata_list_full) / 1000

# goa_stations_2025 <- vect(x = "analysis/goa_strata_2025/goa_stations_2025.gpkg")
# goa_strata_2025 <- vect(x = "analysis/goa_strata_2025/goa_strata_2025.gpkg")
# plot(goa_stations_2025[goa_stations_2025$GRIDID %in% c("416-44", "415-44", "415-43", "416-43"), ], col = "green", border = F)
# plot(strata_list_full[strata_list_full$STRATUM == 24, ], add = TRUE, col = "red")
# plot(bits[bits$NMFS_AREA == "Chirikof"], add = TRUE, col = "black")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Format strata object for GAP_PRODUCTS.AREA----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
goa_strata_2025 <-
  data.frame("SURVEY" = "GOA",
             "SURVEY_DEFINITION_ID" = 47,
             "DESIGN_YEAR" = 2025,
             "AREA_ID" = strata_list_full$STRATUM,
             "AREA_TYPE" = "STRATUM",
             "AREA_NAME" = paste0(strata_list_full$NMFS_AREA, ", ",
                                  strata_list_full$DEPTH_MIN_M, "-",
                                  strata_list_full$DEPTH_MAX_M, " m"),
             "DESCRIPTION" = paste0(strata_list_full$NMFS_AREA, ", ",
                                    strata_list_full$DEPTH_MIN_M, "-",
                                    strata_list_full$DEPTH_MAX_M, " m"),
             "AREA_KM2" = strata_list_full$AREA_KM2,
             "PERIM_KM" = strata_list_full$PERIM_KM,
             "MIN_DEPTH" = strata_list_full$DEPTH_MIN_M,
             "MAX_DEPTH" = strata_list_full$DEPTH_MAX_M)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Re-project `strata_list` to EPSG:3338
##   Write geopackage ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
strata_list_full <- terra::project(x = strata_list_full, "EPSG:3338")
terra::writeVector(x = strata_list_full,
                   filename = "analysis/goa_strata_2025/goa_strata_2025.gpkg",
                   overwrite = TRUE)
