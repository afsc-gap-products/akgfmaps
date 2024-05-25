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
library(rmapshaper)
library(sf)
# devtools::install_github("MattCallahan-NOAA/akmarineareas2")
# library(akmarineareas2)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import input data ----
##   Make sure to connect to VPN or NOAA Internal Network
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## `depth_mods` is a table that specifies depth boundaries for the new strata
## for each NMFS reporting area. Note that the depth strata are different
## across NMFS reporting areas
depth_mods <-
  read.csv(file = "analysis/goa_strata_2025/depth_modifications_2025.csv")
depth_mods <- rbind(depth_mods,
                    data.frame(NMFS_AREA = c("Southeast Inside", "NMFS519"), 
                               REP_AREA = c(659, 519),
                               STRATUM = c(52, 16),
                               DEPTH_MIN_M = 1,
                               DEPTH_MAX_M = 1000))

## `bathy` is the most recent 2023 bathymetric compilation of the GOA provided
## from Mark Zimmerman  
bathy <-
  terra::rast("//AKC0SS-n086/AKC_PubliC/Dropbox/Zimm/GEBCO/GOA/goa_bathy")

## `goa_base` are basic shape layers from the akgfmaps package
goa_base <- akgfmaps::get_base_layers(select.region = "goa", 
                                      set.crs = terra::crs(x = bathy))

## `ak_land` is the extracted land/coastline shapefile from `goa_base` 
ak_land <- terra::vect(x = goa_base$akland[goa_base$akland$POPYADMIN %in% 
                                             c("BRITISH COLUMBIA",
                                               "YUKON TERRITORY",
                                               "ALASKA"), ])

## `old_goa_strata` is the extracted historical GOA stratum polygons. Filter
## out land and kingman reef areas (STRATUM == 0)
old_goa_strata <- 
  terra::vect(x = goa_base$survey.strata[goa_base$survey.strata$STRATUM != 0,])

## The `goa_domain` is a dissolved version of `old_goa_strata`
goa_domain <- terra::aggregate(x = old_goa_strata)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Use `goa_domain` to mask the `bathy` raster
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bathy <- terra::crop(x = bathy, y = goa_domain, mask = TRUE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import NMFS Areas ----
##   NMFS Management area is a 2025 stratum variable and is different from the
##   historically used INPFC areas. Reproject `nmfs` shape object to the same
##   projection as the `bathy` raster and add management area names. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nmfs <-
terra::vect(x = akgfmaps::get_nmfs_areas(set.crs = terra::crs(x = bathy)) )

nmfs <- terra::project(x = nmfs[nmfs$REP_AREA %in% c(519, 610, 620, 630, 640, 
                                                     650, 659),
                                "REP_AREA"],
                       terra::crs(x = goa_domain))

nmfs$NMFS_AREA <- c("519" = "NMFS519",
                    "610" = "Shumagin",
                    "620" = "Chirikof",
                    "630" = "Kodiak",
                    "640" = "West Yakutat",
                    "650" = "Southeast Outside",
                    "659" = "Southeast Inside")[paste(nmfs$REP_AREA)]
# nmfs <- terra::crop(x = nmfs, y = goa_domain)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Create stratum polygons ----
##   For each management area, create new strata based on depth specifications
##   and append to strata_list
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
strata_list <- list()
for (idistrict in unique(x = depth_mods$NMFS_AREA)) { ## Loop over area --st.
  
  ## Mask bathymetry raster to just the nmfs management area and goa_domain
  district_outline <- terra::crop(x = nmfs[nmfs$NMFS_AREA == idistrict], 
                                  y = goa_domain)
  district_bathy <- terra::mask(x = bathy, mask = district_outline)
  district_bathy <- terra::crop(x = district_bathy, y = district_outline)
  
  ## Define modified stratum depth boundaries
  depth_boundary <- subset(x = depth_mods,
                           subset = NMFS_AREA == idistrict,
                           select = c("DEPTH_MIN_M", "DEPTH_MAX_M"))
  
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
  strata_poly <- terra::as.polygons(x = district_bathy)
  # strata_poly <- 
  #   terra::vect(x = rmapshaper::ms_simplify(input = sf::st_as_sf(strata_poly), 
  #                                           keep = 0.05 ))
  
  strata_poly_disagg <- terra::disagg(x = strata_poly)
  strata_poly_disagg$area <- terra::expanse(x = strata_poly_disagg) / 1e6
  
  ## The literal assignment of raster cells to strata creates a lot of very
  ## small "specks" so in this step we dissolve these specks less than a certain 
  ## chosen area (25 km2) into their surrounding larger stratum polygons. 
  ## 
  ## First, for each stratum polygon, calculate adjacent polygons. The argument 
  ## type == "rook" excludes polygons that touch at a single node.
  nearest_poly <- terra::adjacent(x = strata_poly_disagg, type = "intersects")
  
  specks <- which(x = strata_poly_disagg$area < 25)
  
  for (i in 1:length(x = specks)) {
    
    ## isolate the speck
    temp_speck <- strata_poly_disagg[specks[i], ]
    
    ## isolate the adjacent polygons
    adj_polys <- nearest_poly[nearest_poly[, 2] == specks[i], 1]
    
    if (length(x = adj_polys) != 0) {
      ## assign the speck to the adjacent polygon with the highest area
      adj_poly <- adj_polys[which.max(x = strata_poly_disagg$area[adj_polys])]
      strata_poly_disagg$GOA_bathy[specks[i]] <-
        strata_poly_disagg$GOA_bathy[adj_poly]
    }
    if (i %% 500 == 0) 
      print(paste("Finished reassigning via dissolving", i, "of", 
                  length(x = specks), idistrict, "speck areas"))
  }
  
  ## aggregate the newly assigned specks to their new strata
  strata_poly_agg <- terra::aggregate(x = strata_poly_disagg,
                                      by = "GOA_bathy",
                                      fun = "sum",
                                      count = F)
  
  ## After this round of speck dissolution, some specks < 5 km2 remain so we do 
  ## another round of speck dissolution. 
  strata_poly_disagg <- terra::disagg(x = strata_poly_agg)
  strata_poly_disagg$area <- terra::expanse(x = strata_poly_disagg,
                                            unit = "km")
  if (idistrict == c("Southeast Inside") )
    strata_poly_disagg <- strata_poly_disagg[strata_poly_disagg$area > 100, ]
  if (idistrict == c("NMFS519") )
    strata_poly_disagg <- strata_poly_disagg[strata_poly_disagg$area > 10, ]
  
  nearest_poly <- terra::adjacent(x = strata_poly_disagg, type = "intersects")
  
  for (i in which(strata_poly_disagg$area < 25)) {
    temp_speck <- strata_poly_disagg[i, ]
    adj_polys <- nearest_poly[nearest_poly[, 2] == i, 1]
    
    if (length(adj_polys) != 0) {
      adj_poly <- adj_polys[which.max(x = strata_poly_disagg$area[adj_polys])]
      strata_poly_disagg$GOA_bathy[i] <- strata_poly_disagg$GOA_bathy[adj_poly]
    }
  }
  
  ## Aggregate the newly assigned specks to their new strata
  strata_poly_agg <- terra::aggregate(x = strata_poly_disagg,
                                      by = "GOA_bathy",
                                      fun = "sum",
                                      count = F)
  
  ## Create dataframe of stratum information
  strata_poly[, names(x = depth_mods)] <- 
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

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Format strata object for GAP_PRODUCTS.AREA----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
goa_strata_2025 <-
  data.frame("SURVEY" = "GOA",
             "SURVEY_DEFINITION_ID" = 47,
             "DESIGN_YEAR" = 2025,
             "AREA_ID" = strata_list$STRATUM,
             "AREA_TYPE" = "STRATUM",
             "AREA_NAME" = paste0(strata_list$NMFS_AREA, ", ",
                                  strata_list$DEPTH_MIN_M, "-",
                                  strata_list$DEPTH_MAX_M, " m"),
             "DESCRIPTION" = paste0(strata_list$NMFS_AREA, ", ",
                                    strata_list$DEPTH_MIN_M, "-",
                                    strata_list$DEPTH_MAX_M, " m"),
             "AREA_KM2" = strata_list$AREA_KM2,
             "PERIM_KM" = strata_list$PERIM_KM,
             "MIN_DEPTH" = strata_list$DEPTH_MIN_M,
             "MAX_DEPTH" = strata_list$DEPTH_MAX_M)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Re-project `strata_list` to EPSG:3338
##   Write geopackage ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
strata_list <- terra::project(x = strata_list, "EPSG:3338")
terra::writeVector(x = strata_list, 
                   filename = "analysis/goa_strata_2025/goa_strata_2025.gpkg", 
                   overwrite = TRUE)
