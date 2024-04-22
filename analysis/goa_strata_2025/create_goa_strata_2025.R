##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       GOA Restratification Adjustments
## Author:        Zack Oyafuso (zack.oyafuso@noaa.gov)
## Description:   Workflow to take the new stratum boundaries and create new
##                stratum polygons. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Restart R Session before running
rm(list = ls())

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import Packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## install.packages("terra")
library(terra)
# devtools::install_github("MattCallahan-NOAA/akmarineareas2")
library(akmarineareas2)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import stratum depth boundaries ----
##   Import depth from Mark's shared drive location
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
depth_mods <-
  read.csv(file = "analysis/goa_strata_2025/depth_modifications_2025.csv")
bathy <-
  terra::rast("//AKC0SS-n086/AKC_PubliC/Dropbox/Zimm/GEBCO/GOA/goa_bathy")
ak_land <- terra::vect(x = akmarineareas2::ak)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Use the historical GOA survey footprint to mask the `bathy` raster
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
old_goa_strata <- terra::vect(x = "analysis/goa_strata_2025/goa_strata.shp")
goa_domain <- terra::aggregate(x = old_goa_strata[old_goa_strata$STRATUM != 0])
bathy <- terra::crop(x = bathy, y = goa_domain, mask = TRUE)

bathy <- terra::mask(x = bathy, 
                      mask = ak_land, 
                      inverse = TRUE)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import NMFS Areas ----
##   NMFS Management area is a 2025 stratum variable and is different from the
##   historically used INPFC areas. Reproject `nmfs` shape object to the same
##   projection as the `bathy` raster and add management area names. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nmfs <- terra::vect(x = akmarineareas2::nmfs)
nmfs <- terra::project(x = nmfs[nmfs$AreaID %in% c(610, 620, 630, 640, 650),
                                "AreaID"], 
                       terra::crs(x = goa_domain))
nmfs$AREA_NAME <- c("610" = "Shumagin",
                    "620" = "Chirikof",
                    "630" = "Kodiak",
                    "640" = "West Yakutat",
                    "650" = "Southeast Outside")[paste(nmfs$AreaID)]
nmfs <- terra::crop(x = terra::mask(x = nmfs, mask = goa_domain), 
                    y = goa_domain)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Create stratum polygons ----
##   For each management area, create new strata based on depth specifications
##   and append to strata_list
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
strata_list <- list()
for (idistrict in unique(x = depth_mods$manage_area)) { ## Loop over area --st.
  
  ## Mask bathymetry raster to just the nmfs management area and goa_domain
  district_outline <- nmfs[nmfs$AREA_NAME  == idistrict]
  district_bathy <- terra::crop(x = bathy,
                                y = district_outline, 
                                mask = T)
  
  ## Define modified stratum depth boundaries
  depth_boundary <- subset(x = depth_mods,
                           subset = manage_area == idistrict,
                           select = c("lower_depth_m", "upper_depth_m"))
  
  ## Discretize the `bathy`` raster: Define each raster cell based on the
  ## defined stratum depth boundaries in `depth_mods` and create an arbitrary
  ## integer label for each stratum.
  depth_cuts <- as.matrix(cbind(from = depth_boundary$lower_depth_m - 1, 
                                to = depth_boundary$upper_depth_m,
                                becomes = 1:nrow(x = depth_boundary)))
  colnames(x = depth_cuts) <- NULL
  
  district_bathy <- terra::classify(x = district_bathy, rcl = depth_cuts, 
                                    others = NA, 
                                    include.lowest=TRUE)
  
  
  ## Convert discretized raster to polygon based on those discrete values
  strata_poly <- terra::as.polygons(x = district_bathy)
  strata_poly_disagg <- terra::disagg(x = strata_poly)
  strata_poly_disagg$area <- terra::expanse(x = strata_poly_disagg) / 1e6
  
  ## The literal assignment of raster cells to strata creates a lot of very
  ## small "specks" so in this step we dissolve these specks less than a certain 
  ## chosen area (5 km2) into their surrounding larger stratum polygons. 
  ## 
  ## First, for each stratum polygon, calculate adjacent polygons. The argument 
  ## type == "rook" excludes polygons that touch at a single node.
  nearest_poly <- terra::adjacent(x = strata_poly_disagg, type = "intersects")
  
  specks <- which(x = strata_poly_disagg$area < 5)
  
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
  
  strata_poly_disagg <- terra::disagg(x = strata_poly_agg)
  strata_poly_disagg$area <- terra::expanse(x = strata_poly_disagg,
                                            unit = "km")
  
  ## After this round of speck dissolution, some specks < 5 km2 remain so we do 
  ## another round of speck dissolution. 
  nearest_poly <- terra::adjacent(x = strata_poly_disagg, type = "intersects")
  
  for (i in which(strata_poly_disagg$area < 5)) {
    temp_speck <- strata_poly_disagg[i, ]
    adj_polys <- nearest_poly[nearest_poly[, 2] == i, 1]
    
    if (length(adj_polys) != 0) {
      adj_poly <- adj_polys[which.max(x = strata_poly_disagg$area[adj_polys])]
      strata_poly_disagg$GOA_bathy[i] <- strata_poly_disagg$GOA_bathy[adj_poly]
    }
  }
  
  strata_poly_agg <- terra::aggregate(x = strata_poly_disagg,
                                      by = "GOA_bathy",
                                      fun = "sum",
                                      count = F)
  
  ## Create dataframe of stratum information
  strata_poly[, names(depth_mods)] <- strata_poly_agg[, names(depth_mods)] <-
    subset(x = depth_mods, subset = manage_area == idistrict)
  
  ## Calculate the total area and perimeter of the strata.
  strata_poly_agg$AREA_KM2 <- terra::expanse(x = strata_poly_agg, unit = "km")
  strata_poly_agg$PER_KM <- terra::perim(x = strata_poly_agg) / 1000
  
  ## Append to strata_list
  strata_list <- c(strata_list, list(strata_poly_agg))
  
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
             "AREA_ID" = strata_list$stratum,
             "AREA_TYPE" = "STRATUM",
             "AREA_NAME" = paste0(strata_list$manage_area, ", ",
                                  strata_list$lower_depth_m, "-",
                                  strata_list$upper_depth_m, " m"),
             "DESCRIPTION" = paste0(strata_list$manage_area, ", ",
                                    strata_list$lower_depth_m, "-",
                                    strata_list$upper_depth_m, " m"),
             "AREA_KM2" = strata_list$AREA_KM2,
             "MIN_DEPTH" = strata_list$lower_depth_m,
             "MAX_DEPTH" = strata_list$upper_depth_m)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Write geopackage ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
strata_list <- terra::project(x = strata_list,  "EPSG:3338")
terra::writeVector(x = strata_list, 
                   filename = "analysis/goa_strata_2025/goa_strata_2025.gpkg", 
                   overwrite = TRUE)
