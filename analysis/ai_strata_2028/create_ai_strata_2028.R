##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project: Update Aleutian Islands strata with updated bathymetry
## Author:  Zack Oyafuso (zack.oyafuso@noaa.gov)
## Notes:   The AI bottom trawl survey footprint is first sectioned into four
##          INPFC areas: Western AI (WAI), Central AI (CAI), Eastern AI (EAI), 
##          Southern Bering Sea (SBS). Within each INPFC area, there are further
##          subareas delinations by longitude (East/West) or depending on 
##          whether the area faces the Bering Sea (North) or Pacific Ocean
##          (South). Within each subarea, strata are defined by four depth
##          bins: 1-100 m; 101-200 m; 201-300 m; 301-500 m. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Import libraries
library(terra)
library(akgfmaps) ## version 4.1.2
library(readxl)
library(gapindex)

chl <- gapindex::get_connected(check_access = FALSE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import Base Data
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Import Mark's AI grid
ai_bathy <- terra::rast("Y:/RACE_GF/Oyafuso/AI New Strata/AI/ai_grid_100m/") |>
  terra::project("EPSG:3338")

## Import AI base layers from akgfmaps package (version 4.1.2)
ai_base_layers <- akgfmaps::get_base_layers(select.region = "AI",
                                            set.crs = "EPSG:3338") 
old_strata <- terra::vect(x = ai_base_layers$survey.strata)
inpfc <- terra::vect(x = ai_base_layers$inpfc.strata)
inpfc_areas <- inpfc$AREA_NAME

## Import stratum table with names of subareas within INPFC areas
strata_table <- RODBC::sqlQuery(
  channel = chl,
  query = "
  SELECT   INPFC.AREA_NAME AS INPFC_AREA, 
           STRATUM_INFO.AREA_NAME AS STRATUM_NAME, AI_STRATUM.STRATUM, 
           STRATUM_INFO.DEPTH_MIN_M, STRATUM_INFO.DEPTH_MAX_M
  
  FROM     GAP_PRODUCTS.STRATUM_GROUPS AI_STRATUM
  
           -- Filter only INPFC groupings  
  JOIN     (SELECT * FROM GAP_PRODUCTS.AREA
           WHERE AREA_TYPE = 'INPFC'
           AND SURVEY_DEFINITION_ID = 52
           AND DESIGN_YEAR = 1991) INPFC
           ON AI_STRATUM.AREA_ID = INPFC.AREA_ID
           
  JOIN     (SELECT * FROM GAP_PRODUCTS.AREA
           WHERE SURVEY_DEFINITION_ID = 52
           AND DESIGN_YEAR = 1991) STRATUM_INFO
           ON AI_STRATUM.STRATUM = STRATUM_INFO.AREA_ID
           
  ORDER BY STRATUM
"
)
subareas <- unique(x = strata_table$STRATUM_NAME)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Create first round of stratum polygons: within each INPFC area, classify
##   raster values into four depth bins:
##   1: < 100 m; 2: 100 - 200 m; 3: 200 - 300 m; 4: > 300 m
##   Save as (ai_strata_step1.gpkg)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Crop and mask the bathymetry layer to only the INPFC areas
ai_bathy_masked <- terra::crop(x = ai_bathy,
                               y = inpfc, 
                               mask = TRUE) |> round()

## Loop over inpfc areas, reclassify strata based on the raster values
inpfc_DEPTH_MAX_Ms <- terra::vect()
for (iarea in inpfc_areas) {
  
  ## Subset the stratum polygons, stratum IDs, and depth breaks
  temp_strata <- strata_table$STRATUM[strata_table$INPFC_AREA == iarea]
  temp_subarea <- terra::vect(subset(x = ai_base_layers$survey.strata, 
                                     subset = STRATUM %in% temp_strata))
  
  ## Mask the bathymetry raster using the subarea polygon
  temp_raster <- terra::crop(x = ai_bathy_masked, y = temp_subarea, mask = TRUE)
  
  ## Set the depth breaks for each stratum
  depth_breaks <- cbind(from = c(-10000, 100, 200, 300),
                        to = c(100, 200, 300, 10000),
                        becomes = c(100, 200, 300, 500)) |>
    matrix(ncol = 3,
           dimnames = list(NULL, c("from", "to", "becomes")))
  
  ## Classify raster values into depth bins and then polygonize like values
  stratum_polys <- 
    terra::classify(x = temp_raster,
                    rcl = depth_breaks, 
                    right = FALSE, 
                    include.lowest = FALSE, 
                    others = temp_strata[length(x = temp_strata)]) |>
    terra::as.polygons() |> 
    terra::intersect(y = terra::aggregate(temp_subarea) )
  names(stratum_polys) <- "DEPTH_MAX_M"
  stratum_polys$INPFC_AREA <- iarea
  
  ## Append new strata to new_ai_strata object
  inpfc_DEPTH_MAX_Ms <- rbind(inpfc_DEPTH_MAX_Ms, stratum_polys)
  
  cat("Done with", iarea, "\n")
}

100 * (sum(terra::expanse(x = inpfc_DEPTH_MAX_Ms)) - 
         sum(terra::expanse(x = old_strata)) ) / 
  sum(terra::expanse(x = old_strata))
## 0.61% difference in area, mostly at the coastline

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   In the Western Aleutians, the -185 W (175 E) longitude separates the 
##   strata into West and East portions. -183 W (177 E) separates the E Western
##   AI subarea from the Central Aleutians
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wai_footprint <- 
  merge(x = old_strata,
        y = strata_table[strata_table$STRATUM_NAME %in% 
                           c("W Western Aleutians",
                             "E Western Aleutians"), 
                         c("STRATUM", "STRATUM_NAME", "INPFC_AREA")],
        by = "STRATUM") |>
  terra::aggregate(by = "STRATUM_NAME") 

wai_strata <-  terra::intersect(
  x = wai_footprint[, c("INPFC_AREA", "STRATUM_NAME")],
  y = inpfc_DEPTH_MAX_Ms[inpfc_DEPTH_MAX_Ms$INPFC_AREA == "Western Aleutians", ]
) 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   In the Central Aleutian Islands, the -180 W longitude separates the 
##   strata into West and East portions except for those strata around
##   Semisopochnoi and Petrel Bank. The -183 W and -177 W longitude are the 
##   western and eastern boundaries of the CAI.
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cai_pacific_footprint <- 
  merge(x = old_strata, 
        y = strata_table[strata_table$STRATUM_NAME %in% 
                           c("SE Central Aleutians",
                             "SW Central Aleutians"), 
                         c("STRATUM", "STRATUM_NAME", "INPFC_AREA")],
        by = "STRATUM") |>
  terra::aggregate(by = "STRATUM_NAME") |>
  subset(select = c("INPFC_AREA", "STRATUM_NAME"), NSE = TRUE)

cai_bering_footprint <- 
  merge(x = old_strata, 
        y = strata_table[strata_table$STRATUM_NAME == "N Central Aleutians", 
                         c("STRATUM", "STRATUM_NAME", "INPFC_AREA")],
        by = "STRATUM") |> 
  terra::aggregate() 
cai_bering_footprint$INPFC_AREA <- "Central Aleutians"
cai_bering_footprint$STRATUM_NAME <- "N Central Aleutians"

petrel_bank_strata <- 
  strata_table$STRATUM[strata_table$STRATUM_NAME == "Petrel Bank"]
petrel_bank_footprint <- old_strata[old_strata$STRATUM %in% petrel_bank_strata] |>
  terra::aggregate()
petrel_bank_footprint$INPFC_AREA <- "Central Aleutians"
petrel_bank_footprint$STRATUM_NAME <- "Petrel Bank"

cai_strata <- terra::intersect(
  x = rbind(cai_bering_footprint, cai_pacific_footprint, petrel_bank_footprint),
  y = inpfc_DEPTH_MAX_Ms[, "DEPTH_MAX_M"]
)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   In the Eastern Aleutian Islands, the -174 W longitude separates the 
##   Pacific-side strata into West and East portions. On the Bering side, 
##   the shallower strata (1 - 100 m, 101 - 200 m, and 201 - 300 m) into West 
##   and East portions. The deepest stratum on the Bering side (301 - 500 m) 
##   are not separated by the -174 W longitude. The -170 W longitude separates 
##   the Eastern Aleutian Islands from the Southern Bering Sea.
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
eai_pacific_footprint <- 
  merge(x = old_strata, 
        y = strata_table[strata_table$STRATUM_NAME %in% 
                           c("SE Eastern Aleutians",
                             "SW Eastern Aleutians"), 
                         c("STRATUM", "STRATUM_NAME", "INPFC_AREA")],
        by = "STRATUM") |>
  terra::aggregate(by = "STRATUM_NAME") |>
  subset(select = c("INPFC_AREA", "STRATUM_NAME"), NSE = TRUE)

eai_pacific_strata <- terra::intersect(
  x = eai_pacific_footprint,
  y = inpfc_DEPTH_MAX_Ms[, "DEPTH_MAX_M"]
) 

longitude_174W <- rbind(
  ## Box that contains NW Eastern Aleutian Subarea
  terra::vect(x = matrix(c( -174, 51,
                            -174, 54,
                            -177, 54,
                            -177, 51,
                            -174, 51), ncol = 2, byrow = TRUE),
              crs = "EPSG:4326",
              type = "polygons", 
              atts = data.frame("INPFC_AREA" = "Eastern Aleutians",
                                "STRATUM_NAME" = "NW Eastern Aleutians")),
  ## Box that contains NE Eastern Aleutian Subarea
  terra::vect(x = matrix(c( -174, 51,
                            -174, 54,
                            -170, 54,
                            -170, 51,
                            -174, 51), ncol = 2, byrow = TRUE),
              crs = "EPSG:4326",
              type = "polygons", 
              atts = data.frame("INPFC_AREA" = "Eastern Aleutians",
                                "STRATUM_NAME" = "NE Eastern Aleutians"))) |>
  terra::project("EPSG:3338")

eai_bering_footprint <- 
  merge(x = old_strata, 
        y = strata_table[strata_table$STRATUM_NAME %in% 
                           c("NE Eastern Aleutians",
                             "NW Eastern Aleutians",
                             "Combined Eastern Aleutian Islands"), 
                         c("STRATUM", "STRATUM_NAME", "INPFC_AREA")],
        by= "STRATUM") |>
  terra::aggregate() |>
  terra::intersect(y = longitude_174W )

eai_bering_strata <- terra::intersect(
  x = eai_bering_footprint,
  y = inpfc_DEPTH_MAX_Ms[, "DEPTH_MAX_M"]
)
eai_bering_strata <- subset(x = eai_bering_strata,
                            subset = terra::expanse(eai_bering_strata) > 1)

eai_bering_strata_deep <- 
  subset(x = eai_bering_strata,
         subset = STRATUM_NAME %in% 
           c("NE Eastern Aleutians", "NW Eastern Aleutians") & 
           DEPTH_MAX_M == 500, NSE = TRUE) |>
  aggregate() 
eai_bering_strata_deep$INPFC_AREA <- "Eastern Aleutians"
eai_bering_strata_deep$STRATUM_NAME <- "Combined Eastern Aleutian Islands"
eai_bering_strata_deep$DEPTH_MAX_M <- 500

eai_bering_strata <- rbind(
  subset(x = eai_bering_strata,
         subset = DEPTH_MAX_M != 500, NSE = TRUE),
  eai_bering_strata_deep
)

eai_strata <- rbind(eai_bering_strata, eai_pacific_strata)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   In the Southern Bering Sea, the -168 W longitude separates the shallower 
##   strata (1 - 100 m and 101 - 200 m) into West and East portions. The deeper
##   strata (200 - 300 m and 300 - 500 m depths) are not separated by the 
##   -168 W longitude. The -170 W longitude separates the Southern Bering Sea
##   from the Eastern Aleutian Islands. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
longitude_168W <- rbind(
  ## Box that contains W Southern Bering Sea
  terra::vect(x = matrix(c( -168, 52,
                            -168, 55,
                            -170, 55,
                            -170, 52,
                            -168, 52), ncol = 2, byrow = TRUE),
              crs = "EPSG:4326",
              type = "polygons",
              atts = data.frame("STRATUM_NAME" = "W Southern Bering Sea")),
  ## Box that contains E Southern Bering Sea
  terra::vect(x = matrix(c( -168, 52,
                            -168, 55,
                            -165, 55,
                            -165, 52,
                            -168, 52), ncol = 2, byrow = TRUE),
              crs = "EPSG:4326",
              type = "polygons",
              atts = data.frame("STRATUM_NAME" = "E Southern Bering Sea"))
) |> terra::project("EPSG:3338") 

sbs_strata <- terra::intersect(
  x = longitude_168W,
  y = inpfc_DEPTH_MAX_Ms[inpfc_DEPTH_MAX_Ms$INPFC_AREA == "Southern Bering Sea", ]
) 

sbs_shallow_strata <- sbs_strata[sbs_strata$DEPTH_MAX_M %in% c(100, 200)]
sbs_deep_strata <- aggregate(sbs_strata[sbs_strata$DEPTH_MAX_M %in% c(300, 500)], 
                             by = "DEPTH_MAX_M") 
sbs_deep_strata$STRATUM_NAME <- "Combined Southern Bering Sea"

## Append SBS to the new strata object
sbs_strata <- rbind(sbs_shallow_strata, 
                    sbs_deep_strata[, names(x = sbs_shallow_strata)])

new_ai_strata_v1 <- 
  merge(x = rbind(wai_strata, cai_strata, 
                  eai_strata, sbs_strata)[, c("STRATUM_NAME", "DEPTH_MAX_M")],
        y = strata_table,
        by = c("STRATUM_NAME", "DEPTH_MAX_M"))

writeVector(x = new_ai_strata_v1, 
            filename = "Y:/RACE_GF/Oyafuso/AI New Strata/new_ai_strata_v1.gpkg",
            overwrite = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   There is a lot of holes around the coastline (new_ai_strata_2.gpkg)
##   We update the first round of stratum polygons by absorbing specks of 
##   non-classified areas around the coast to the nearest stratum polygons
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
new_ai_strata_v2 <- new_ai_strata_v1

for (iarea in inpfc_areas) {
  temp_subarea <- new_ai_strata_v2[new_ai_strata_v2$INPFC_AREA == iarea]
  coastal_bits <-
    terra::erase(x = old_strata[old_strata$STRATUM %in% temp_subarea$STRATUM],
                 y = temp_subarea)
  
  ## Add specks < 1000 m2 
  specks <- terra::disagg(x = coastal_bits)
  
  new_ai_strata_v2 <- rbind(new_ai_strata_v2,
                            specks[terra::expanse(specks) >= 1000, "STRATUM"])
}

new_ai_strata_v2 <- terra::aggregate(x = new_ai_strata_v2, by = "STRATUM")
new_ai_strata_v2 <- merge(x = new_ai_strata_v2[, "STRATUM"],
                          y = strata_table,
                          by = "STRATUM")

writeVector(x = new_ai_strata_v2, 
            filename = "Y:/RACE_GF/Oyafuso/AI New Strata/new_ai_strata_v2.gpkg", 
            overwrite = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   After the second round of cleaning, there are still small specks of 
##   stratum polygons dispersed throughout the survey area. For any speck 
##   < 1 km2 (arbitrarily set), absorb into the largest adjacent stratum polygon.
##   File: new_ai_strata_3.gpkg
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
area_change <- 1
new_ai_strata_v2.5 <- new_ai_strata_v2

while(area_change != 0) {
  
  ## Disaggregate the stratum polygons and query any polygons < 1 km^2. 
  specks <- terra::disagg(x = new_ai_strata_v2.5)
  specks <- specks[terra::expanse(x = specks)/1e6 < 1, ]
  
  temp_strata <- terra::vect()
  for (iarea in subareas) { ## loop over subareas -- start
    
    ## 
    temp_subarea <- new_ai_strata_v2.5[new_ai_strata_v2.5$STRATUM_NAME == iarea]
    temp_specks <- specks[specks$STRATUM_NAME == iarea, ]
    
    combined_areas <- rbind(erase(temp_subarea, temp_specks), temp_specks)
    combined_areas$area <- terra::expanse(x = combined_areas) / 1e6
    
    neighbors <- terra::adjacent(x = combined_areas, 
                                 type = "rook", 
                                 symmetrical = T, 
                                 pairs = F)
    
    ## Query any specks (polygons < 1 km2) that have a neighboring polygon
    speck_idx <- which(combined_areas$area < 1 & 
                         rowSums(x = neighbors) != 0)
    
    ## Loop over the specks and absorb them into the stratum with the 
    ## largest area
    speck_report <- data.frame()
    cat(paste(iarea, "--", length(x = speck_idx), "specks\n"))
    for( temp_speck in speck_idx ) {
      
      ## Query the stratum polygons that neighbor temp_speck
      adj_polys <- which(neighbors[temp_speck, ] == T)
      ## The "parent" stratum polygon is the largest stratum polygon that 
      ## neighbors temp_speck
      parent_poly <- adj_polys[which.max(x = combined_areas$area[adj_polys])]
      
      speck_report <- 
        rbind(speck_report, 
              data.frame(
                speck = temp_speck,
                speck_stratum = NA,
                parent_poly = parent_poly,
                parent_stratum = combined_areas$STRATUM[parent_poly])
        )
      
      ## Assign the stratum of the speck to its "parent" stratum polygon
      combined_areas$STRATUM[temp_speck] <- 
        combined_areas$STRATUM[parent_poly]
      
    }
    
    ## Reaggregate stratum polygons, merging the specks with their respective
    ## parent stratum polygons
    combined_areas <- terra::aggregate(x = combined_areas,
                                       by = "STRATUM",
                                       fun = "sum",
                                       count = F,
                                       na.rm = TRUE)
    temp_strata <- rbind(temp_strata, combined_areas)
  } ## loop over subareas -- end
  
  ## Calculate difference in updated stratum areas and rewrite 
  ## new_ai_strata_v2.5. If area_change != 0, this loop will repeat. 
  area_change <- sum(terra::expanse(x = temp_strata)) - 
    sum(terra::expanse(x = new_ai_strata_v2.5))
  new_ai_strata_v2.5 <- temp_strata
}

new_ai_strata_v3 <- new_ai_strata_v2.5[, c("STRATUM", "STRATUM_NAME")]
new_ai_strata_v3 <- merge(x = new_ai_strata_v3[, "STRATUM"],
                          y = strata_table,
                          by = "STRATUM")

writeVector(x = new_ai_strata_v3, 
            filename = "Y:/RACE_GF/Oyafuso/AI New Strata/new_ai_strata_v3.gpkg", 
            overwrite = TRUE)

sum(expanse(new_ai_strata_v3))
sum(expanse(old_strata))
(sum(expanse(new_ai_strata_v3)) - sum(expanse(old_strata)) ) / 
  sum(expanse(old_strata)) * 100

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Most of the specks have been absorbed except for regions where the deep 
##   strata are merged across subareas (e.g., strata 593, 793). For any speck 
##   < 1 km2, absorb into the largest adjacent stratum polygon, even if the 
##   stratum now jumps subareas (new_ai_strata_5.gpkg)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
new_ai_strata_v3.5 <- new_ai_strata_v3
area_change <- 1
while(area_change != 0) {
  specks <- terra::disagg(x = new_ai_strata_v3.5)
  specks <- specks[terra::expanse(x = specks)/1e6 < 1, ]
  
  temp_strata <- terra::vect()
  for (iarea in inpfc_areas) {
    temp_subarea <- new_ai_strata_v3.5[new_ai_strata_v3.5$INPFC_AREA == iarea]
    temp_specks <- specks[specks$INPFC_AREA == iarea, ]
    
    combined_areas <- rbind(erase(temp_subarea, temp_specks), temp_specks)
    combined_areas$area <- terra::expanse(x = combined_areas) / 1e6
    
    neighbors <- terra::adjacent(x = combined_areas, 
                                 type = "rook", 
                                 symmetrical = T, 
                                 pairs = F)
    
    ## Query any specks (polygons < 1 km2) that have a neighboring polygon
    speck_idx <- which(combined_areas$area < 1 & 
                         rowSums(x = neighbors) != 0)
    
    speck_report <- data.frame()
    cat(paste(iarea, "--", length(x = speck_idx), "specks\n"))
    
    for( temp_speck in speck_idx ) {
      adj_polys <- which(neighbors[temp_speck, ] == T)
      parent_poly <- adj_polys[which.max(x = combined_areas$area[adj_polys])]
      
      speck_report <- 
        rbind(speck_report, 
              data.frame(
                speck = temp_speck,
                speck_stratum = NA,
                parent_poly = parent_poly,
                parent_stratum = combined_areas$STRATUM[parent_poly])
        )
      
      combined_areas$STRATUM[temp_speck] <- 
        combined_areas$STRATUM[parent_poly]
      
    }
    
    combined_areas <- terra::aggregate(x = combined_areas,
                                       by = "STRATUM",
                                       fun = "sum",
                                       count = F,
                                       na.rm = TRUE)
    temp_strata <- rbind(temp_strata, combined_areas)
  }
  
  ## Calculate difference in updated area and rewrite updated_ai_strata
  area_change <- sum(terra::expanse(x = new_ai_strata_v3.5)) - 
    sum(terra::expanse(x = temp_strata))
  new_ai_strata_v3.5 <- temp_strata
}

new_ai_strata_v4 <- new_ai_strata_v3.5[, c("STRATUM", "STRATUM_NAME")]
new_ai_strata_v4 <- merge(x = new_ai_strata_v4[, "STRATUM"],
                          y = strata_table,
                          by = "STRATUM")

writeVector(x = new_ai_strata_v4, 
            filename = "Y:/RACE_GF/Oyafuso/AI New Strata/new_ai_strata_v4.gpkg", 
            overwrite = TRUE)

# sum(expanse(new_ai_strata_v4))
# sum(expanse(old_strata))
# (sum(expanse(new_ai_strata_v4)) - sum(expanse(old_strata)) ) / sum(expanse(old_strata)) * 100
# 
# ## Compare areas
# new_ai_strata_v4$AREA_M2 <- terra::expanse(x = new_ai_strata_v4)
# compare_areas <- 
#   merge(x = new_ai_strata_v4[, c("STRATUM", "STRATUM_NAME", "AREA_M2",
#                                  "DEPTH_MIN_M", "DEPTH_MAX_M")], 
#         y = old_strata[, c("STRATUM", "AREA_M2")], 
#         by = "STRATUM", suffixes = c("_new", "_old"))
# compare_areas$AREA_DIFF <- compare_areas$AREA_M2_new - compare_areas$AREA_M2_old 
# compare_areas$AREA_DIFF_PERC <- round(x = compare_areas$AREA_DIFF /
#                                         compare_areas$AREA_M2_old * 100, 
#                                       digits = 3) 
# compare_areas |> as.data.frame()

writeVector(x = new_ai_strata_v4, 
            filename = "analysis/ai_strata_2028/ai_strata_2028.gpkg", 
            overwrite = TRUE)
