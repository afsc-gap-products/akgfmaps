##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Create GOA Stations
## Author:        Zack Oyafuso (zack.oyafuso@noaa.gov)
## Description:
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Restart R Session before running
rm(list = ls())

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import libraries and connect to Oracle
##   Make sure you are connected to VPN or NOAA Internal Network
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(terra)
library(akgfmaps)
library(gapindex)
library(rmapshaper)
# library(devtools)
# devtools::install_github("afsc-gap-products/navmaps")

channel <- gapindex::get_connected(check_access = FALSE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import Input Data
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## `goa_base` are basic shape layers from the akgfmaps package
goa_base <- akgfmaps::get_base_layers(select.region = "goa", 
                                      set.crs = "EPSG:3338")

## If runnning for first time, use the navmaps package to create a shapefile
## of historical GOA stations with trawlability information
if (!file.exists("output/goa/shapefiles/goa_trawlwable_grid.shp")) 
  navmaps::make_trawlable(region = 'goa', channel = channel)

## `goa_stations_2023` is the historical 2023 GOA stations with trawlability
## information. `trawl_polygons` is a shapefile that aggregate the historical
## GOA station geometries by trawlability status 
goa_stations_2023 <- 
  terra::vect(x = "output/goa/shapefiles/goa_trawlwable_grid.shp")
trawl_polygons <- terra::aggregate(x = goa_stations_2023, 
                                   by = "TRAWLAB")[, "TRAWLAB"]

## `goa_grid_2025` is the new 5-km grid imported from 
## create_aigoa_grid_temporary.R
goa_grid_2025 <- 
  terra::vect(x = "analysis/goa_strata_2025/goaai_grid_2025.shp")

## `goa_strata_2025` is the new 5-km grid imported from 
## create_goa_strata_2025.R
goa_strata_2025 <- 
  terra::vect(x = "analysis/goa_strata_2025/goa_strata_2025.gpkg")

goa_strata_2025 <- terra::vect(rmapshaper::ms_simplify(input = sf::st_as_sf(goa_strata_2025), keep_shapes = T, keep = 0.05))

latlon_crs <- "+proj=longlat +datum=NAD83"

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Create new stations ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Intersect the new stratum polygons with the 5x5 km grid to create new
## station polygons. 
goa_stations_2025 <- 
  terra::intersect(x = goa_grid_2025[, "GRIDID"], 
                   y = goa_strata_2025[, c("NMFS_AREA", "STRATUM")])
goa_stations_2025$STATION <- paste0(goa_stations_2025$GRIDID, "-", 
                                    goa_stations_2025$STRATUM)

## Intersect the station polygons with the trawl_polygons to calculate any new 
## stations with mixed trawlability information. 
goa_stations_2025_trawl <- terra::intersect(x = goa_stations_2025, 
                                            y = trawl_polygons)

## Calculate perimeter of each new station
# goa_stations_2025_trawl$PERIM_KM <- 
#   terra::perim(x = goa_stations_2025_trawl) / 1e3

## Calculate area of each new station
goa_stations_2025_trawl$AREA_KM2 <-
  terra::expanse(x = goa_stations_2025_trawl) / 1e6

## Calculate centroid lat/lon location each new station
# goa_stations_2025_trawl[, c("LONGITUDE_DD", "LATITUDE_DD")] <-
#   terra::geom(terra::centroids(x = terra::project(x = goa_stations_2025_trawl,
#                                                   y = latlon_crs),
#                                inside = TRUE))[, c("x", "y")]

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import tow data
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
towpaths <- terra::vect(x = "analysis/goa_strata_2025/towpaths/towpaths.shp")

## Import haul data from 1990. This is the start of the time series so
## we will only use towpaths from then.
goa_hauls_from_1990 <-
  RODBC::sqlQuery(channel = channel,
                  query = "SELECT FLOOR(CRUISE / 100) AS YEAR,
                           HAULJOIN, STATIONID,
                           CASE
                            WHEN PERFORMANCE >= 0 THEN 'TRUE'
                            WHEN PERFORMANCE < 0 THEN 'FALSE'
                           END AS PERFORMANCE
                           FROM RACEBASE.HAUL
                           WHERE REGION = 'GOA'
                           AND CRUISE >= 199000
                           ORDER BY YEAR")
names(x = towpaths) <- "HAULJOIN"
towpaths <- merge(x = towpaths,
                  y = goa_hauls_from_1990,
                  by = "HAULJOIN")
towpaths <- terra::project(x = towpaths, "EPSG:3338")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   There are some station areas that are cut off due to being at the outside
##   of the legacy goa footprint. The main reason this happens is becuase 
##   the deeper end of the survey footprint (1000 m) extends past the legacy 
##   GOA survey footprint and thus is cut off when the trawlability information
##   from the legacy GOA stations are intersected with the new GOA stations. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Isolate the area in `goa_stations_2025` is truncated by 
## `goa_stations_2025_trawl`, label these areas with unknown trawlability,
## and calculate the area
outside_legacy_footprint <- terra::vect(
  x = rmapshaper::ms_erase(target = sf::st_as_sf(goa_stations_2025),
                           erase = sf::st_as_sf(goa_stations_2025_trawl),
                           remove_slivers = TRUE)
)
outside_legacy_footprint$TRAWLAB <- "UNK"
outside_legacy_footprint$AREA_KM2 <- 
  terra::expanse(x = outside_legacy_footprint, "km")

## Append the area outside the legacy footprint to `goa_stations_2025_trawl`
new_goa_stations_2025 <- rbind(goa_stations_2025_trawl,
                               outside_legacy_footprint)
new_goa_stations_2025$TRAWLAB[is.na(x = new_goa_stations_2025$TRAWLAB)] <- "UNK"

## Aggregate new_goa_stations_2025 by station and trawlability
new_goa_stations_2025 <- 
  terra::aggregate(x = new_goa_stations_2025, 
                   by = c("GRIDID", "NMFS_AREA", "STRATUM", 
                          "STATION", "TRAWLAB"))[c("GRIDID", "NMFS_AREA", 
                                                   "STRATUM", "STATION", 
                                                   "TRAWLAB")]
## Recalculate total area
new_goa_stations_2025$AREA_KM2 <- 
  terra::expanse(x = new_goa_stations_2025, "km")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Resolve stations with mixe trawlability information. First identify
##   which stations have mixed trawlability info (`stns_idx_mixed_trawl_info`)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## The default scenario for a new station is no change, Scenario 1
new_goa_stations_2025$FLAG <- 1

stns_idx_mixed_trawl_info <- 
  which(x = table(new_goa_stations_2025$STATION) > 1)
stns_mixed_trawl_info <- names(x = stns_idx_mixed_trawl_info)

for (icell in stns_mixed_trawl_info[-c(1:5394)]) { ## loop over cells -- start
  
  ## Subset stations within icell
  temp_stn <- subset(x = new_goa_stations_2025,
                     subset = new_goa_stations_2025$STATION == icell)
  
  plot(temp_stn,
       axes = F,
       col = c("Y" = "green", "UNK" = "grey", "N" = "red")[temp_stn$TRAWLAB])
  plot(towpaths, add = TRUE, lwd = 2, xpd = NA)

  
  {
    ## Scenario 3: station is a mixture of T area (with good tows paths)
    ## and either UKN or UT area. Since there is a good tow in the station,
    ## the whole station is turned to T.
    
    ## Query whether there are any good tows in the mixed station
    good_tow_in_station <-
      any(terra::relate(x = towpaths[towpaths$PERFORMANCE == T, ],
                        y = temp_stn,
                        relation = "intersects"))
    
    ## If so, convert the non-T area in the station as T
    if (good_tow_in_station & any(temp_stn$TRAWLAB == "Y")) {
      trawl_area <- subset(x = temp_stn,
                           subset = temp_stn$TRAWLAB == "Y")
      non_trawl_area <- subset(x = temp_stn,
                               subset = temp_stn$TRAWLAB != "Y")
      ## if the non-trawlable area consists of UKN and UT areas,
      ## then merge and dissolve into one geometry
      if (nrow(x = non_trawl_area) > 1)
        non_trawl_area <-
        terra::aggregate(x = non_trawl_area)
      
      temp_combined_geo <-
        tryCatch(
          {
            terra::combineGeoms(x = trawl_area,
                                y =  non_trawl_area)
          },
          error = function(cond){
            message("Cannot merge, skipping.")
            return(trawl_area)
          }
        )

      temp_combined_geo$FLAG <- 3
      ## and then replace the merged station in new_goa_stations_2025
      new_goa_stations_2025 <-
        new_goa_stations_2025[new_goa_stations_2025$STATION != icell]
      new_goa_stations_2025 <- rbind(new_goa_stations_2025,
                                     temp_combined_geo)
      print(paste("Station", icell, "in grid cell", icell,
                  "converted to TRAWLABLE"))
      next
    }
    ## Scenario 4-10: if there are no tows that
    if ((!good_tow_in_station) |
        (good_tow_in_station & !any(temp_stn$TRAWLAB %in% "Y")) ) {
      
      larger_area <- temp_stn[which.max(x = temp_stn$AREA_KM2)]
      other_area <- temp_stn[-which.max(x = temp_stn$AREA_KM2)]
      
      if (nrow(x = other_area) > 1) {
        other_area <- other_area[order(x = other_area$AREA_KM2,
                                       decreasing = T)]
        other_area$TRAWLAB <- "UNK"
        
        other_area <- 
          tryCatch(
            {
              other_area <- terra::combineGeoms(x = other_area[1],
                                                y =  other_area[2])
            },
            error = function(cond){
              message("Cannot merge, skipping.")
              return(other_area[1])
            }
          )
        
        
        other_area$AREA_KM2 <- terra::expanse(other_area, "km") 
      }
      
      temp_combined_geo <- larger_area
      other_area_disagg <- terra::disagg(x = other_area)
      for (ifrag in 1:nrow(x = other_area_disagg)) {
        tryCatch(
          {
            temp_combined_geo <- 
              terra::combineGeoms(x = temp_combined_geo,
                                  y =  other_area_disagg[ifrag])
          },
          error = function(cond){
            message("Cannot merge, skipping.")
          }
        )
        
        
      }
      
      ## Scenario 4: if the lesser area has an area > 5 km2, then station is
      ## turned to unknown
      if (other_area$AREA_KM2 > 5) {
        temp_combined_geo$TRAWLAB <- "UNK"
        temp_combined_geo$FLAG <- 4
      }
      
      ## Scenario 5:
      if (larger_area$TRAWLAB == "N" & other_area$AREA_KM2 < 5) {
        temp_combined_geo$TRAWLAB <- "N"
        temp_combined_geo$FLAG <- 5
      }
      
      ## Scenario 6:
      if (larger_area$TRAWLAB == "UNK" & other_area$TRAWLAB == "N") {
        temp_combined_geo$TRAWLAB <- "N"
        temp_combined_geo$FLAG <- 6
      }
      
      ## Scenario 7
      if (larger_area$TRAWLAB == "UNK" & other_area$TRAWLAB == "Y") {
        temp_combined_geo$TRAWLAB <- "UNK"
        temp_combined_geo$FLAG <- 7
      }
      
      ## Scenario 8:
      if (larger_area$TRAWLAB == "Y" & other_area$TRAWLAB == "UNK") {
        temp_combined_geo$TRAWLAB <- "UNK"
        temp_combined_geo$FLAG <- 8
      }
      
      ## Scenarios 9 and 10:
      if (larger_area$TRAWLAB == "Y" & other_area$TRAWLAB == "N") {
        temp_combined_geo$TRAWLAB <-
          ifelse(test = larger_area$AREA_KM2 < 5,
                 yes = "N",
                 no = "UNK")
        temp_combined_geo$FLAG <-
          ifelse(test = larger_area$AREA_KM2 < 5,
                 yes = 9,
                 no = 10)
      }
      
      ## and then replace the merged station in new_goa_stations_2025
      new_goa_stations_2025 <-
        new_goa_stations_2025[new_goa_stations_2025$STATION != icell]
      new_goa_stations_2025 <- rbind(new_goa_stations_2025,
                                     temp_combined_geo)
      print(paste0("STATION ", icell, " in grid cell ", 
                   temp_combined_geo$GRIDID, " converted to ", 
                   temp_combined_geo$TRAWLAB, ". Finished with ", 
                   which(x = stns_mixed_trawl_info == icell), " of ",
                   length(x = stns_mixed_trawl_info), " instances."))
      next
    }
  }
  # } ## Loop over mixed stations -- end
  
  ## Scenario 2: If there is a speck...
  if (any(temp_stn$AREA_KM2 < 1)) {
    ## Otherwise, subset the station from x
    major_stn <- temp_stn[which.max(x = temp_stn$AREA_KM2)]
    ## and subset the speck station
    speck_stn <- temp_stn[temp_stn$AREA_KM2 < 1]
    if (nrow(x = speck_stn) > 1)
      speck_stn <- terra::combineGeoms(x = speck_stn[1], y = speck_stn[2])
    ## then absorb the speck station into the bigger station
    temp_combined_geo <- terra::combineGeoms(x = major_stn, y = speck_stn)
    temp_combined_geo$FLAG <- 2
    
    ## and then replace the merged station in new_goa_stations_2025
    new_goa_stations_2025 <-
      new_goa_stations_2025[new_goa_stations_2025$STATION != icell]
    new_goa_stations_2025 <- rbind(new_goa_stations_2025,
                                   temp_combined_geo)
    print(paste("STATION", icell, "in grid cell", temp_combined_geo$GRIDID, 
                "had a speck"))
    next
    
  } 
  
} ## Loop over cells -- end

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Recalculate area and perimeter of each new station
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
new_goa_stations_2025$AREA_KM2 <-
  terra::expanse(x = new_goa_stations_2025) / 1000 / 1000
new_goa_stations_2025$PERIMETER_KM <-
  terra::perim(x = new_goa_stations_2025) / 1000

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Save to geopackage
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
terra::writeVector(x = new_goa_stations_2025,
                   filename = paste0("analysis/goa_strata_2025/",
                                     "goa_stations_2025.gpkg"),
                   overwrite = T)
terra::writeVector(x = towpaths,
                   filename = paste0("analysis/goa_strata_2025/",
                                     "towpaths.gpkg"),
                   overwrite = T)
