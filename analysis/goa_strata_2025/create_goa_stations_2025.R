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
# library(terra)
library(sf)
library(akgfmaps)
library(gapindex)
library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)
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
  sf::st_read(dsn = "output/goa/shapefiles/goa_trawlwable_grid.shp")
goa_stations_2023$TRAWLAB[is.na(x = goa_stations_2023$TRAWLAB)] <- "UNK"

trawl_polygons <-
  goa_stations_2023 %>% dplyr::group_by(TRAWLAB) %>% dplyr::summarize()

## `goa_grid_2025` is the new 5-km grid imported from 
## create_aigoa_grid_temporary.R
goa_grid_2025 <- 
  sf::st_read(dsn = "analysis/goa_strata_2025/goaai_grid_2025.shp")

## `goa_strata_2025` is the new 5-km grid imported from 
## create_goa_strata_2025.R
goa_strata_2025 <- 
  sf::st_read(dsn = "analysis/goa_strata_2025/goa_strata_2025.gpkg")

latlon_crs <- "+proj=longlat +datum=NAD83"

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Create new stations ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Intersect the new stratum polygons with the 5x5 km grid to create new
## station polygons. 
goa_stations_2025 <- 
  sf::st_intersection(x = goa_grid_2025[, "GRIDID"], 
                      y = goa_strata_2025[, c("NMFS_AREA", "STRATUM")])

goa_stations_2025$STATION <- paste0(goa_stations_2025$GRIDID, "-", 
                                    goa_stations_2025$STRATUM)

## Intersect the station polygons with the trawl_polygons to calculate any new 
## stations with mixed trawlability information. 
goa_stations_2025_trawl <- sf::st_intersection(x = goa_stations_2025, 
                                               y = trawl_polygons)

## Calculate area of each new station
goa_stations_2025_trawl$AREA_KM2 <-
  sf::st_area(x = goa_stations_2025_trawl) 
units(x = goa_stations_2025_trawl$AREA_KM2) <- "km^2"

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

towpaths <- sf::st_as_sf(x = towpaths)

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
nmfs <- akgfmaps::get_nmfs_areas(set.crs = "EPSG:3338")
nmfs <- nmfs[nmfs$REP_AREA %in% c(519, 610, 620, 630, 640, 650, 659),
             "REP_AREA"]

# old_deep_strata <-
#   goa_base$survey.strata[goa_base$survey.strata$STRATUM %in%
#                            c(510, 520, 530, 540, 550), ]
# 
# old_deep_strata <- sf::st_union(x = sf::st_buffer(x = old_deep_strata, 
#                                                   dist = 2*5000))
# old_deep_strata <- sf::st_intersection(x = old_deep_strata, y = nmfs)
# 
# stns_in_old_deep_strata <- sf::st_intersection(x = goa_stations_2025,
# y = old_deep_strata)
# 
# outside_legacy_footprint <- 
#   sf::st_sf(sf::st_difference(x = stns_in_old_deep_strata,
#                               y = sf::st_union(trawl_polygons)))
outside_legacy_footprint <- 
  sf::st_sf(sf::st_difference(x = goa_stations_2025,
                              y = sf::st_union(trawl_polygons)))
outside_legacy_footprint$TRAWLAB <- "UNK"
outside_legacy_footprint$AREA_KM2 <- sf::st_area(x = outside_legacy_footprint)
units(x = outside_legacy_footprint$AREA_KM2) <- "km2"

## Append the area outside the legacy footprint to `goa_stations_2025_trawl`
new_goa_stations_2025 <- dplyr::bind_rows(x = goa_stations_2025_trawl,
                                          y = outside_legacy_footprint)
new_goa_stations_2025$TRAWLAB[is.na(x = new_goa_stations_2025$TRAWLAB)] <- "UNK"

new_goa_stations_2025 <-
  new_goa_stations_2025 %>%
  dplyr::group_by(GRIDID, NMFS_AREA, STRATUM, 
                  STATION, TRAWLAB) %>% 
  dplyr::summarize()

## Recalculate total area
new_goa_stations_2025$AREA_KM2 <- sf::st_area(x = new_goa_stations_2025)
units(x = new_goa_stations_2025$AREA_KM2) <- "km2"

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Resolve stations with mixe trawlability information. First identify
##   which stations have mixed trawlability info (`stns_idx_mixed_trawl_info`)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## The default scenario for a new station is no change, Scenario 1
stns_idx_mixed_trawl_info <- 
  which(x = table(new_goa_stations_2025$STATION) > 1)
stns_mixed_trawl_info <- names(x = stns_idx_mixed_trawl_info)

updated_stations <- list()

for (istn in stns_mixed_trawl_info) { ## loop over cells -- start
  
  ## Subset stations within istn
  temp_stn <- subset(x = new_goa_stations_2025,
                     subset = new_goa_stations_2025$STATION == istn)
  
  # plot(sf::st_geometry(temp_stn),
  #      axes = F,
  #      col = c("Y" = "green", "UNK" = "grey", "N" = "red")[temp_stn$TRAWLAB])
  # plot(sf::st_geometry(towpaths), add = TRUE, lwd = 2, xpd = NA)
  
  {
    ## Scenario 3: station is a mixture of T area (with good tows paths)
    ## and either UKN or UT area. Since there is a good tow in the station,
    ## the whole station is turned to T.
    
    ## Query whether there are any good tows in the mixed station
    good_tow_in_station <- 
      sum(sf::st_intersects(x = towpaths[towpaths$PERFORMANCE == T, ],
                            y = temp_stn, 
                            sparse = F)) > 0
    
    ## If so, convert the non-T area in the station as T
    if (good_tow_in_station & any(temp_stn$TRAWLAB == "Y")) {
      
      temp_stn %>%
        dplyr::group_by(GRIDID, NMFS_AREA, STRATUM, STATION) %>% 
        dplyr::summarize() -> temp_stn
      
      temp_stn$TRAWLAB <- "Y"
      temp_stn$FLAG <- 3
      temp_stn$AREA_KM2 <- sf::st_area(x = temp_stn)
      units(x = temp_stn$AREA_KM2) <- "km2"
      
      ## and then replace the merged station in new_goa_stations_2025
      updated_stations <- c(updated_stations, list(temp_stn))
      
      print(paste("Station", istn, "in grid cell", istn, 
                  "converted to T. Finished with", 
                  which(x = stns_mixed_trawl_info == istn), "of",
                  length(x = stns_mixed_trawl_info), "instances.")))
    }
    
    ## Scenario 4-10: if there are no tows that
    if ( (!good_tow_in_station) |
         (good_tow_in_station & !any(temp_stn$TRAWLAB %in% "Y")) ) {
      
      larger_area <- temp_stn[which.max(x = temp_stn$AREA_KM2), ]
      other_area <- temp_stn[-which.max(x = temp_stn$AREA_KM2), ]
      temp_stn %>%
        dplyr::group_by(GRIDID, NMFS_AREA, STRATUM, STATION) %>% 
        dplyr::summarize() -> temp_stn
      
      ## Scenario 4: if the lesser area has an area > 5 km2, then station is
      ## turned to unknown
      if (as.numeric(x = sum(other_area$AREA_KM2)) > 5) {
        temp_stn$TRAWLAB <- "UNK"
        temp_stn$FLAG <- 4
      }
      
      ## Scenario 5:
      if (larger_area$TRAWLAB == "N" & 
          as.numeric(x = sum(other_area$AREA_KM2)) < 5) {
        temp_stn$TRAWLAB <- "N"
        temp_stn$FLAG <- 5
      }
      
      ## Scenario 6:
      if (larger_area$TRAWLAB == "UNK" & 
          any(other_area$TRAWLAB == "N")) {
        temp_stn$TRAWLAB <- "N"
        temp_stn$FLAG <- 6
      }
      
      ## Scenario 7
      if (larger_area$TRAWLAB == "UNK" & 
          any(other_area$TRAWLAB == "Y")) {
        temp_stn$TRAWLAB <- "UNK"
        temp_stn$FLAG <- 7
      }
      
      ## Scenario 8:
      if (larger_area$TRAWLAB == "Y" & 
          any(other_area$TRAWLAB == "UNK")) {
        temp_stn$TRAWLAB <- "UNK"
        temp_stn$FLAG <- 8
      }
      
      ## Scenarios 9 and 10:
      if (larger_area$TRAWLAB == "Y" & 
          any(other_area$TRAWLAB == "N")) {
        temp_stn$TRAWLAB <-
          ifelse(test = as.numeric(x = larger_area$AREA_KM2) < 5,
                 yes = "N",
                 no = "UNK")
        temp_stn$FLAG <-
          ifelse(test = as.numeric(x = larger_area$AREA_KM2) < 5,
                 yes = 9,
                 no = 10)
      }
      
      ## and then replace the merged station in new_goa_stations_2025
      updated_stations <- c(updated_stations, list(temp_stn))
      print(paste0("Station ", istn, " converted to ", 
                   temp_stn$TRAWLAB, ". Finished with ", 
                   which(x = stns_mixed_trawl_info == istn), " of ",
                   length(x = stns_mixed_trawl_info), " instances."))
    }
  }
  # } ## Loop over mixed stations -- end
} ## Loop over cells -- end

updated_stations <- do.call(dplyr::bind_rows, updated_stations)

## Update stations
new_goa_stations_2025 <- 
  dplyr::bind_rows(new_goa_stations_2025[!new_goa_stations_2025$STATION %in% 
                                           updated_stations$STATION, ],
                   updated_stations)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Recalculate area of the new stations
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
new_goa_stations_2025$AREA_KM2 <- sf::st_area(x = new_goa_stations_2025)
units(x = new_goa_stations_2025$AREA_KM2) <- "km2"

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Save to geopackage
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sf::st_write(obj = new_goa_stations_2025,
             dsn = "analysis/goa_strata_2025/goa_stations_2025.gpkg",
             append = FALSE)

sf::st_write(obj = towpaths,
             dsn = "analysis/goa_strata_2025/towpaths.gpkg",
             append = FALSE)
