##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Create GOA Stations
## Author:        Zack Oyafuso (zack.oyafuso@noaa.gov)
## Description:   Given a completed 2025 stratum spatial object and a GOA
##                5-km grid, create new stations and systematically transfer
##                the trawlability information of the legacy GOA stations to 
##                the new stations. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Restart R Session before running
rm(list = ls())

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import libraries and connect to Oracle
##   Make sure you are connected to VPN or NOAA Internal Network
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(sf)
library(akgfmaps)
library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE) ## Suppress dplyr warning messages

# library(devtools)
# devtools::install_github("afsc-gap-products/navmaps")
# library(navmaps)
# library(gapindex)
# devtools::install_github("afsc-gap-products/navmaps")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import Input Data
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## `goa_base` are basic shape layers from the akgfmaps package
goa_base <- akgfmaps::get_base_layers(select.region = "goa", 
                                      set.crs = "EPSG:3338")

## If running for first time, use the navmaps package to create a shapefile
## of historical GOA stations with trawlability information
if (!file.exists("output/goa/shapefiles/goa_trawlwable_grid.shp")) {
  navmaps::make_trawlable(
    region = 'goa', 
    channel = gapindex::get_connected(check_access = FALSE)
  )
}

## Import the legacy 2023 GOA stations with trawlability info
goa_stations_2023 <- 
  sf::st_read(dsn = "output/goa/shapefiles/goa_trawlwable_grid.shp")
goa_stations_2023$TRAWLAB[is.na(x = goa_stations_2023$TRAWLAB)] <- "UNK"

## Group the legacy GOA stations by trawlability status
trawl_polygons <-
  goa_stations_2023 %>% dplyr::group_by(TRAWLAB) %>% dplyr::summarize()

## Import the new 5-km grid created using from create_aigoa_grid_2025.R
goa_grid_2025 <- 
  sf::st_read(dsn = "analysis/goa_strata_2025/goaai_grid_2025.shp")

## `goa_strata_2025` is the new 5-km grid imported from 
## create_goa_strata_2025.R
goa_strata_2025 <- 
  sf::st_read(dsn = "analysis/goa_strata_2025/goa_strata_2025.gpkg")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Create new stations ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Intersect the new stratum polygons with the 5x5 km grid to create new
## station polygons. 
# goa_stations_2025 <- 
#   sf::st_intersection(x = goa_grid_2025[, "GRIDID"], 
#                       y = goa_strata_2025[goa_strata_2025$NMFS_AREA == "Shumagin", c("NMFS_AREA", "STRATUM")])

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
goa_stations_2025_trawl$AREA_KM2 <- sf::st_area(x = goa_stations_2025_trawl) 
units(x = goa_stations_2025_trawl$AREA_KM2) <- "km^2"

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import tow data
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (!file.exists("analysis/goa_strata_2025/towpaths.gpkg")) {
  
  ## Connect to Oracle. Make sure you are connected to the NOAA internal 
  ## network or VPN.  
  channel <- gapindex::get_connected(check_access = FALSE)
  
  ## Import towpaths made from navmaps package
  towpaths <- 
    sf::st_read(dsn = "analysis/goa_strata_2025/towpaths/towpaths.shp")
  ## Transform to Alaska Albers projection
  towpaths <- sf::st_transform(x = towpaths, crs = "EPSG:3338")
  ## Id is the HAULJOIN associated with the tow
  towpaths$HAULJOIN <-towpaths$Id
  
  ## Import haul data from 1990. This is the start of the time series so
  ## we will only use towpaths from then.
  goa_hauls_from_1990 <-
    RODBC::sqlQuery(channel = gapindex::get_connected(check_access = FALSE),
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
  
  ## Merge PERFORMANCE and STATIONID fields using HAULJOIN as the key. These
  ## STATIONIDS refer to the legacy AIGOA survey grid. 
  towpaths <- merge(x = towpaths[, c("HAULJOIN", "geometry")],
                    y = goa_hauls_from_1990,
                    by = "HAULJOIN")
  
  ## Save output
  sf::st_write(obj = towpaths,
               dsn = "analysis/goa_strata_2025/towpaths.gpkg",
               append = FALSE)
} else towpaths <- sf::st_read(dsn = "analysis/goa_strata_2025/towpaths.gpkg")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   There are some station areas that are cut off due to being outside the 
##   legacy goa footprint. The main reason this happens is becuase 
##   the deeper end of the survey footprint (1000 m) extends past the legacy 
##   GOA survey footprint and thus is cut off when the trawlability information
##   from the legacy GOA stations are intersected with the new GOA stations. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Isolate the area in `goa_stations_2025` is truncated by the historical GOA
## survey footprint
outside_legacy_footprint <- 
  sf::st_sf(sf::st_difference(x = goa_stations_2025,
                              y = sf::st_union(trawl_polygons)))
## Assign these station/station bits with unknown trawlability
outside_legacy_footprint$TRAWLAB <- "UNK"
# Calculate the area of each station/station bit
outside_legacy_footprint$AREA_KM2 <- sf::st_area(x = outside_legacy_footprint)
units(x = outside_legacy_footprint$AREA_KM2) <- "km2"

## Append the area outside the legacy footprint to `goa_stations_2025_trawl`
goa_stations_2025_trawl <- dplyr::bind_rows(x = goa_stations_2025_trawl,
                                            y = outside_legacy_footprint)
goa_stations_2025_trawl$TRAWLAB[is.na(x = goa_stations_2025_trawl$TRAWLAB)] <- "UNK"

## Merge any station bits outside the legacy GOA footprint with their respective
## station ID within the legacy GOA footprint. This step creates a new 2025
## GOA survey footprint.
goa_stations_2025_trawl <-
  goa_stations_2025_trawl %>%
  dplyr::group_by(GRIDID, NMFS_AREA, STRATUM, STATION, TRAWLAB) %>% 
  dplyr::summarize()

## Recalculate total area of each station
goa_stations_2025_trawl$AREA_KM2 <- sf::st_area(x = goa_stations_2025_trawl)
units(x = goa_stations_2025_trawl$AREA_KM2) <- "km2"

## Summarize differences in total area between survey footprints
# sum(goa_stations_2025_trawl$AREA_KM2) #316768.4 km2
# sum(sf::st_area(x = goa_stations_2023)/1e6)  #320002.2 km2
# 100 * (320002.2 - 316768.4) / 320002.2 # 1.01% difference

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Resolve stations with mixed trawlability information. First identify
##   which stations have mixed trawlability info (`stns_idx_mixed_trawl_info`)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
new_goa_stations_2025 <- goa_stations_2025_trawl
## Query 2025 stations that inherited > 1 trawlability statuses from the 
## legacy GOA survey stations 
stns_mixed_trawl_info <- 
  names(x = which(x = table(new_goa_stations_2025$STATION) > 1))

## `updated_stations` will contain updated trawlability status of the stations
## within the stns_mixed_trawl_info 
updated_stations <- list()

for (istn in stns_mixed_trawl_info) { ## loop over affected stations -- start
  
  ## Subset stations within istn
  temp_stn <- subset(x = new_goa_stations_2025,
                     subset = new_goa_stations_2025$STATION == istn)
  
  # plot(sf::st_geometry(temp_stn),
  #      axes = F,
  #      col = c("Y" = "green", "UNK" = "grey", "N" = "red")[temp_stn$TRAWLAB])
  # plot(sf::st_geometry(towpaths), add = TRUE, lwd = 2, xpd = NA)
  
  
  ## Scenario 1: station is a mixture of T area (with good tows paths)
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
    temp_stn$FLAG <- 1
    temp_stn$AREA_KM2 <- sf::st_area(x = temp_stn)
    units(x = temp_stn$AREA_KM2) <- "km2"
    
    ## and then replace the merged station in new_goa_stations_2025
    updated_stations <- c(updated_stations, list(temp_stn))
    
    cat(paste("Station", istn, "in grid cell", istn, 
              "converted to T. Finished with", 
              which(x = stns_mixed_trawl_info == istn), "of",
              length(x = stns_mixed_trawl_info), "instances.\n"))
  }
  
  ## Scenario 2-8: if there are no tows that
  if ( (!good_tow_in_station) |
       (good_tow_in_station & !any(temp_stn$TRAWLAB %in% "Y")) ) {
    
    ## Subset any stns features that are either trawlable (Y) or unknown (UNK) 
    open_area <- subset(x = temp_stn, subset = TRAWLAB %in% c("UNK", "Y"))
    open_area$AREA_KM2 <- sf::st_area(x = open_area)
    units(x = open_area$AREA_KM2) <- "km2"
    
    ## Subset any untrawlable station features
    ut_area <- subset(x = temp_stn, subset = TRAWLAB %in% c("N"))
    ut_area$AREA_KM2 <- sf::st_area(x = ut_area)
    units(x = ut_area$AREA_KM2) <- "km2"
    
    ## Merge all station bits back together
    temp_stn %>%
      dplyr::group_by(GRIDID, NMFS_AREA, STRATUM, STATION) %>% 
      dplyr::summarize() -> temp_stn
    
    ## Reassign trawlable status and flag 
    
    ## Scenario 2: If the total open area is >= 5km2, there's ample space to 
    ## search for a tow, reassign station as unknown trawlability
    if (as.numeric(x = sum(open_area$AREA_KM2)) >= 5) {
      temp_stn$TRAWLAB <- "UNK"
      temp_stn$FLAG <- 2
    } else if (nrow(x = ut_area) > 0) {
      ## Scenario 3: If the total open area is < 5km2, and there is any portion 
      ## of the station that is untrawlable, turn the station to untrawlable 
      ## because the open area is < 5km2 and too small to search for a tow,
      ## effectively turning the station untrawlable.
      temp_stn$TRAWLAB <- "N"
      temp_stn$FLAG <- 3
    } else {
      ## Scenario 4: If the total open area is < 5km2, and none of the station
      ## has untrawlable area, the entire station is assigned  as unknown. The 
      ## station is effectively untrawlable because it is too small to be 
      ## chosen in the allocation. However, there is no portion in the station 
      ## that is untrawlable to assign the whole station as untrawlable nor can
      ## the station be called trawlable because of the lack of a previously 
      ## good tow. 
      temp_stn$TRAWLAB <- "UNK"
      temp_stn$FLAG <- 4
    }
  }
  ## and then replace the merged station in new_goa_stations_2025
  updated_stations <- c(updated_stations, list(temp_stn))
  cat(paste0("Station ", istn, " converted to ", 
             temp_stn$TRAWLAB, ". Finished with ", 
             which(x = stns_mixed_trawl_info == istn), " of ",
             length(x = stns_mixed_trawl_info), " instances.\n"))
} ## loop over affected stations -- end


## Bind updated stations into one sf object
updated_stations <- do.call(dplyr::bind_rows, updated_stations)

## Update newly trawlability-reassigned stations
new_goa_stations_2025 <- dplyr::bind_rows(
  new_goa_stations_2025[!new_goa_stations_2025$STATION %in% 
                          updated_stations$STATION, ],
  updated_stations
)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Recalculate area of the new stations
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
new_goa_stations_2025$AREA_KM2 <- sf::st_area(x = new_goa_stations_2025)
units(x = new_goa_stations_2025$AREA_KM2) <- "km2"

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Save to geopackage
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# sf::st_write(obj = new_goa_stations_2025,
#              dsn = "analysis/goa_strata_2025/goa_stations_2025.gpkg",
#              append = FALSE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Plot each changed stations to pdf
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for (iscenario in 1:4) { ## Loop over the 8 scenarios -- start
  
  ## Subset the stations by the scenario flag
  scenario_subset <- subset(x = updated_stations,
                            subset = FLAG == iscenario)
  
  ## Open a pdf for the scenario 
  pdf(file = paste0("analysis/goa_strata_2025/trawl_scenario_", 
                    iscenario, ".pdf"), width = 8, height = 11, 
      onefile = T, family = "serif")
  
  ## Set figure parameters
  par(mar = c(0,0,1,0), mfrow = c(10, 6), oma = c(4, 4, 4, 4))
  
  for (stn_idx in 1:nrow(x = scenario_subset)) { ## Loop over stations -- start
    
    ## Station with mixed trawlability information
    temp_stn <- subset(x = goa_stations_2025_trawl,
                       STATION == scenario_subset$STATION[stn_idx])
    
    ## Station with updated trawlability information 
    updated_stn <- subset(x = new_goa_stations_2025,
                          STATION == scenario_subset$STATION[stn_idx])
    
    ## Any towpaths contained within the station
    temp_towpaths <- sf::st_intersection(x = towpaths, y = updated_stn)
    
    ## Plot the original station with mixed trawlability information
    plot(st_geometry(temp_stn), axes = F, cex.main = 0.75, lwd = 0.5,
         col = c("Y" = "green", "UNK" = "grey", "N" = "red")[temp_stn$TRAWLAB],
         main = paste("Station", scenario_subset$STATION[stn_idx]))
    
    ## Plot towpaths
    plot(st_geometry(obj = temp_towpaths), add = T, lwd = 2, 
         col = c("TRUE" = "black", 
                 "FALSE" = "purple")[paste(temp_towpaths$PERFORMANCE)])
    
    ## Legend for trawlability information
    legend("bottomleft", bty = "n", cex = 0.6,
           legend = paste0(temp_stn$TRAWLAB, ": ", 
                           round(temp_stn$AREA_KM2, 1), " km2"), 
           fill = c("Y" = "green", 
                    "UNK" = "grey", 
                    "N" = "red")[temp_stn$TRAWLAB]
    )
    ## Legend for towpaths
    legend("topleft", lty = 1, lwd = 1.5, bty = "n", cex = 0.6, 
           legend = c("good", "bad"), col = c("black", "purple"))
    
    ## figure box
    box(which = "figure")
    
    ## Plot the station with updated trawlability information
    plot(st_geometry(updated_stn), cex.main = 0.75, lwd = 0.5,
         col = c("Y" = "green", 
                 "UNK" = "grey", 
                 "N" = "red")[updated_stn$TRAWLAB],
         main = paste("Updated Station", scenario_subset$STATION[stn_idx]))
    
    ## Plot towpaths
    plot(st_geometry(obj = temp_towpaths), add = T, lwd = 2, 
         col = c("TRUE" = "black", 
                 "FALSE" = "purple")[paste(temp_towpaths$PERFORMANCE)])
    
    ## Legend for trawlability information    
    legend("bottomleft", bty = "n", cex = 0.6, 
           legend = paste0(updated_stn$TRAWLAB, ": ", 
                           round(updated_stn$AREA_KM2, 1), " km2"),
           fill = c("Y" = "green", 
                    "UNK" = "grey", 
                    "N" = "red")[updated_stn$TRAWLAB])
    ## Legend for towpaths
    legend("topleft", lty = 1, lwd = 1.5, bty = "n", cex = 0.6, 
           legend = c("good", "bad"), col = c("black", "purple"))
    
    ## figure box
    box(which = "figure")
  } ## Loop over stations -- end
  
  ## Close pdf
  dev.off()
  
  ## Print message
  cat("Finished with", paste0("analysis/goa_strata_2025/trawl_scenario_", 
                              iscenario, ".pdf\n"))
} ## Loop over the 8 scenarios -- end
