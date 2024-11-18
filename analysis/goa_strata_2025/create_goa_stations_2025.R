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
goa_stations_2023$TRAWLABLE <- goa_stations_2023$TRAWLAB

## Group the legacy GOA stations by trawlability status
trawl_polygons <-
  goa_stations_2023 %>% dplyr::group_by(TRAWLABLE) %>% dplyr::summarize()

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
goa_stations_2025 <- sf::st_intersection(
  x = goa_grid_2025[, "GRIDID"],
  y = goa_strata_2025[, c("NMFS_AREA", "REP_AREA", "STRATUM")]
)

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

if (!file.exists("output/goa/shapefiles/goa_towpath.shp")) {
  library(navmaps)
  library(gapindex)
  
  ## Connect to Oracle. Make sure you are connected to the NOAA internal 
  ## network or VPN.  
  channel <- gapindex::get_connected(check_access = FALSE)
  navmaps::get_gps_data(channel = channel, region = "goa")
  navmaps::make_towpaths(region = "goa")
  
  ## Move shapefile folder into the analysis/goa_strata_2025 folder
  file.copy(from = "output/goa/shapefiles/", 
            to = "analysis/goa_strata_2025/",
            recursive = TRUE)
  
} else {
  towpaths <- sf::st_read(dsn = "output/goa/shapefiles/goa_towpath.shp")
  towpath_mid <- sf::st_read(dsn = "output/goa/shapefiles/goa_midpoint.shp")
}
## Remove Green Hope (VESSEL 83) and 80s data
towpaths <- subset(x = towpaths,
                   subset = CRUISE >= 199000 & VESSEL != 83)
towpaths_mid <- sf::st_centroid(x = towpaths)

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
outside_legacy_footprint$TRAWLABLE <- "UNK"
# Calculate the area of each station/station bit
outside_legacy_footprint$AREA_KM2 <- sf::st_area(x = outside_legacy_footprint)
units(x = outside_legacy_footprint$AREA_KM2) <- "km2"

## Append the area outside the legacy footprint to `goa_stations_2025_trawl`
goa_stations_2025_trawl <- dplyr::bind_rows(x = goa_stations_2025_trawl,
                                            y = outside_legacy_footprint)
goa_stations_2025_trawl$TRAWLABLE[is.na(x = goa_stations_2025_trawl$TRAWLABLE)] <- "UNK"

## Merge any station bits outside the legacy GOA footprint with their respective
## station ID within the legacy GOA footprint. This step creates a new 2025
## GOA survey footprint.
goa_stations_2025_trawl <-
  goa_stations_2025_trawl %>%
  dplyr::group_by(GRIDID, NMFS_AREA, REP_AREA, STRATUM, STATION, TRAWLABLE) %>% 
  dplyr::summarize()

## Recalculate total area of each station
goa_stations_2025_trawl$AREA_KM2 <- sf::st_area(x = goa_stations_2025_trawl)
units(x = goa_stations_2025_trawl$AREA_KM2) <- "km2"

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
  #      col = c("Y" = "green", "UNK" = "grey", "N" = "red")[temp_stn$TRAWLABLE])
  # points(towpaths_mid,  lwd = 2, xpd = F)
  # plot(sf::st_geometry(towpaths), add = TRUE, lwd = 2, xpd = NA)
  
  ## Scenario 1: station is a mixture of T area (with good tows paths)
  ## and either UKN or UT area. Since there is a good tow in the station,
  ## the whole station is turned to T if it contains the midpoint of the 
  ## towline. 
  
  ## Query whether there are any good tows in the mixed station
  good_tow_in_station <- 
    sum(sf::st_intersects(x = towpaths_mid[towpaths_mid$PERFORM >= 0, ],
                          y = temp_stn, 
                          sparse = F)) > 0
  
  ## If so, convert the non-T area in the station as T
  if (good_tow_in_station & any(temp_stn$TRAWLABLE == "Y")) {
    
    temp_stn %>%
      dplyr::group_by(GRIDID,  NMFS_AREA, REP_AREA, STRATUM, STATION) %>% 
      dplyr::summarize() -> temp_stn
    
    temp_stn$TRAWLABLE <- "Y"
    temp_stn$FLAG <- 1
    temp_stn$AREA_KM2 <- sf::st_area(x = temp_stn)
    units(x = temp_stn$AREA_KM2) <- "km2"
  } else {
    
    ## Subset any stns features that are either trawlable (Y) or unknown (UNK) 
    open_area <- subset(x = temp_stn, subset = TRAWLABLE %in% c("UNK", "Y"))
    open_area$AREA_KM2 <- sf::st_area(x = open_area)
    units(x = open_area$AREA_KM2) <- "km2"
    
    ## Subset any untrawlable station features
    ut_area <- subset(x = temp_stn, subset = TRAWLABLE %in% c("N"))
    ut_area$AREA_KM2 <- sf::st_area(x = ut_area)
    units(x = ut_area$AREA_KM2) <- "km2"
    
    ## Merge all station bits back together
    temp_stn %>%
      dplyr::group_by(GRIDID, NMFS_AREA, REP_AREA, STRATUM, STATION) %>% 
      dplyr::summarize() -> temp_stn
    
    ## Reassign trawlable status and flag 
    
    ## Scenario 2: If the total open area is >= 5km2, there's ample space to 
    ## search for a tow, reassign station as unknown trawlability
    if (as.numeric(x = sum(open_area$AREA_KM2)) >= 5) {
      temp_stn$TRAWLABLE <- "UNK"
      temp_stn$FLAG <- 2
    } else if (nrow(x = ut_area) > 0) {
      ## Scenario 3: If the total open area is < 5km2, and there is any portion 
      ## of the station that is untrawlable, turn the station to untrawlable 
      ## because the open area is < 5km2 and too small to search for a tow,
      ## effectively turning the station untrawlable.
      temp_stn$TRAWLABLE <- "N"
      temp_stn$FLAG <- 3
    } else {
      ## Scenario 4: If the total open area is < 5km2, and none of the station
      ## has untrawlable area, the entire station is assigned  as unknown. The 
      ## station is effectively untrawlable because it is too small to be 
      ## chosen in the allocation. However, there is no portion in the station 
      ## that is untrawlable to assign the whole station as untrawlable nor can
      ## the station be called trawlable because of the lack of a previously 
      ## good tow. 
      temp_stn$TRAWLABLE <- "UNK"
      temp_stn$FLAG <- 4
    }
  }
  ## and then replace the merged station in new_goa_stations_2025
  updated_stations <- c(updated_stations, list(temp_stn))
  cat(paste0("Station ", istn, " converted to ", 
             temp_stn$TRAWLABLE, ". Finished with ", 
             which(x = stns_mixed_trawl_info == istn), " of ",
             length(x = stns_mixed_trawl_info), " instances.\n"))
} ## loop over affected stations -- end

## Bind updated stations into one sf object
updated_stations <- do.call(dplyr::bind_rows, updated_stations)

## Update newly trawlability-reassigned stations
new_goa_stations_2025 <- dplyr::bind_rows(
  new_goa_stations_2025[!(new_goa_stations_2025$STATION %in% 
                            updated_stations$STATION), ],
  updated_stations
)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
T_areas <- sf::st_intersects(
  x = new_goa_stations_2025[new_goa_stations_2025$TRAWLABLE == "Y", ],
  y = towpaths_mid[towpaths_mid$PERFORM >= 0, ],
  sparse = F
)

rownames(x = T_areas) <- 
  new_goa_stations_2025$STATION[new_goa_stations_2025$TRAWLABLE == "Y"]

new_goa_stations_2025$TRAWLABLE[ 
  new_goa_stations_2025$STATION %in% 
    rownames(x = T_areas)[rowSums(x = T_areas) == 0] 
] <- "UNK"

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Recalculate area and centorid lat/lon of the new stations.
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
new_goa_stations_2025$AREA_KM2 <- sf::st_area(x = new_goa_stations_2025)
units(x = new_goa_stations_2025$AREA_KM2) <- "km2"

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Save to geopackage
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sf::st_write(obj = sf::st_cast(x = subset(new_goa_stations_2025, 
                                          select = -FLAG), 
                               to = "MULTIPOLYGON"),
             dsn = "analysis/goa_strata_2025/goa_stations_2025.gpkg",
             append = FALSE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Plot each changed stations to pdf
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for (iscenario in 1:4) { ## Loop over the 4 scenarios -- start
  
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
    temp_towpaths_mids <- sf::st_intersection(x = towpaths_mid, y = updated_stn)
    
    ## Plot the original station with mixed trawlability information
    plot(st_geometry(temp_stn), axes = F, cex.main = 0.75, lwd = 0.5,
         col = c("Y" = "green", "UNK" = "grey", "N" = "red")[temp_stn$TRAWLABLE],
         main = paste("Station", scenario_subset$STATION[stn_idx]))
    
    ## Plot towpaths
    lines(temp_towpaths, lwd = 2, 
          col = c("TRUE" = "black", 
                  "FALSE" = "purple")[paste(temp_towpaths$PERFORM >= 0)])
    points(temp_towpaths_mids,  pch = 16, 
           col = c("TRUE" = "black", 
                   "FALSE" = "purple")[paste(temp_towpaths_mids$PERFORM >= 0)])
    
    ## Legend for trawlability information
    legend("bottomleft", bty = "n", cex = 0.6,
           legend = paste0(temp_stn$TRAWLABLE, ": ", 
                           round(temp_stn$AREA_KM2, 1), " km2"), 
           fill = c("Y" = "green", 
                    "UNK" = "grey", 
                    "N" = "red")[temp_stn$TRAWLABLE]
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
                 "N" = "red")[updated_stn$TRAWLABLE],
         main = paste("Updated Station", scenario_subset$STATION[stn_idx]))
    
    ## Plot towpaths
    lines(temp_towpaths, lwd = 2, 
          col = c("TRUE" = "black", 
                  "FALSE" = "purple")[paste(temp_towpaths$PERFORM >= 0)])
    points(temp_towpaths_mids,  pch = 16, 
           col = c("TRUE" = "black", 
                   "FALSE" = "purple")[paste(temp_towpaths_mids$PERFORM >= 0)])
    
    ## Legend for trawlability information    
    legend("bottomleft", bty = "n", cex = 0.6, 
           legend = paste0(updated_stn$TRAWLABLE, ": ", 
                           round(updated_stn$AREA_KM2, 1), " km2"),
           fill = c("Y" = "green", 
                    "UNK" = "grey", 
                    "N" = "red")[updated_stn$TRAWLABLE])
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
} ## Loop over the 4 scenarios -- end
