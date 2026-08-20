##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project: Update Aleutian Islands stations with updated bathymetry
## Author:  Zack Oyafuso (zack.oyafuso@noaa.gov)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Import libraries
library(terra)
library(akgfmaps) ## v.4.1.2

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import AI stations with trawlability information as of the 2024 BTS
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ai_base_layers <- akgfmaps::get_base_layers(select.region = "AI",
                                            set.crs = "EPSG:3338") 
ai_stations_current <- 
  sf::st_read(dsn = "Y:/RACE_GF/AI-GOA/shapefiles/aigrid_stratum_corrected.shp")
ai_stations_current$TRAWLABLE <- 
  ifelse(test = ai_stations_current$Field2 == "[NULL]",
         yes = "UNK", 
         no = ai_stations_current$Field2)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import new stratum polygons and 5-km grid
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ai_grid_5km <- sf::st_read(dsn = "analysis/goa_strata_2025/goaai_grid_2025.shp")
ai_strata_new <- sf::st_read(dsn = "analysis/ai_strata_2028/ai_strata_2028.gpkg")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import tow data
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (!file.exists("output/ai/shapefiles/ai_towpath.shp")) {
  
  library(navmaps)
  library(gapindex)
  ## Connect to Oracle. Make sure you are connected to the NOAA internal 
  ## network or VPN.  
  navmaps::get_gps_data(channel = channel, region = "ai")
  navmaps::make_towpaths(region = "ai")
  
  ## Move shapefile folder into the analysis/goa_strata_2025 folder
  file.copy(from = "output/ai/shapefiles/", 
            to = "analysis/ai_strata_2028/",
            recursive = TRUE)
  
  towpaths <- sf::st_read(dsn = "output/ai/shapefiles/ai_towpath.shp")
  towpath_mid <- sf::st_read(dsn = "output/ai/shapefiles/ai_midpoint.shp")
  
} else {
  towpaths <- sf::st_read(dsn = "output/ai/shapefiles/ai_towpath.shp")
  towpath_mid <- sf::st_read(dsn = "output/ai/shapefiles/ai_midpoint.shp")
}
## Remove Green Hope (VESSEL 83) and 80s data
towpaths <- subset(x = towpaths,
                   subset = CRUISE >= 199100 & VESSEL != 83)
towpaths_mid <- sf::st_centroid(x = towpaths)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import tow data
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ai_stations_v1 <- sf::st_intersection(x = ai_grid_5km,
                                      y = ai_strata_new)
ai_stations_v1$STATION <- paste0(ai_stations_v1$GRIDID, "-", 
                                 ai_stations_v1$STRATUM)

## Intersect the station polygons with the trawl_polygons to calculate any new 
## stations with mixed trawlability information. 
ai_stations_v1_trawl <- 
  sf::st_intersection(x = ai_stations_v1, 
                      y = ai_stations_current[, c("TRAWLABLE", "ID")])

ai_stations_v2_trawl <-
  ai_stations_v1_trawl %>%
  dplyr::group_by(GRIDID, STRATUM, STATION, TRAWLABLE) %>% 
  dplyr::summarize()

## Query 2025 stations that inherited > 1 trawlability statuses from the 
## legacy GOA survey stations 
stns_mixed_trawl_info <- 
  names(x = which(x = table(ai_stations_v2_trawl$STATION) > 1))

## `updated_stations` will contain updated trawlability status of the stations
## within the stns_mixed_trawl_info 
updated_stations <- list()

# sf::st_write(obj = ai_stations_v2_trawl, 
# dsn = "Y:/RACE_GF/Oyafuso/AI New Strata/ai_stations_v2_trawl.gpkg")

for (istn in stns_mixed_trawl_info) { ## loop over affected stations -- start
  
  ## Subset stations within istn
  temp_stn <- subset(x = ai_stations_v2_trawl,
                     subset = STATION == istn)
  
  plot(st_geometry(obj = temp_stn),
       axes = F,
       col = c("Y" = "green", "UNK" = "grey", "N" = "red")[temp_stn$TRAWLABLE])
  points(sf::st_geometry(towpaths_mid),  lwd = 2, xpd = F)
  plot(sf::st_geometry(towpaths), add = TRUE, lwd = 2, xpd = NA)
  
  ## Scenario 1: station is a mixture of T area (with good tows paths)
  ## and either UKN or UT area. Since there is a good tow in the station,
  ## the whole station is turned to T if it contains the midpoint of the 
  ## towline. 
  
  ## Query whether there are any good tows in the mixed station
  good_tow_in_station <- 
    # sum(sf::st_intersects(x = towpaths_mid[towpaths_mid$PERFORM >= 0, ],
    #                       y = temp_stn, 
    #                       sparse = F)) > 0
    "Y" %in%
    temp_stn$TRAWLABLE[
      rowSums(x = sf::st_intersects(y = towpaths_mid[towpaths_mid$PERFORM >= 0, ],
                                    x = temp_stn, 
                                    sparse = F)) > 0
    ]
  
  
  ## If so, convert the non-T area in the station as T
  if (good_tow_in_station & any(temp_stn$TRAWLABLE == "Y")) {
    
    temp_stn %>%
      dplyr::group_by(GRIDID, STRATUM, STATION) %>% 
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
      dplyr::group_by(GRIDID, STRATUM, STATION) %>% 
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
ai_stations_v3_trawl <- dplyr::bind_rows(
  ai_stations_v2_trawl[!(ai_stations_v2_trawl$STATION %in% 
                           updated_stations$STATION), ],
  updated_stations
)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Any leftover stations that intersect with the midpoints of good tows
##   are turned trawlable. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
T_areas <- sf::st_intersects(
  x = ai_stations_v3_trawl[ai_stations_v3_trawl$TRAWLABLE == "Y", ],
  y = towpaths_mid[towpaths_mid$PERFORM >= 0, ],
  sparse = F
)

rownames(x = T_areas) <- 
  ai_stations_v3_trawl$STATION[ai_stations_v3_trawl$TRAWLABLE == "Y"]

ai_stations_v3_trawl$TRAWLABLE[ 
  ai_stations_v3_trawl$STATION %in% 
    rownames(x = T_areas)[rowSums(x = T_areas) == 0] 
] <- "UNK"

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Recalculate area and centorid lat/lon of the new stations.
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ai_stations_v3_trawl$AREA_KM2 <- sf::st_area(x = ai_stations_v3_trawl)
units(x = ai_stations_v3_trawl$AREA_KM2) <- "km2"

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Save to geopackage
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sf::st_write(obj = sf::st_cast(x = subset(ai_stations_v3_trawl, 
                                          select = -FLAG), 
                               to = "MULTIPOLYGON"),
             dsn = "Y:/RACE_GF/Oyafuso/AI New Strata/ai_stations_2028.gpkg",
             append = FALSE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Plot each changed stations to pdf
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for (iscenario in 1:4) { ## Loop over the 4 scenarios -- start
  
  ## Subset the stations by the scenario flag
  scenario_subset <- subset(x = updated_stations,
                            subset = FLAG == iscenario)
  
  ## Open a pdf for the scenario 
  pdf(file = paste0("analysis/ai_strata_2028/trawl_scenario_", 
                    iscenario, ".pdf"), width = 8, height = 11, 
      onefile = T, family = "serif")
  
  ## Set figure parameters
  par(mar = c(0,0,1,0), mfrow = c(10, 6), oma = c(4, 4, 4, 4))
  
  for (stn_idx in 1:nrow(x = scenario_subset)) { ## Loop over stations -- start
    
    ## Station with mixed trawlability information
    temp_stn <- subset(x = ai_stations_v2_trawl,
                       STATION == scenario_subset$STATION[stn_idx])
    
    ## Station with updated trawlability information 
    updated_stn <- subset(x = ai_stations_v3_trawl,
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
                           round(sf::st_area(temp_stn) / 1e6, 1), " km2"), 
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
                           round(sf::st_area(updated_stn) / 1e6, 1), " km2"),
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
