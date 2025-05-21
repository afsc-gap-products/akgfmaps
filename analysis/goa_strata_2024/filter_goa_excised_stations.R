##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Excise historical GOA stations outside of main NMFS areas
## Author:        Zack Oyafuso (zack.oyafuso@noaa.gov)
## Description:   These records will be appended to GAP_PRODUCTS tables
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Restart R Session before running
rm(list = ls())

library(terra)
library(gapindex)
channel <- gapindex::get_connected(check_access = FALSE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import GOA hauls and NMFS areas 659 (SE Inside) and 519 (Unimak)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nmfs_areas <- akgfmaps::get_nmfs_areas(set.crs = 3338) |>
  subset(subset = REP_AREA %in% c(659, 519)) |> terra::vect()

## Pull in all ABUNDANCE_HAUL 
goa_hauls <- RODBC::sqlQuery(channel = channel,
                             query = "SELECT CRUISE, HAULJOIN, STRATUM, 
                             END_LATITUDE, END_LONGITUDE 
                             FROM RACEBASE.HAUL
                             WHERE REGION = 'GOA' 
                             AND ABUNDANCE_HAUL = 'Y'
                             ORDER BY CRUISE
                             ") |>
  ## Create SpatVector in lat/lon
  terra::vect(geom = c("END_LONGITUDE", "END_LATITUDE"), 
              crs = "EPSG:4326") |>
  ## Project to aea
  terra::project(terra::crs("EPSG:3338"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Intersect NMFS areas 659 and 519 with the historical gulf stations
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
goa_excised <- goa_hauls[
  terra::is.related(x = goa_hauls, 
                    y = nmfs_areas, 
                    "intersects")
]

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Plot
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
goa_strata_2024 <- 
  terra::vect(x = "analysis/goa_strata_2024/goa_strata_2024.gpkg")

plot(goa_strata_2024, border = "grey")
plot(nmfs_areas, add = TRUE, border = "blue")
plot(goa_excised, add = TRUE, col = "red", pch = 16, cex = 0.5)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Save as csv and gpkg
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write.csv(x = as.data.frame(x = goa_excised), 
          file = "analysis/goa_strata_2024/goa_excised_stations.csv", 
          row.names = FALSE)

terra::writeVector(
  x = goa_excised, 
  filename = "analysis/goa_strata_2024/goa_excised_stations.gpkg"
)
