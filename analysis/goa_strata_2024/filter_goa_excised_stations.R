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

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Append new area records to Oracle 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
area_1984 <- 
  RODBC::sqlQuery(channel = channel, 
                  query = "SELECT * FROM GAP_PRODUCTS.AREA
                           WHERE SURVEY_DEFINITION_ID = 47
                           AND DESIGN_YEAR = 1984")
goa_strata_2024$AREA_KM2 <- goa_strata_2024$AREA_M2 * 1e-6
area_2024 <- merge(x = subset(x = area_1984, select = -AREA_KM2), 
                   by.x = "AREA_ID", all.x = TRUE, 
                   y = subset(x = as.data.frame(x = goa_strata_2024), 
                              select = c("STRATUM", "AREA_KM2")),
                   by.y = "STRATUM")
area_2024$DESIGN_YEAR <- 2024
area_2024 <- area_2024[, names(x = area_1984)]

col_metadata <- 
  RODBC::sqlQuery(channel = channel, 
                  query = paste('
                SELECT METADATA_COLNAME as "colname", 
                METADATA_DATATYPE as "datatype"
                FROM GAP_PRODUCTS.METADATA_COLUMN
                         WHERE METADATA_COLNAME IN',
                                gapindex::stitch_entries(names(x = area_2024)))
  )

RODBC::sqlSave(
  channel = channel,
  dat = as.data.frame(x = area_2024),
  tablename = "GAP_PRODUCTS.AREA",
  append = TRUE,
  rownames = FALSE,
  varTypes = stats::setNames(object = col_metadata$datatype,
                             nm = col_metadata$colname)[names(area_2024)]
)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Append new stratum_groups records to Oracle
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stratum_groups_2024 <- 
  RODBC::sqlQuery(channel = channel, 
                  query = "SELECT * FROM GAP_PRODUCTS.STRATUM_GROUPS
                         WHERE SURVEY_DEFINITION_ID = 47
                         AND DESIGN_YEAR = 1984")
stratum_groups_2024$DESIGN_YEAR <- 2024

col_metadata <- 
  RODBC::sqlQuery(
    channel = channel, 
    query = paste('
                SELECT METADATA_COLNAME as "colname", 
                METADATA_DATATYPE as "datatype"
                FROM GAP_PRODUCTS.METADATA_COLUMN
                WHERE METADATA_COLNAME IN',
                  gapindex::stitch_entries(names(x = stratum_groups_2024)))
  )

RODBC::sqlSave(
  channel = channel,
  dat = stratum_groups_2024,
  tablename = "GAP_PRODUCTS.STRATUM_GROUPS",
  append = TRUE,
  rownames = FALSE,
  varTypes = stats::setNames(object = col_metadata$datatype,
                             nm = col_metadata$colname)[names(x = stratum_groups_2024)]
)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   GAP_PRODUCTS.SURVEY_DESIGN was modified by hand in SQL Developer
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

