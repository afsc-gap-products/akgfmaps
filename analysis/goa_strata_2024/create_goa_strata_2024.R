##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Creation of DESIGN_YEAR 2024 GOA strata
## Author:        Zack Oyafuso (zack.oyafuso@noaa.gov)
## Description:   Create historical (DESIGN_YEAR = 1984) GOA Strata truncated
##                using NMFS Areas 610, 620, 630, 640, 650
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Restart R Session before running
rm(list = ls())

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Import the 1984 and 2025 design_year versions of the goa base layers
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(akgfmaps)
library(gapindex)

goa_1984 <- akgfmaps::get_base_layers(select.region = "goa", 
                                      design.year = 1984, 
                                      set.crs = 3338)

goa_2025 <- akgfmaps::get_base_layers(select.region = "goa", 
                                      set.crs = 3338)

nmfs_areas <- akgfmaps::get_nmfs_areas(set.crs = 3338) |>
  subset(subset = REP_AREA %in% c(610, 620, 630, 640, 650))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Intersect the 1984 DESIGN_YEAR GOA strata with the 2025 DESIGN_YEAR
##   GOA survey area
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
goa_2024 <- sf::st_intersection(x = goa_1984$survey.strata, 
                                y = dplyr::select(nmfs_areas, geometry)) 
goa_2024 <- aggregate(x = goa_2024[, c("STRATUM", "AREA_M2")], 
                      by = list(goa_2024$STRATUM), 
                      FUN = sum)
goa_2024$STRATUM <- goa_2024$Group.1

## Recalculate Stratum Area
goa_2024$AREA_M2 <- as.numeric(x = sf::st_area(goa_2024))
goa_2024$AREA_KM2 <- as.numeric(x = sf::st_area(goa_2024)) / 1e6
goa_2024$SURVEY_DEFINITION_ID <- 47
goa_2024$DESIGN_YEAR <- 2024

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot(st_geometry(goa_1984$survey.strata), border = F, col = "black")
plot(st_geometry(goa_2024), col = "red", border = F, add = TRUE)

sum(goa_1984$survey.strata$AREA_M2) / 1e6 # 320006.5 km2
sum(goa_2024$AREA_M2) / 1e6 #315508.6 km2
sum(goa_2025$survey.strata$AREA_M2) / 1e6 # 312791.7

(sum(goa_2024$AREA_M2) - sum(goa_1984$survey.strata$AREA_M2)) /
  sum(goa_1984$survey.strata$AREA_M2) * 100 ## 1.4% reduction in survey area

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Write to geopackage
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sf::st_write(obj = goa_2024[, c("SURVEY_DEFINITION_ID", "DESIGN_YEAR", 
                                "STRATUM", "AREA_M2", "geometry")], 
             dsn = "analysis/goa_strata_2024/goa_strata_2024.gpkg", 
             delete_layer = TRUE)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Append new area records to Oracle 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
area_1984 <- 
  RODBC::sqlQuery(channel = channel, 
                  query = "SELECT * FROM GAP_PRODUCTS.AREA
                           WHERE SURVEY_DEFINITION_ID = 47
                           AND DESIGN_YEAR = 1984")

goa_strata_2024 <- 
  terra::vect(x = "analysis/goa_strata_2024/goa_strata_2024.gpkg")
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

