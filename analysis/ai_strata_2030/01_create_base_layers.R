##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Create spatial polygon that captures each AI subarea
##  Output base spatial data from akgfmaps to a form that can be imported 
##  into ArcPro
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())

## Import libraries
library(sf)
library(terra)
library(akgfmaps)
library(gapindex)
library(navmaps)

## Connect to Oracle
chl <- gapindex::get_connected(check_access = FALSE)

## Setup directories to put the outputted spatial objects
shared_dir <- "G:/My Drive/Aleutian Island BTS Redesign/shapefiles/"
for (idir in c("coastline", "towpaths", "survey_footprint", "checks",
               "historical_objects", "intermediate_objects", "final_objects")) {
  if (!dir.exists(paths = paste0(shared_dir, idir, "/"))) 
    dir.create(path = paste0(shared_dir, idir))
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Download base spatial layers from akgfmaps and write some to the shared
## google drive directory as gpkg files
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## 1991 Design Year Aleutian Islands Base Layers from akgfmaps
ai_base_layers <- 
  akgfmaps::get_base_layers(select.region = "ai", 
                            design.year = 1991, 
                            set.crs = "EPSG:3338", 
                            high.resolution.coast = TRUE)
current_ai_strata <- ai_base_layers$survey.strata |> terra::vect()
current_ai_stations <- ai_base_layers$survey.grid |> terra::vect()

## Import Stratum table from GAP_PRODUCTS.AREA
current_ai_trawl_status <- gapindex::sql_query(channel = chl,
                                               query = "SELECT 
                      AIGRID_ID AS GRID_ID, 
                      TRAWLABLE, 
                      STATIONID AS STATION, 
                      STRATUM FROM AI.AIGRID_GIS 
                    WHERE STRATUM != 0")

## Merge trawlability status information with the station data
current_ai_stations <- merge(x = current_ai_stations,
                             y = current_ai_trawl_status,
                             by = c("GRID_ID", "STATION", "STRATUM"))

current_ai_strata_tbl <- 
  gapindex::sql_query(channel = chl,
                      query = "
SELECT * 
FROM GAP_PRODUCTS.AREA
WHERE
  SURVEY_DEFINITION_ID = 52 
  AND DESIGN_YEAR = 1991 
  AND AREA_TYPE = 'STRATUM'
                    ")

## Eastern cutoff of the survey footprint for our purposes is 164 W. Extract
## what that longitude is projected onto EPSG:3338
eastern_lon_cutoff <-  sf::st_point(x = c(-164, 54.609)) |>
  sf::st_sfc(crs = 4326) |>
  sf::st_transform(crs = 3338) |>
  st_coordinates() |>
  subset(select = "X") |>
  as.numeric()

## 2025 Design Year GOA survey footprint west of 164 W
goa_footprint_2025 <-
  akgfmaps::get_base_layers(select.region = "goa", 
                            design.year = 2025,
                            set.crs = "EPSG:3338")$survey.area

goa_footprint_2025 <-
  sf::st_crop(x = goa_footprint_2025,
              y = c(sf::st_bbox(goa_footprint_2025)["xmin"],
                    sf::st_bbox(goa_footprint_2025)["ymin"],
                    xmax = eastern_lon_cutoff,
                    sf::st_bbox(goa_footprint_2025)["ymax"])) |>
  terra::vect()

## 1984 Design Year GOA survey footprint west of 164 W
goa_footprint_1984 <- 
  akgfmaps::get_base_layers(select.region = "goa", 
                            design.year = 1984,
                            set.crs = "EPSG:3338")$inpfc.strata |>
  subset(subset = AREA_NAME == "Shumagin")

goa_footprint_1984 <-
  sf::st_crop(x = goa_footprint_1984,
              y = c(sf::st_bbox(obj = goa_footprint_1984)["xmin"],
                    sf::st_bbox(obj = goa_footprint_1984)["ymin"],
                    xmax = eastern_lon_cutoff,
                    sf::st_bbox(obj = goa_footprint_1984)["ymax"])) |>
  terra::vect()

## 2022 Design Year Bering Sea Shelf survey footprint
bs_footprint <- 
  akgfmaps::get_base_layers(select.region = "bs.all", 
                            design.year = 2022,
                            set.crs = "EPSG:3338")$survey.area |> 
  terra::vect() 

## 2023 Design Year Bering Sea Slope survey footprint
bss_footprint <- 
  akgfmaps::get_base_layers(select.region = "ebs.slope", 
                            design.year = 2023,
                            set.crs = "EPSG:3338")$survey.area |> 
  terra::vect() 

## Alaska Coastline West of 164 W 
akland <- 
  ai_base_layers$akland |> subset(subset = COUNTRY == "US") |> 
  sf::st_crop(y = c(sf::st_bbox(obj = ai_base_layers$survey.area)["xmin"], 
                    sf::st_bbox(obj = goa_footprint_2025)["ymin"], 
                    xmax = as.numeric(eastern_lon_cutoff), 
                    sf::st_bbox(obj = ai_base_layers$survey.area)["ymax"]
  )) |>
  terra::vect() |> terra::aggregate()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Download all the towpath data from the navamps package.
##  Extracting the towpath data takes a lot of time, so it's best to just do 
##  this once. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!file.exists(paste0(shared_dir, "towpaths/ai_towpath.shp"))){
  ## Connect to Oracle. Make sure you are connected to the NOAA internal 
  ## network or VPN.
  channel <- gapindex::get_connected(check_access = FALSE)
  navmaps::get_gps_data(channel = channel, region = "ai")
  navmaps::make_towpaths(region = "ai")
  
  ## Move shapefile folder into the analysis/goa_strata_2025 folder
  file.copy(from = "output/ai/shapefiles/", 
            to = shared_dir,
            recursive = TRUE)
} else(paste("towpaths are already saved to", shared_dir))


## Save objects
for (iobj in c("bss_footprint", "bs_footprint", "goa_footprint_1984", 
               "goa_footprint_2025")) {
  get(x = iobj) |>
    terra::writeVector(filename = paste0(shared_dir, "survey_footprint/",
                                         iobj, ".gpkg"), 
                       overwrite = TRUE)
}

for (iobj in c("current_ai_strata", "current_ai_stations")) {
  get(x = iobj) |>
    terra::writeVector(filename = paste0(shared_dir, "historical_objects/",
                                         iobj, ".gpkg"), 
                       overwrite = TRUE)
}

write.csv(x = current_ai_strata_tbl,
          file = paste0(shared_dir, 
                        "historical_objects/current_ai_strata_tbl.csv"),
          row.names = FALSE)

terra::writeVector(x = akland,
                   filename = paste0(shared_dir, "coastline/akland.gpkg"), 
                   overwrite = TRUE)
