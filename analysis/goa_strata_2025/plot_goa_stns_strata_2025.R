##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Plot 2025 GOA Strata
## Author:        Zack Oyafuso (zack.oyafuso@noaa.gov)
## Description:
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Restart R Session before running
rm(list = ls())

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import Libraries
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(akgfmaps)
library(terra)
library(RColorBrewer)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import base layers
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## `goa_base` are basic shape layers from the akgfmaps package
goa_base <- akgfmaps::get_base_layers(select.region = "goa", 
                                      set.crs = "EPSG:3338")
goa_domain <- terra::aggregate(terra::vect(x = goa_base$survey.strata))

goa_stations_2025 <- 
  terra::vect(x = "analysis/goa_strata_2025/goa_stations_2025.gpkg")

## `ak_land` is the extracted land/coastline shapefile from `goa_base` 
ak_land <- terra::vect(x = goa_base$akland[goa_base$akland$POPYADMIN %in% 
                                             c("BRITISH COLUMBIA",
                                               "YUKON TERRITORY",
                                               "ALASKA"), ])

strata_list <- terra::vect(x = "analysis/goa_strata_2025/goa_strata_2025.gpkg")
goa_grid <- terra::vect(x = "analysis/goa_strata_2025/goaai_grid_2025.shp")

## GOA depth strata 
depth_mods <-
  read.csv(file = "analysis/goa_strata_2025/depth_modifications_2025.csv")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Plot ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf(file = "analysis/goa_strata_2025/updated_strata.pdf",
    width = 8, height = 6, onefile = TRUE)
for (iarea in unique(x = depth_mods$NMFS_AREA)) { ## Loop over area -- start
  
  ## temporary objects
  n_strata <- with(depth_mods, table(NMFS_AREA))[iarea]
  temp_strata <- depth_mods$STRATUM[depth_mods$NMFS_AREA == iarea]
  
  stratum_cols <- c(RColorBrewer::brewer.pal(name = "Set1",
                                             n = n_strata - 1), "cyan"
                    # , adjustcolor(col = "darkgrey", alpha.f = 0.6)
  )
  
  ## Plot blank 
  plot(strata_list[strata_list$NMFS_AREA == iarea], border = F, axes = F, 
       col = "white", lwd = 0.1)
  
  ## add land
  plot(ak_land, add = TRUE, col = "tan", border = T, lwd = 0.1)
  
  ## add strata
  plot(strata_list[strata_list$NMFS_AREA == iarea], border = F, axes = F, 
       col = stratum_cols, 
       lwd = 0.1, add = TRUE)
  
  ## Add Untrawlable Areas
  # plot(terra::crop(x = goa_stations_2025[goa_stations_2025$TRAWLABLE == "N"],
  #                  y = strata_list[strata_list$NMFS_AREA == iarea]),
  #      col =   adjustcolor( "darkgrey", alpha.f = 0.8), border = F,
  #      add = TRUE)
  
  ## Add outlines of stations
  plot(terra::crop(x = goa_stations_2025,
                   y = strata_list[strata_list$NMFS_AREA == iarea]),
       border = T, lwd = 0.05, add = TRUE )
  
  ## Legend
  legend_labels <- with(subset(depth_mods, NMFS_AREA == iarea),
                        paste0("Stratum ", STRATUM, ": ", 
                               DEPTH_MIN_M , " - ", DEPTH_MAX_M, " m"))
  # legend_labels <- c(legend_labels, "Untrawlable")
  
  legend(c("Shumagin" = "topleft", "Chirikof" = "topleft",
           "Kodiak" = "bottomright", "West Yakutat" = "bottom",
           "Southeast Outside" = "bottomleft")[iarea],
         legend = legend_labels,
         title = "Stratum Legend",
         fill = stratum_cols, 
         xpd = NA)
  
  mtext(side = 3, line = -2, text = iarea, font = 2, cex = 1.5)
}  ## Loop over area -- end
dev.off()

## Open pdf file
if (file.exists("analysis/goa_strata_2025/updated_strata.pdf")) {
  system('open  analysis/goa_strata_2025/updated_strata.pdf')
}

