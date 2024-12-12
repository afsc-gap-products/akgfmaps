##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       NMFS area vs INPFC area biomass analysis
## Author:        Zack Oyafuso (zack.oyafuso@noaa.gov)
## Description:   Evaluate changes in biomass from truncating the historical 
##                GOA strata by the NMFS areas: 610, 620, 630, 640, and 650.
##                Spatial scales of concern are: Western GOA (610) and Eastern 
##                GOA (640, 650) and species of focus are all single taxa 
##                SPECIES_CODE values in GOA.ANALYSIS_SPECIES.
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Restart R Session before running
rm(list = ls())

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import Libraries, Connect to Oracle
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# devtools::install_github("afsc-gap-products/akgfmaps@with_GOA2025_strata", 
# force = TRUE)
library(akgfmaps)

if (!dir.exists(paths = "analysis/goa_strata_2025/presentation/"))
  dir.create(path = "analysis/goa_strata_2025/presentation/")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import spatial objects from akgfmaps
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
goa_base_layers <- akgfmaps::get_base_layers(select.region = "goa", 
                                             set.crs = "EPSG:3338")
inpfc_areas <- goa_base_layers$inpfc.strata
goa_strata <- goa_base_layers$survey.strata
goa_land <- goa_base_layers$akland

nmfs_areas <-  subset(x = akgfmaps::get_nmfs_areas(set.crs = "EPSG:3338"),
                      REP_AREA %in% c(610, 620, 630, 640, 650))
new_survey_footprint <- sf::st_union(nmfs_areas)

## Crop goa_strata to new goa
goa_strata_cropped <- st_intersection(x = goa_strata, 
                                      y = new_survey_footprint)

plot(st_geometry(inpfc_areas))
plot(st_geometry(goa_land), add = TRUE, col = "tan", border = F)

plot(st_geometry(subset(inpfc_areas, INPFC_STRATUM == "Shumagin")), col = "red", border = F)
plot(st_geometry(new_survey_footprint), add = T, col = "white", lwd = 3, border = "white")
plot(st_geometry(subset(inpfc_areas, INPFC_STRATUM == "Shumagin")), add = T)
plot(st_geometry(subset(goa_strata, STRATUM %in% 10:11)), add = T, border = "blue", lwd = 2)
plot(st_geometry(goa_land), add = TRUE, col = "tan", border = F)

plot(st_geometry(subset(inpfc_areas, INPFC_STRATUM == "Southeastern")), col = "red", border = F)
plot(st_geometry(new_survey_footprint), add = T, col = "white", lwd = 3, border = "white")
plot(st_geometry(subset(inpfc_areas, INPFC_STRATUM == "Southeastern")), add = T)
plot(st_geometry(subset(goa_strata, STRATUM %in% c(50) )), add = T, border = "blue")

plot(st_geometry(subset(inpfc_areas, INPFC_STRATUM == "Southeastern")), col = "red", border = F)
plot(st_geometry(new_survey_footprint), add = T, col = "white", lwd = 3, border = "white")
plot(st_geometry(subset(inpfc_areas, INPFC_STRATUM == "Southeastern")), add = T)
plot(st_geometry(subset(goa_strata, STRATUM %in% c(151) )), add = T, border = "blue")

plot(st_geometry(subset(inpfc_areas, INPFC_STRATUM == "Southeastern")), col = "red", border = F)
plot(st_geometry(new_survey_footprint), add = T, col = "white", lwd = 3, border = "white")
plot(st_geometry(subset(inpfc_areas, INPFC_STRATUM == "Southeastern")), add = T)
plot(st_geometry(subset(goa_strata, STRATUM %in% c(251) )), add = T, border = "blue")

plot(st_geometry(subset(inpfc_areas, INPFC_STRATUM == "Southeastern")), col = "red", border = F)
plot(st_geometry(new_survey_footprint), add = T, col = "white", lwd = 3, border = "white")
plot(st_geometry(subset(inpfc_areas, INPFC_STRATUM == "Southeastern")), add = T)
plot(st_geometry(subset(goa_strata, STRATUM %in% c(350) )), add = T, border = "blue")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Calculate Area of GOA strata when the survey strata are truncated by
##   NMFS Statistical Areas 610, 620, 630, 640, and 650. 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
goa_strata$AREA_KM2 <- as.numeric(round(sf::st_area(goa_strata) / 1E6, 1))
goa_strata_cropped$AREA_KM2 <- as.numeric(round(sf::st_area(goa_strata_cropped) / 1E6, 1))

temp <- 
  merge(x = aggregate(AREA_KM2 ~ STRATUM, data = goa_strata, FUN = sum),
        y = aggregate(AREA_KM2 ~ STRATUM, data = goa_strata_cropped, FUN = sum),
        by = "STRATUM", suffixes = c("_ORIG", "_CROP"))
temp$DIFF <- temp$AREA_KM2_ORIG - temp$AREA_KM2_CROP
temp$PERC_DIFF <- 100 * round(temp$DIFF / temp$AREA_KM2_ORIG, 4)
subset(temp, PERC_DIFF > 2)


library(gapindex)

## Connect to Oracle
channel <- gapindex::get_connected(check_access = F)
analysis_species <- 
  RODBC::sqlQuery(channel = channel,
                  query = "SELECT SPECIES_CODE
                  FROM GOA.ANALYSIS_SPECIES
                  WHERE BIOMASS_FLAG IN ('BOTH', 'GOA')
                  AND SUMMARY_GROUP IS NULL")$SPECIES_CODE

## Pull data.
gapindex_data <- gapindex::get_data(
  year_set = c(1990:2000, 2002:2023),
  survey_set = "GOA",
  spp_codes = analysis_species,   
  haul_type = 3,
  abundance_haul = "Y",
  pull_lengths = F, 
  taxonomic_source = "GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION",
  channel = channel)

gapindex_goa_trunc <- gapindex_data
gapindex_goa_trunc$strata$AREA_KM2 <- 
  temp$AREA_KM2_CROP[match(x = gapindex_goa_trunc$strata$STRATUM, 
                           table = temp$STRATUM)]

goa_stns <- 
  terra::vect(x = subset(x = gapindex_goa_trunc$haul, 
                         select = c(HAULJOIN, START_LATITUDE, START_LONGITUDE)),
              geom = c("START_LONGITUDE", "START_LATITUDE"), 
              crs = "EPSG:4326")
goa_stns <- sf::st_as_sf(terra::project(goa_stns, "EPSG:3338")) 
gapindex_goa_trunc$haul <-
  subset(x = gapindex_goa_trunc$haul,
         subset = HAULJOIN %in% st_filter(x = goa_stns, 
                                          y = nmfs_areas)$HAULJOIN)

## Fill in zeros and calculate CPUE
cpue <- gapindex::calc_cpue(gapdata = gapindex_data)
cpue_trunc <- gapindex::calc_cpue(gapdata = gapindex_goa_trunc)

## Calculate stratum-level biomass, population abundance, mean CPUE and 
## associated variances
biomass_stratum <- gapindex::calc_biomass_stratum(
  gapdata = gapindex_data,
  cpue = cpue)

biomass_stratum_trunc <- gapindex::calc_biomass_stratum(
  gapdata = gapindex_goa_trunc,
  cpue = cpue_trunc)

## Calculate aggregated biomass and population abundance across subareas,
## management areas, and regions
biomass_subareas <- 
  subset(x = gapindex::calc_biomass_subarea(
    gapdata = gapindex_data,
    biomass_stratum = biomass_stratum),
    subset = AREA_ID %in% c(804:805, 99903), 
    select = c(YEAR, SPECIES_CODE, AREA_ID, BIOMASS_MT))

biomass_subareas_trunc <- 
  subset(x = gapindex::calc_biomass_subarea(
    gapdata = gapindex_goa_trunc,
    biomass_stratum = biomass_stratum_trunc),
    subset = AREA_ID %in% c(804:805, 99903),
    select = c(YEAR, SPECIES_CODE, AREA_ID, BIOMASS_MT))

## Rank list of species by change in total biomass from the truncation.
total_biomass <- merge(x = biomass_subareas,
                       y = biomass_subareas_trunc,
                       by = c("SPECIES_CODE", "AREA_ID", "YEAR"),
                       suffixes = c("_FULL", "_TRUNC"))
total_biomass <- 
  merge(x = total_biomass,
        y = gapindex_data$species[, c("SPECIES_CODE", 
                                      "SPECIES_NAME", 
                                      "COMMON_NAME")],
        by = "SPECIES_CODE")
total_biomass$PERC_DIFF <- 
  with(total_biomass, 
       100 * round(x = (BIOMASS_MT_FULL - BIOMASS_MT_TRUNC) / BIOMASS_MT_FULL,
                   digits = 2))
total_biomass$PERC_DIFF[is.nan(x = total_biomass$PERC_DIFF)] <- 0

total_biomass <- 
  readRDS(file = "analysis/goa_strata_2025/presentation/total_biomass.RDS")

mean_diff <- aggregate(PERC_DIFF ~ AREA_ID + SPECIES_CODE + COMMON_NAME,
                       data = total_biomass,
                       subset = AREA_ID == 99903,
                       FUN = function(x) (mean(abs(x))))

biomass_rank <- aggregate(BIOMASS_MT_FULL ~ AREA_ID + SPECIES_CODE + COMMON_NAME,
                          data = total_biomass,
                          subset = AREA_ID == 99903,
                          FUN = mean )

mean_diff <- mean_diff[order(abs(x = biomass_rank$BIOMASS_MT_FULL), 
                             decreasing = TRUE), ]

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

png(filename = paste0("analysis/goa_strata_2025/presentation/",
                      "biomass_diff_barplot_region.png"), 
    width = 6, height = 8, units = "in", res = 500)
par(mar = c(5, 10, 1, 1), mfrow = c(1, 1), oma = c(0, 0, 0, 0))
with(mean_diff[20:1, ],
     barplot(PERC_DIFF, horiz = TRUE, las = 1, names.arg = COMMON_NAME, 
             xlim = c(0, 10), col = "black",
             xlab = "Average % Change in Biomass")
)
abline(v = seq(5, 50, 5), lty = "dashed", col = "grey")
box()
dev.off()

pdf(file = paste0("analysis/goa_strata_2025/presentation/",
                      "biomass_diff_ts_region.pdf"), 
    width = 6, height = 8, onefile = T)
par(mfrow = c(5, 1), mar = c(1, 0, 0, 0), oma = c(3, 5, 1, 1))
for (iname in c("arrowtooth flounder", "Pacific ocean perch", "walleye pollock", 
                "Pacific halibut", "Pacific cod", 
                mean_diff$COMMON_NAME[order(mean_diff$PERC_DIFF, 
                                            decreasing = TRUE)[1:5]] )) {
  
  plot(BIOMASS_MT_FULL/1000 ~ YEAR, type = "l", las = 1, ann = F, axes = F,
       main = paste0(iname),
       ylim = c(0, 
                max(total_biomass[AREA_ID == 99903 & 
                                    COMMON_NAME == iname
                ]$BIOMASS_MT_FULL/1000 * 1.25)),
       data = total_biomass[COMMON_NAME == iname & 
                              AREA_ID == 99903])
  points(BIOMASS_MT_FULL/1000 ~ YEAR, pch = 16,
         data = total_biomass[AREA_ID == 99903 & 
                                COMMON_NAME == iname])
  lines(BIOMASS_MT_TRUNC/1000 ~ YEAR, type = "l", col = "red", 
        data = total_biomass[AREA_ID == 99903 & 
                               COMMON_NAME == iname])
  points(BIOMASS_MT_TRUNC/1000 ~ YEAR, pch = 16, col = "red", 
         data = total_biomass[AREA_ID == 99903 & 
                                COMMON_NAME == iname])
  box()
  axis(side = 2, las = 1)
  legend("topright", legend = iname, bty = "n", text.font = 2)
  legend("topleft", legend = c("Full", "Truncated"), 
         col = c("black", "red"), pch = 16, lty = 1, bty= 'n')
  
  if (iname %in% c("scissortail sculpin", "Pacific cod")){
    axis(side = 1)
    mtext(side = 2, text = "Biomass (thousand mts)", outer = T, font = 2, line = 3)
  }
}
dev.off()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# png(filename = "analysis/goa_strata_2025/presentation/biomass_diff_barplot.png", 
#     width = 6, height = 8, units = "in", res = 500)
# par(mar = c(4, 12, 0, 1))
# with(mean_diff,
#      barplot(PERC_DIFF, horiz = TRUE, las = 1, names.arg = COMMON_NAME, 
#              xlim = c(0, 50), 
#              col = c("804" = "white", "805" = "darkgreen")[paste(AREA_ID)], 
#              xlab = "Average % Change in Biomass")
# )
# abline(v = seq(10, 50, 10), lty = "dashed", col = "grey")
# legend("right", legend = c("Eastern GOA", "Western GOA"), 
#        fill = c("white", "darkgreen"))
# dev.off()
# 
# png(filename = "analysis/goa_strata_2025/presentation/biomass_diff_timeseries.png", 
#     width = 6, height = 8, units = "in", res = 500)
# par(mfrow = c(5, 1), mar = c(1, 0, 0, 0), oma = c(3, 5, 1, 1))
# for (iname in c("spotted ratfish", "southern rock sole", "walleye pollock",
#                 "Pacific cod", "flathead sole")) {
#   
#   plot(BIOMASS_MT_FULL/1000 ~ YEAR, type = "l", las = 1, ann = F, axes = F,
#        main = paste0(iname),
#        ylim = c(0, 
#                 max(total_biomass[AREA_ID == 804 & 
#                                     COMMON_NAME == iname
#                 ]$BIOMASS_MT_FULL/1000 * 1.25)),
#        data = total_biomass[COMMON_NAME == iname & 
#                               AREA_ID == 804])
#   points(BIOMASS_MT_FULL/1000 ~ YEAR, pch = 16,
#          data = total_biomass[AREA_ID == 804 & 
#                                 COMMON_NAME == iname])
#   lines(BIOMASS_MT_TRUNC/1000 ~ YEAR, type = "l", col = "red", 
#         data = total_biomass[AREA_ID == 804 & 
#                                COMMON_NAME == iname])
#   points(BIOMASS_MT_TRUNC/1000 ~ YEAR, pch = 16, col = "red", 
#          data = total_biomass[AREA_ID == 804 & 
#                                 COMMON_NAME == iname])
#   box()
#   axis(side = 2, las = 1)
#   legend("topright", legend = iname, bty = "n", text.font = 2)
#   legend("topleft", legend = c("Full", "Truncated"), 
#          col = c("black", "red"), pch = 16, lty = 1, bty= 'n')
# }
# axis(side = 1)
# mtext(side = 2, text = "Biomass (thousand mts)", outer = T, font = 2, line = 3)
# 
# dev.off()
# 
# mean_diff <- subset(mean_diff, subset = abs(x = PERC_DIFF) > 5)
# 
# {
#   pdf(file = "analysis/goa_strata_2025/presentation/biomass_comparison.pdf",
#       onefile = TRUE, width = 8, height = 10)
#   
#   par(mfrow = c(3, 3))
#   for (irow in nrow(x = mean_diff):1) {
#     ispp <- mean_diff$SPECIES_CODE[irow]
#     iname <- mean_diff$COMMON_NAME[irow]
#     iarea <- mean_diff$AREA_ID[irow]
#     iarea_name <- c("804" = "Eastern GOA", 
#                     "805" = "Western GOA")[paste(iarea)]
#     
#     plot(BIOMASS_MT_FULL/1000 ~ YEAR, type = "l", las = 1, 
#          ylab = "Biomass (thousand mts)",
#          main = paste0(iname, "\n", iarea_name),
#          ylim = c(0, 
#                   max(total_biomass[AREA_ID == iarea & 
#                                       SPECIES_CODE == ispp
#                   ]$BIOMASS_MT_FULL/1000 * 1.1)),
#          data = total_biomass[SPECIES_CODE == ispp & 
#                                 AREA_ID == iarea])
#     points(BIOMASS_MT_FULL/1000 ~ YEAR, pch = 16,
#            data = total_biomass[SPECIES_CODE == ispp & 
#                                   AREA_ID == iarea])
#     lines(BIOMASS_MT_TRUNC/1000 ~ YEAR, type = "l", col = "red", 
#           data = total_biomass[SPECIES_CODE == ispp & 
#                                  AREA_ID == iarea])
#     points(BIOMASS_MT_TRUNC/1000 ~ YEAR, pch = 16, col = "red", 
#            data = total_biomass[SPECIES_CODE == ispp & 
#                                   AREA_ID == iarea])
#     
#     if (irow%%3 == 0)
#       legend("topleft", legend = c("Full", "Truncated"), 
#              col = c("black", "red"), pch = 16, lty = 1, bty= 'n')
#   }
#   
#   dev.off()
# }
