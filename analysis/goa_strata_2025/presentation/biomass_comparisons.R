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
goa_strata_cropped <- sf::st_intersection(x = goa_strata, 
                                          y = new_survey_footprint)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot(st_geometry(inpfc_areas))
# plot(st_geometry(goa_land), add = TRUE, col = "tan", border = F)

# plot(st_geometry(subset(inpfc_areas, INPFC_STRATUM == "Shumagin")), col = "red", border = F)
# plot(st_geometry(new_survey_footprint), add = T, col = "white", lwd = 3, border = "white")
# plot(st_geometry(subset(inpfc_areas, INPFC_STRATUM == "Shumagin")), add = T)
# plot(st_geometry(subset(goa_strata, STRATUM %in% 10:11)), add = T, border = "blue", lwd = 2)
# plot(st_geometry(goa_land), add = TRUE, col = "tan", border = F)
# 
# plot(st_geometry(subset(inpfc_areas, INPFC_STRATUM == "Southeastern")), col = "red", border = F)
# plot(st_geometry(new_survey_footprint), add = T, col = "white", lwd = 3, border = "white")
# plot(st_geometry(subset(inpfc_areas, INPFC_STRATUM == "Southeastern")), add = T)
# plot(st_geometry(subset(goa_strata, STRATUM %in% c(50) )), add = T, border = "blue")
# 
# plot(st_geometry(subset(inpfc_areas, INPFC_STRATUM == "Southeastern")), col = "red", border = F)
# plot(st_geometry(new_survey_footprint), add = T, col = "white", lwd = 3, border = "white")
# plot(st_geometry(subset(inpfc_areas, INPFC_STRATUM == "Southeastern")), add = T)
# plot(st_geometry(subset(goa_strata, STRATUM %in% c(151) )), add = T, border = "blue")
# 
# plot(st_geometry(subset(inpfc_areas, INPFC_STRATUM == "Southeastern")), col = "red", border = F)
# plot(st_geometry(new_survey_footprint), add = T, col = "white", lwd = 3, border = "white")
# plot(st_geometry(subset(inpfc_areas, INPFC_STRATUM == "Southeastern")), add = T)
# plot(st_geometry(subset(goa_strata, STRATUM %in% c(251) )), add = T, border = "blue")
# 
# plot(st_geometry(subset(inpfc_areas, INPFC_STRATUM == "Southeastern")), col = "red", border = F)
# plot(st_geometry(new_survey_footprint), add = T, col = "white", lwd = 3, border = "white")
# plot(st_geometry(subset(inpfc_areas, INPFC_STRATUM == "Southeastern")), add = T)
# plot(st_geometry(subset(goa_strata, STRATUM %in% c(350) )), add = T, border = "blue")
# 
# plot(st_geometry(subset(goa_strata, STRATUM %in% c(13) )), border = "blue")
# st_area(subset(goa_strata, STRATUM %in% c(13))) * 1e-6
# st_area(st_intersection(x = subset(goa_strata, STRATUM %in% c(13) ), 
#                                  y = new_survey_footprint)) * 1e-6
# 
# plot(st_geometry(subset(goa_strata_cropped, STRATUM %in% c(13) )), border = "red", add = TRUE)

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
temp <- subset(x = temp,
               subset = STRATUM %in% c(10, 11, 50, 151, 251, 350 ))
temp$DIFF <- temp$AREA_KM2_ORIG - temp$AREA_KM2_CROP
temp$PERC_DIFF <- 100 * round(temp$DIFF / temp$AREA_KM2_ORIG, 4)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Pull GOA survey data
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Scenario A: all stations, full area, baseline shown to Plan Team Nov 2025
##   Scenario B: all stations, truncate areas
##   Scenario C: truncate stations, full area
##   Scenario D: truncate stations and areas, shown to Plan Team Nov 2025
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data_A <- data_B <- data_C <- data_D <- gapindex_data

## Modify truncated areas in NMFS area 610 and 650
data_B$strata$AREA_KM2[match(temp$STRATUM, data_B$strata$STRATUM)] <-
  data_D$strata$AREA_KM2[match(temp$STRATUM, data_D$strata$STRATUM)] <- 
  temp$AREA_KM2_CROP

goa_stns <- 
  terra::vect(x = subset(x = gapindex_data$haul,
                         select = c(HAULJOIN, START_LATITUDE, START_LONGITUDE)),
              geom = c("START_LONGITUDE", "START_LATITUDE"), 
              crs = "EPSG:4326")
goa_stns <- sf::st_as_sf(terra::project(goa_stns, "EPSG:3338")) 

truncated_stns <- 
  subset(x = data_A$haul,
         subset = !HAULJOIN %in% st_filter(x = goa_stns, 
                                           y = nmfs_areas)$HAULJOIN
         & STRATUM %in% c(10, 11, 50, 151, 251, 350))

data_D$haul <- data_C$haul <-
  subset(x = data_D$haul,
         subset = !HAULJOIN %in% truncated_stns$HAULJOIN)

## Fill in zeros and calculate 
cpue_A <- gapindex::calc_cpue(gapdata = data_A)
cpue_B <- gapindex::calc_cpue(gapdata = data_B)
cpue_C <- gapindex::calc_cpue(gapdata = data_C)
cpue_D <- gapindex::calc_cpue(gapdata = data_D)

## Calculate stratum-level biomass, population abundance, mean CPUE and 
## associated variances
biomass_stratum_A <- gapindex::calc_biomass_stratum(
  gapdata = data_A,
  cpue = cpue_A)
biomass_stratum_B <- gapindex::calc_biomass_stratum(
  gapdata = data_B,
  cpue = cpue_B)
biomass_stratum_C <- gapindex::calc_biomass_stratum(
  gapdata = data_C,
  cpue = cpue_C)
biomass_stratum_D <- gapindex::calc_biomass_stratum(
  gapdata = data_D,
  cpue = cpue_D)

## Calculate aggregated biomass and population abundance across subareas,
## management areas, and regions
biomass_subarea_A <- 
  subset(x = gapindex::calc_biomass_subarea(
    gapdata = data_A,
    biomass_stratum = biomass_stratum_A),
    subset = AREA_ID %in% c(804:805, 99903), 
    select = c(YEAR, SPECIES_CODE, AREA_ID, BIOMASS_MT, BIOMASS_VAR))
biomass_subarea_B <- 
  subset(x = gapindex::calc_biomass_subarea(
    gapdata = data_B,
    biomass_stratum = biomass_stratum_B),
    subset = AREA_ID %in% c(804:805, 99903), 
    select = c(YEAR, SPECIES_CODE, AREA_ID, BIOMASS_MT, BIOMASS_VAR))
biomass_subarea_C <- 
  subset(x = gapindex::calc_biomass_subarea(
    gapdata = data_C,
    biomass_stratum = biomass_stratum_C),
    subset = AREA_ID %in% c(804:805, 99903), 
    select = c(YEAR, SPECIES_CODE, AREA_ID, BIOMASS_MT, BIOMASS_VAR))
biomass_subarea_D <- 
  subset(x = gapindex::calc_biomass_subarea(
    gapdata = data_D,
    biomass_stratum = biomass_stratum_D),
    subset = AREA_ID %in% c(804:805, 99903), 
    select = c(YEAR, SPECIES_CODE, AREA_ID, BIOMASS_MT, BIOMASS_VAR))

biomass_subarea_A$BIOMASS_CV <- 
  sqrt(x = biomass_subarea_A$BIOMASS_VAR) / ifelse(test = biomass_subarea_A$BIOMASS_MT == 0, NA, biomass_subarea_A$BIOMASS_MT)
biomass_subarea_B$BIOMASS_CV <- 
  sqrt(x = biomass_subarea_B$BIOMASS_VAR) / ifelse(test = biomass_subarea_B$BIOMASS_MT == 0, NA, biomass_subarea_B$BIOMASS_MT)
biomass_subarea_C$BIOMASS_CV <- 
  sqrt(x = biomass_subarea_C$BIOMASS_VAR) / ifelse(test = biomass_subarea_C$BIOMASS_MT == 0, NA, biomass_subarea_C$BIOMASS_MT)
biomass_subarea_D$BIOMASS_CV <- 
  sqrt(x = biomass_subarea_D$BIOMASS_VAR) / ifelse(test = biomass_subarea_D$BIOMASS_MT == 0, NA, biomass_subarea_D$BIOMASS_MT)

## Rank list of species by change in total biomass from the truncation.
merged_biomass <- 
  merge(x = merge(x = biomass_subarea_A,
                  y = biomass_subarea_B,
                  by = c("YEAR", "SPECIES_CODE", "AREA_ID"), 
                  suffixes = c("_A", "_B")), 
        
        y = merge(x =  biomass_subarea_C,
                  y = biomass_subarea_D,
                  by = c("YEAR", "SPECIES_CODE", "AREA_ID"), 
                  suffixes = c("_C", "_D")),
        by = c("YEAR", "SPECIES_CODE", "AREA_ID"))

merged_biomass[, paste0("DIFF_", LETTERS[2:4])] <-
  sweep(x = merged_biomass[, paste0("BIOMASS_MT_", LETTERS[2:4]), with = F],
        MARGIN = 1, 
        STATS = merged_biomass$BIOMASS_MT_A, 
        FUN = "-")

merged_biomass[, paste0("PERC_DIFF_", LETTERS[2:4])] <-
  sweep(x = merged_biomass[, paste0("DIFF_", LETTERS[2:4]), with = F], 
        MARGIN = 1, 
        STATS = ifelse(test = merged_biomass$BIOMASS_MT_A == 0,
                       yes = 1, 
                       no =  merged_biomass$BIOMASS_MT_A  ), 
        FUN = "/")

merged_biomass[, paste0("PERC_DIFF_", LETTERS[2:4])] <-
  round(merged_biomass[, paste0("PERC_DIFF_", LETTERS[2:4]), with = F] * 100, 2)

biomass_rank <- 
  aggregate(BIOMASS_MT_A ~ SPECIES_CODE, 
            data = merged_biomass, FUN = mean, subset = AREA_ID == 99903)
biomass_rank <- 
  biomass_rank[order(biomass_rank$BIOMASS_MT_A, decreasing = T), ]

mean_diff <- 
  aggregate(cbind(PERC_DIFF_B, PERC_DIFF_C, PERC_DIFF_D) ~ SPECIES_CODE + AREA_ID, 
            data = merged_biomass, FUN = function(x) abs(mean(x)))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

truncated_cpue <- subset(cpue_A,
                         subset = HAULJOIN %in% truncated_stns$HAULJOIN)

for (ispp in biomass_rank$SPECIES_CODE) {
  png(filename = paste0("analysis/goa_strata_2025/presentation/biomass_diff_",
                        ispp, ".png"),
      width = 8, height = 8, units = "in", res = 300)
  
  layout(mat = matrix(data = c(7, 8, 9,
                               1, 3, 5,
                               2, 4, 6),
                      byrow = TRUE,
                      nrow = 3), 
         widths = 1, heights = 1)
  par(mar = c(2, 0.5, 2, 0.5), oma = c(4, 5, 0, 1))
  common_name <- gapindex_data$species$COMMON_NAME[
    which(gapindex_data$species$SPECIES_CODE == ispp)
  ]
  
  ylimits <- 
    max(5, 
        max(abs(x = merged_biomass[merged_biomass$SPECIES_CODE == ispp,
                                   c("PERC_DIFF_B", "PERC_DIFF_C",
                                     "PERC_DIFF_D")])) * 1.1)
  
  cv_ylimits <- max(abs(x = merged_biomass[merged_biomass$SPECIES_CODE == ispp,
                                           paste0("BIOMASS_CV_", LETTERS[1:4]),
                                           with = F]),
                    na.rm = TRUE) * 1.1
  
  for (iarea in c(805, 804, 99903)) {
    temp_df <- subset(merged_biomass, SPECIES_CODE == ispp & AREA_ID == iarea)
    
    
    
    with(temp_df,
         matplot(x = YEAR, 
                 y = cbind(PERC_DIFF_B, PERC_DIFF_C, PERC_DIFF_D),
                 pch = 16, ylim = c(-ylimits, ylimits), ann = F, axes = F)
    )
    
    with(temp_df,
         matlines(x = YEAR, 
                  y = cbind(PERC_DIFF_B, PERC_DIFF_C, PERC_DIFF_D),
                  lty = 1)
    )
    abline(h = 0, lty = "dashed")
    abline(h = colMeans(subset(temp_df, 
                               select = c(PERC_DIFF_B, 
                                          PERC_DIFF_C, 
                                          PERC_DIFF_D))),
           col = palette(), lwd = 2)
    box()
    axis(side = 1)
    
    if (iarea == 805) {
      axis(side = 2, las = 1)
      mtext(side = 2, text = "% Change in Biomass Estimate", line = 3.5)
    }
    
    # axis(side = 1, labels = c("805" = F, "804" = F, "99903" = T)[paste0(iarea)])
    legend("topleft", legend = c("805" = "Western GOA", 
                                 "804" = "Eastern GOA", 
                                 "99903" = "Gulf-wide")[paste0(iarea)], 
           bty = "n")
    
    if (all(is.na(temp_df[, paste0("BIOMASS_CV_", LETTERS[1:4]), with = F])) ) {
      plot(1, type = "n", axes = F, ann = F)
    } else {
      with(temp_df,
           matplot(x = YEAR, 
                   y = cbind(BIOMASS_CV_A, BIOMASS_CV_B, 
                             BIOMASS_CV_C, BIOMASS_CV_D),
                   pch = 16, ann = F, axes = F, col = c("gray", palette()),
                   ylim = c(0, cv_ylimits))
      )
      with(temp_df,
           matlines(x = YEAR, 
                    y = cbind(BIOMASS_CV_A, BIOMASS_CV_B, 
                              BIOMASS_CV_C, BIOMASS_CV_D),
                    lty = 1, col = c("gray", palette()))
      )
      box()
      axis(side = 1)
      
      if (iarea == 805) {
        axis(side = 2, las = 1)
        mtext(side = 2, text = "Biomass CV Estimate", line = 3.5)
      }
      
      legend("topleft", legend = c("805" = "Western GOA", 
                                   "804" = "Eastern GOA", 
                                   "99903" = "Gulf-wide")[paste0(iarea)], 
             bty = "n")
    }
    
  }
  
  ylimits <- 
    1.1 * abs(x = max(truncated_cpue$CPUE_KGKM2[truncated_cpue$SPECIES_CODE == ispp]/1000, 
                      na.rm = TRUE))
  
  plot(CPUE_KGKM2/1000 ~ YEAR,
       data = truncated_cpue,
       ylim = c(0, ylimits),
       subset = SPECIES_CODE == ispp
       & HAULJOIN %in% subset(truncated_stns, STRATUM %in% 10:11)$HAULJOIN,
       axes = F, ann = F)
  axis(side = 1); axis(side = 2, las = 1);
  box()
  mtext(side = 2, text = "CPUE (thousand kg/km2)", line = 3.5)
  legend("topleft", "Removed WGOA Stations", bty = "n")
  
  plot(CPUE_KGKM2/1000 ~ YEAR,
       data = truncated_cpue,
       ylim = c(0, ylimits),
       subset = SPECIES_CODE == ispp
       & HAULJOIN %in% subset(truncated_stns, !STRATUM %in% 10:11)$HAULJOIN,
       axes = F, ann = F)
  axis(side = 1)
  box()
  mtext(side = 1, text = "Year", outer = TRUE, line = 2)
  legend("topleft", "Removed EGOA Stations", bty = "n")
  
  plot(1, type = "n", ann = F, axes = F)
  legend("center", legend = c("all stations, truncated areas", 
                              "truncated stations, full area",
                              "truncated stations and areas"),
         col = palette(), pch = 16, lty = 1, bty = "n")
  dev.off()
}

saveRDS(object = gapindex_data, 
        file = "analysis/goa_strata_2025/presentation/gapindex_data.RDS")
saveRDS(object = truncated_cpue, 
        file = "analysis/goa_strata_2025/presentation/truncated_cpue.RDS")
