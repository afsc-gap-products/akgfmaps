##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Northern Bering Sea comparisons
## Author:        Zack Oyafuso (zack.oyafuso@noaa.gov)
##
## Description:   Comparison of 2025 NBS biomass indices for all species 
##                under three survey footprint scenarios: A) DESIGN_YEAR = 2022 
##                stratum areas and as we would normally calculate them and 
##                B) DESIGN_YEAR = 2022 area records but with stratum XX 
##                reduced to account for the survey not surveying that section,
##                C) Estimate total biomass based on the ratio between 
##                stratum biomass calculated with and without data from the 
##                seven unsampled stations.
##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Connect to Oracle (Make sure to connect to network or VPN)
library(gapindex)
channel <- gapindex::get_connected(check_access = F)

## Pull NBS data, make a copy for the "b" scenario.
gapindex_data_2025a <- gapindex_data_2025b <- gapindex_data_2025c <- gapindex::get_data(
  year_set = 2025,
  survey_set = "NBS",
  spp_codes = NULL,
  channel = channel)

gapindex_data_hist <- gapindex_data_drop <- gapindex::get_data(
  year_set = 2010:2023,
  survey_set = "NBS",
  spp_codes = NULL,
  channel = channel)

## Modify the stratum area for the "b" version
gapindex_data_2025b$strata$AREA_KM2[
  gapindex_data_2025b$strata$STRATUM == 70
] <- 69352.403

## Drop stations for version "c"

## Get hauljoins for stations that were not sampled in 2025

drop_hauljoins <- 
  gapindex_data_drop$haul$HAULJOIN[
    gapindex_data_drop$haul$STATION %in% c("T-03", "S-03", "R-03", "T-02", "S-02", "R-02", "R-01")
    ]

gapindex_data_drop$haul <- 
  gapindex_data_drop$haul[!(gapindex_data_drop$haul$HAULJOIN %in% drop_hauljoins), ]

gapindex_data_drop$catch <- 
  gapindex_data_drop$catch[!(gapindex_data_drop$catch$HAULJOIN %in% drop_hauljoins)]

# Calculate historical CPUE, stratum biomass, and subarea biomass with and without dropped stations

cpue_drop <- gapindex::calc_cpue(gapdata = gapindex_data_drop)
cpue_hist <- gapindex::calc_cpue(gapdata = gapindex_data_hist)

biomass_stratum_drop <- gapindex::calc_biomass_stratum(
  gapdata = gapindex_data_drop,
  cpue = cpue_drop
)

biomass_stratum_hist <- gapindex::calc_biomass_stratum(
  gapdata = gapindex_data_hist,
  cpue = cpue_hist
)

biomass_subarea_drop <- gapindex::calc_biomass_subarea(
  gapdata = gapindex_data_drop,
  biomass_stratum = biomass_stratum_drop
)

biomass_subarea_hist <- gapindex::calc_biomass_subarea(
  gapdata = gapindex_data_hist,
  biomass_stratum = biomass_stratum_hist
)

# Calculate ratio between biomass estimates

biomass_stratum_ratio <-  
  dplyr::inner_join(
    biomass_stratum_hist, 
    biomass_stratum_drop, 
    by = c("SURVEY_DEFINITION_ID", "SURVEY", "STRATUM", "SPECIES_CODE", "YEAR"),
    suffix = c("_hist", "_drop")
  )

biomass_stratum_ratio$FULL_TO_DROP <- biomass_stratum_ratio$BIOMASS_MT_hist / biomass_stratum_ratio$BIOMASS_MT_drop

biomass_stratum_ratio$FULL_TO_DROP[is.na(biomass_stratum_ratio$FULL_TO_DROP)] <- 1
biomass_stratum_ratio$FULL_TO_DROP[is.infinite(biomass_stratum_ratio$FULL_TO_DROP)] <- 1

biomass_ratio_by_species <- 
  biomass_stratum_ratio |>
  dplyr::group_by(SURVEY_DEFINITION_ID, SURVEY, STRATUM, SPECIES_CODE) |>
  dplyr::summarise(RATIO = mean(FULL_TO_DROP, na.rm = TRUE),
                   VAR_RATIO = var(FULL_TO_DROP))

# Biomass subarea 

biomass_stratum_ratio_70 <- biomass_stratum_ratio[
  biomass_stratum_ratio$STRATUM == 70, 
]

ggplot() +
  geom_point(data = 
               dplyr::filter(biomass_stratum_ratio_70, SPECIES_CODE %in% c(21740, 21720, 10210, 10285)),
             mapping = aes(x = YEAR, y = FULL_TO_DROP)) +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = expression('Stratum 70 biomass '*over('All', 'Drop'))) +
  facet_wrap(~SPECIES_CODE) +
  theme_bw()

# biomass_subarea_ratio <-
#   dplyr::inner_join(
#     biomass_subarea_hist,
#     biomass_subarea_drop,
#     by = c("SURVEY_DEFINITION_ID", "SURVEY", "AREA_ID", "SPECIES_CODE", "YEAR"),
#     suffix = c("_hist", "_drop")
#   )

## Fill in zeros and calculate CPUE under both stratum scenarios
cpue_a <- gapindex::calc_cpue(gapdata = gapindex_data_2025a)
cpue_b <- gapindex::calc_cpue(gapdata = gapindex_data_2025b)
cpue_c <- gapindex::calc_cpue(gapdata = gapindex_data_2025c)

## Calculate stratum-level biomass, population abundance, mean CPUE and 
## associated variances under both stratum scenarios
biomass_stratum_a <- gapindex::calc_biomass_stratum(
  gapdata = gapindex_data_2025a,
  cpue = cpue_a)

biomass_stratum_b <- gapindex::calc_biomass_stratum(
  gapdata = gapindex_data_2025b,
  cpue = cpue_b)

# Update biomass for stratum c
biomass_stratum_c <- gapindex::calc_biomass_stratum(
  gapdata = gapindex_data_2025c,
  cpue = cpue_c)

biomass_stratum_c <- 
  dplyr::left_join(
    biomass_stratum_c,
    biomass_ratio_by_species
  )

biomass_stratum_c$RATIO[is.na(biomass_stratum_c$RATIO)] <- 1

biomass_stratum_c$BIOMASS_MT <- biomass_stratum_c$BIOMASS_MT * biomass_stratum_c$RATIO



## Calculate aggregated biomass and population abundance across subareas,
## management areas, and regions under both stratum scenarios
biomass_subarea_a <- gapindex::calc_biomass_subarea(
  gapdata = gapindex_data_2025a,
  biomass_stratum = biomass_stratum_a)

biomass_subarea_b <- gapindex::calc_biomass_subarea(
  gapdata = gapindex_data_2025b,
  biomass_stratum = biomass_stratum_b)

biomass_subarea_c <- gapindex::calc_biomass_subarea(
  gapdata = gapindex_data_2025c,
  biomass_stratum = biomass_stratum_c)

## Rename fields and combine stratum and region estimates into one dataframe
names(x = biomass_stratum_a)[
  names(x = biomass_stratum_a) == "STRATUM"
] <- "AREA_ID"
names(x = biomass_stratum_b)[
  names(x = biomass_stratum_b) == "STRATUM"
] <- "AREA_ID"
names(x = biomass_stratum_c)[
  names(x = biomass_stratum_c) == "STRATUM"
] <- "AREA_ID"


biomass_a <-
  rbind(biomass_stratum_a[,
                          names(x = biomass_subarea_a), 
                          with = F],
        biomass_subarea_a)

## calculate CV of total biomass estimate
biomass_a$CV <- sqrt(x = biomass_a$BIOMASS_VAR) / 
  ifelse(test = biomass_a$BIOMASS_MT == 0, 
         yes = 1,
         no = biomass_a$BIOMASS_MT)

biomass_b <- 
  rbind(biomass_stratum_b[,
                          names(x = biomass_subarea_b), 
                          with = F],
        biomass_subarea_b)

## Calculate CV of total biomass estimate
biomass_b$CV <- sqrt(x = biomass_b$BIOMASS_VAR) / 
  ifelse(test = biomass_b$BIOMASS_MT == 0, 
         yes = 1,
         no = biomass_b$BIOMASS_MT)

biomass_c <-
  rbind(biomass_stratum_c[,
                          names(x = biomass_subarea_c), 
                          with = F],
        biomass_subarea_c)

## calculate CV of total biomass estimate
biomass_c$CV <- sqrt(x = biomass_c$BIOMASS_VAR) / 
  ifelse(test = biomass_c$BIOMASS_MT == 0, 
         yes = 1,
         no = biomass_c$BIOMASS_MT)


## Merge the three scenarios into one dataframe
merged_biomass <- 
  merge(x = subset(x = biomass_a, select = c("SPECIES_CODE", "AREA_ID", 
                                             "BIOMASS_MT", "CV")),
        y = subset(x = biomass_b, select = c("SPECIES_CODE", "AREA_ID", 
                                             "BIOMASS_MT", "CV")),
        by = c("SPECIES_CODE", "AREA_ID"), 
        suffixes = c("_a", "_b"))

merged_biomass <- 
  merge(x = merged_biomass,
        y = subset(x = biomass_c, select = c("SPECIES_CODE", "AREA_ID", 
                                             "BIOMASS_MT", "CV")),
        by = c("SPECIES_CODE", "AREA_ID"))

names(merged_biomass)[(ncol(merged_biomass)-1):ncol(merged_biomass)] <- 
  paste0(
    names(merged_biomass)[(ncol(merged_biomass)-1):ncol(merged_biomass)], "_c"
  )

## Calculate percent difference for total biomass and absolute difference for CV
merged_biomass$BIOMASS_PERC_DIFF <- 
  round(x = with(merged_biomass, 
                 (BIOMASS_MT_b - BIOMASS_MT_a) / 
                   ifelse(test = BIOMASS_MT_a == 0, 
                          yes = 1, 
                          no = BIOMASS_MT_a) * 100), 
        digits = 2) 
merged_biomass$CV_DIFF <-   round(with(merged_biomass, (CV_b - CV_a)), 6)

## Reorder columns
merged_biomass <- 
  merged_biomass[BIOMASS_MT_a > 0, 
                 c("SPECIES_CODE", "AREA_ID", 
                   "BIOMASS_MT_a", "BIOMASS_MT_b", "BIOMASS_MT_c", "BIOMASS_PERC_DIFF",
                   "CV_a", "CV_b", "CV_c", "CV_DIFF"), 
                 with = F] 

table(merged_biomass$SPECIES_CODE)

## Save
write.csv(merged_biomass,
          "code/analysis/nbs_2025/nbs_2025_biomass_comparison.csv",
          row.names = F)
