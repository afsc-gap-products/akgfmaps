##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Project:       Compare biomass estimates without stratum 70 stations
## Author:        Sean Rohan (sean.rohan@noaa.gov)
##
## Description:   Compare approaches to calculating design based index biomass
##                with data missing from NBS stratum 70 using data from 
##                2010-2023. Scenarios: A) Normal calculations using all data. 
##                B) DESIGN_YEAR = 2022 area records but with stratum 70 
##                reduced to account for the survey not surveying that section,
##                C) Estimate biomass for stratum 70 without data from the 
##                seven unsampled stations and adjust based on historical
##                relationship between stratum 70 biomass from scenarios 
##                A and D. D) Remove hauls from seven unsampled stations but 
##                still expand to all of stratum 70.
##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Connect to Oracle (Make sure to connect to network or VPN)
library(gapindex)
library(ggplot2)
library(ggthemes)
channel <- gapindex::get_connected(check_access = F)

## Pull NBS data, make a copy for the "b" option.
gapindex_data_a <- gapindex_data_b <- gapindex_data_hist <- 
  gapindex::get_data(
    year_set = 2010:2023,
    survey_set = "NBS",
    spp_codes = NULL,
    channel = channel)

## Get hauljoins for stations that were not sampled in 2025

drop_stations <- c("T-03", "S-03", "R-03", "T-02", "S-02", "R-02", "R-01")

drop_hauljoins <- 
  gapindex_data_a$haul$HAULJOIN[
    gapindex_data_a$haul$STATION %in% drop_stations
  ]

## Drop stations for options b,c,d
gapindex_data_b$haul <-
  gapindex_data_b$haul[!(gapindex_data_b$haul$HAULJOIN %in% drop_hauljoins), ]

gapindex_data_b$catch <-
  gapindex_data_b$catch[!(gapindex_data_b$catch$HAULJOIN %in% drop_hauljoins), ]

gapindex_data_c <- gapindex_data_d <- gapindex_data_drop <- gapindex_data_b

## Modify the stratum area for "b"
gapindex_data_b$strata$AREA_KM2[
  gapindex_data_b$strata$STRATUM == 70
] <- 69352.403

# option C: Calculate historical CPUE, stratum biomass, and subarea biomass with and without dropped stations

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

# Mean ratio by species averaged across years
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


## Fill in zeros and calculate CPUE under different options
cpue_a <- gapindex::calc_cpue(gapdata = gapindex_data_a)
cpue_b <- gapindex::calc_cpue(gapdata = gapindex_data_b)
cpue_c <- gapindex::calc_cpue(gapdata = gapindex_data_c)
cpue_d <- gapindex::calc_cpue(gapdata = gapindex_data_d)

## Calculate stratum-level biomass, population abundance, mean CPUE and 
## associated variances under both stratum options
biomass_stratum_a <- gapindex::calc_biomass_stratum(
  gapdata = gapindex_data_a,
  cpue = cpue_a)

biomass_stratum_b <- gapindex::calc_biomass_stratum(
  gapdata = gapindex_data_b,
  cpue = cpue_b)

# Update biomass for stratum c
biomass_stratum_c <- gapindex::calc_biomass_stratum(
  gapdata = gapindex_data_c,
  cpue = cpue_c)

biomass_stratum_c <- 
  dplyr::left_join(
    biomass_stratum_c,
    biomass_ratio_by_species
  )

biomass_stratum_c$RATIO[is.na(biomass_stratum_c$RATIO)] <- 1

biomass_stratum_c$BIOMASS_MT <- biomass_stratum_c$BIOMASS_MT * biomass_stratum_c$RATIO

biomass_stratum_d <- gapindex::calc_biomass_stratum(
  gapdata = gapindex_data_d,
  cpue = cpue_d)


## Calculate aggregated biomass and population abundance across subareas,
## management areas, and regions under both stratum options
biomass_subarea_a <- gapindex::calc_biomass_subarea(
  gapdata = gapindex_data_a,
  biomass_stratum = biomass_stratum_a)

biomass_subarea_a$option <- "a"

biomass_subarea_b <- gapindex::calc_biomass_subarea(
  gapdata = gapindex_data_b,
  biomass_stratum = biomass_stratum_b)

biomass_subarea_b$option <- "b"

biomass_subarea_c <- gapindex::calc_biomass_subarea(
  gapdata = gapindex_data_c,
  biomass_stratum = biomass_stratum_c)

biomass_subarea_c$option <- "c"

biomass_subarea_d <- gapindex::calc_biomass_subarea(
  gapdata = gapindex_data_d,
  biomass_stratum = biomass_stratum_d)

biomass_subarea_d$option <- "d"

## Rename fields and combine stratum and region estimates into one dataframe

merged_biomass <-
  rbind(
    biomass_subarea_a,
    biomass_subarea_b,
    biomass_subarea_c,
    biomass_subarea_d
  )

## calculate CV of total biomass estimate and select pertinent columns
merged_biomass <-
  merged_biomass |>
  dplyr::mutate(
    CV = sqrt(BIOMASS_VAR) / 
      ifelse(test = BIOMASS_MT == 0, 
             yes = 1,
             no = BIOMASS_MT)
  ) |>
  dplyr::select(
    c("SPECIES_CODE", "AREA_ID", "YEAR",
      "BIOMASS_MT", "CV", "option")
  )


biomass_change <-
  merged_biomass |>
  dplyr::filter(BIOMASS_MT > 0) |>
  tidyr::pivot_wider(
    id_cols = c("SPECIES_CODE", "AREA_ID", "YEAR"),
    values_from = c("BIOMASS_MT", "CV"),
    names_from = "option"
  ) |>
  dplyr::mutate(
    BIOMASS_PERC_DIFF_b_a = (BIOMASS_MT_b - BIOMASS_MT_a)/ifelse(BIOMASS_MT_a == 0, 1, BIOMASS_MT_a) * 100,
    BIOMASS_PERC_DIFF_c_a = (BIOMASS_MT_c - BIOMASS_MT_a)/ifelse(BIOMASS_MT_a == 0, 1, BIOMASS_MT_a) * 100,
    BIOMASS_PERC_DIFF_d_a = (BIOMASS_MT_d - BIOMASS_MT_a)/ifelse(BIOMASS_MT_a == 0, 1, BIOMASS_MT_a) * 100,
    CV_DIFF_b_a = CV_b - CV_a,
    CV_DIFF_c_a = CV_c - CV_a,
    CV_DIFF_d_a = CV_d - CV_a
  )


biomass_change_by_option <-
  merged_biomass |>
  dplyr::filter(option == "a") |>
  dplyr::rename(BASE_BIOMASS_MT = BIOMASS_MT,
                BASE_CV = CV) |>
  dplyr::select(-option) |>
  dplyr::inner_join(
    dplyr::filter(
      merged_biomass, option != "a"
    ) 
  ) |>
  dplyr::mutate(
    BIOMASS_PCT_DIFF = (BIOMASS_MT - BASE_BIOMASS_MT) / ifelse(BASE_BIOMASS_MT == 0, 1, BASE_BIOMASS_MT) * 100,
    CV_DIFF = CV-BASE_CV) |>
  dplyr::left_join(
    dplyr::select(
      gapindex_data_a$species,
      SPECIES_CODE,
      COMMON_NAME
    )
  )

ggplot() +
  geom_hline(yintercept = 0) +
  geom_point(
    data = dplyr::filter(biomass_change_by_option, SPECIES_CODE %in% c(21740, 21720, 10210, 10285, 10261, 21735)),
             mapping = aes(x = YEAR, y = BIOMASS_PCT_DIFF, color = option),
    size = rel(2.5)) +
  facet_wrap(~COMMON_NAME) +
  scale_color_colorblind() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Biomass change (%)") +
  ggtitle("Percent change in NBS design based index biomass compared to baseline using three approaches (2010-2023) \n- B: Expand to only the sampled portion of stratum 70 \n- C: Expand to full stratum 70 but adjust for the proportion in sampled area from 2010-2023. \n- D: Expand to full stratum 70 without adjustments ") +
  theme_bw()


ggplot() +
  geom_hline(yintercept = 0) +
  geom_point(
    data = dplyr::filter(biomass_change_by_option, SPECIES_CODE %in% c(21740, 21720, 10210, 10285, 10261, 21735)),
    mapping = aes(x = YEAR, y = CV_DIFF, color = option),
    size = rel(2.5)) +
  facet_wrap(~COMMON_NAME) +
  scale_color_colorblind() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "CV - Base CV") +
  ggtitle("Percent change in NBS design based index CV compared to baseline using three approaches (2010-2023) \n- B: Expand to only the sampled portion of stratum 70 \n- C: Expand to full stratum 70 but adjust for the proportion in sampled area from 2010-2023. \n- D: Expand to full stratum 70 without adjustments ") +
  theme_bw()


## Calculate truncated stratum 70 area

library(akgfmaps)

nbs_layers <- akgfmaps::get_base_layers(select.region = "nbs", set.crs = 3338)

truncated_70 <- nbs_layers$survey.grid |>
  dplyr::filter(STATION %in% drop_stations) |>
  dplyr::group_by(SURVEY_DEFINITION_ID, DESIGN_YEAR) |>
  dplyr::summarise(AREA_M2 = sum(AREA_M2))

truncated_70$AREA_M2 - as.numeric(sf::st_area(truncated_70))

(remaining_area_km2 <- 
  (nbs_layers$survey.strata$AREA_M2[nbs_layers$survey.strata$STRATUM == 70] - truncated_70$AREA_M2) / 1e6)


truncated_intersection_70 <- 
  truncated_70 |>
  dplyr::select(geometry) |> 
  sf::st_intersection(nbs_layers$survey.strata) |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)))

(remaining_area_intersected_km2 <- 
  (nbs_layers$survey.strata$AREA_M2[nbs_layers$survey.strata$STRATUM == 70] - truncated_intersection_70$AREA_M2) / 1e6)

lim <- sf::st_bbox(truncated_70)

ggplot() +
  geom_sf(data = nbs_layers$survey.strata, color = NA, fill = "red", alpha = 0.7) +
  geom_sf(data = truncated_intersection_70, color = NA, fill = "navy", alpha = 0.3) +
  scale_x_continuous(limits = c(lim[1], lim[3])) +
  scale_y_continuous(limits = c(lim[2], lim[4])) + 
  theme_bw()

ggplot() +
  geom_sf(data = nbs_layers$survey.strata, fill = "red", alpha = 0.7) +
  geom_sf(data = nbs_layers$survey.grid, fill = NA, color = "navy") +
  scale_x_continuous(limits = c(lim[1], lim[3])) +
  scale_y_continuous(limits = c(lim[2], lim[4])) + 
  theme_bw()

