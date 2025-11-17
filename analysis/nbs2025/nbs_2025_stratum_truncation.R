# Truncate stratum 70 for NBS DESIGN_YEAR = 2025
#
# Sean Rohan
# Last update: September 18, 2025
#
# The 2025 NBS survey did not sample 7 stations in stratum 70.
# The preferred approach to estimate biomass for stratum 70
# is to exclude the portion of the stratum that was not sampled
# from biomass estimation. This requires producing area
# calculations and polygons for stratum 70 without the area
# represented by the missing stations for use in the
# GAP_PRODUCTS area table and presentations.

library(akgfmaps)
library(gapindex)
library(tidyterra)

drop_stations <- c("T-03", "S-03", "R-03", "T-02", "S-02", "R-02", "R-01")

nbs_layers <- akgfmaps::get_base_layers(
  select.region = "nbs",
  set.crs = "EPSG:3338"
  )

area_to_remove <- nbs_layers$survey.grid |>
  dplyr::filter(STATION %in% drop_stations) |>
  dplyr::group_by(SURVEY_DEFINITION_ID, DESIGN_YEAR) |>
  dplyr::summarise() |>
  sf::st_intersection(nbs_layers$survey.strata) |>
  dplyr::select(geometry)

truncated_survey_strata <- area_to_remove |>
  dplyr::bind_rows(nbs_layers$survey.strata) |>
  sf::st_intersection() |>
  dplyr::select(-n.overlaps, -origins) |>
  dplyr::filter(
    !is.na(STRATUM),
    sf::st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")
    ) |>
  sf::st_cast(to = "POLYGON") |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)))

nbs_strata_2025  <-
  truncated_survey_strata |>
  dplyr::select(
    SURVEY_DEFINITION_ID,
    DESIGN_YEAR,
    AREA_ID = STRATUM,
    AREA_M2,
    geometry
  )

nbs_survey_area_2025 <-
  truncated_survey_strata |>
  dplyr::group_by(SURVEY_DEFINITION_ID) |>
  dplyr::summarise() |>
  dplyr::mutate(
    DESIGN_YEAR = 2025,
    AREA_ID = 99902,
    AREA_M2 = as.numeric(sf::st_area(geometry))
  ) |>
  dplyr::select(
    SURVEY_DEFINITION_ID,
    DESIGN_YEAR,
    AREA_ID,
    AREA_M2,
    geometry
  )


p_full_area <-
  ggplot() +
  geom_sf(
    data = nbs_layers$survey.strata,
    alpha = 0.5
  ) +
  geom_sf(
    data = nbs_layers$akland,
    fill = "black", color = NA) +
  geom_sf_text(
    data = nbs_layers$survey.strata,
    mapping = aes(label = STRATUM)
  ) +
  scale_x_continuous(
    limits = nbs_layers$plot.boundary$x,
    breaks = nbs_layers$lon.breaks
  ) +
  scale_y_continuous(
    limits = nbs_layers$plot.boundary$y,
    breaks = nbs_layers$lat.breaks
  ) +
  ggtitle("Full NBS survey area (DESIGN_YEAR = 2022)") +
  theme_bw() +
  theme(axis.title = element_blank())


p_truncated <-
  ggplot() +
  geom_sf(
    data = nbs_strata_2025,
    alpha = 0.5
  ) +
  geom_sf(
    data = nbs_layers$akland,
    fill = "black", color = NA) +
  geom_sf_text(
    data = nbs_layers$survey.strata,
    mapping = aes(label = STRATUM)
  ) +
  scale_x_continuous(
    limits = nbs_layers$plot.boundary$x,
    breaks = nbs_layers$lon.breaks
  ) +
  scale_y_continuous(
    limits = nbs_layers$plot.boundary$y,
    breaks = nbs_layers$lat.breaks
  ) +
  ggtitle("Truncated stratum 70 (DESIGN_YEAR = 2025)") +
  theme_bw() +
  theme(axis.title = element_blank())


cowplot::plot_grid(
  p_full_area,
  p_truncated,
  nrow = 1
)

1-nbs_strata_2025$AREA_M2[nbs_strata_2025$AREA_ID == 70]/nbs_layers$survey.strata$AREA_M2[nbs_layers$survey.strata$STRATUM == 70]

1-nbs_survey_area_2025$AREA_M2/nbs_layers$survey.area$AREA_M2

# Produce CPUE maps for pollock, cod, YKP, and plaice.

library(gapindex)

channel <- gapindex::get_connected(check_access = FALSE)

spp_codes <- c(10210, 10261, 10285, 21720, 21740)

gapdata <- gapindex::get_data(
  survey_set = "NBS",
  year_set = 2010:2025,
  spp_codes = spp_codes,
  channel = channel
)

cpue <- gapindex::calc_cpue(
  gapdata = gapdata
)


both_survey_areas <-
  dplyr::bind_rows(
    nbs_layers$survey.area,
    nbs_layers$survey.area,
    nbs_layers$survey.area,
    nbs_layers$survey.area,
    nbs_layers$survey.area,
    nbs_layers$survey.area,
    nbs_survey_area_2025
  )

both_survey_areas$lyr <- c(2010, 2017, 2019, 2021:2023, 2025)

for(ii in 1:length(spp_codes)) {

  spp_stack <-
    akgfmaps::make_idw_stack(
    x = cpue |>
      dplyr::arrange(YEAR) |>
      dplyr::filter(SPECIES_CODE == spp_codes[ii]) |>
    dplyr::rename(
      LATITUDE = LATITUDE_DD_START,
      LONGITUDE = LONGITUDE_DD_START,
      CPUE_KGHA = CPUE_KGKM2
    ),
    grouping.vars = "YEAR",
    region = "bs.north"
  )

  species_stack <-
    spp_stack$extrapolation.stack |>
    terra::rast() |>
    terra::mask(nbs_layers$survey.area)

  names(species_stack) <- names(spp_stack$extrapolation.stack)

  species_stack$`2025` <-
    terra::mask(species_stack$`2025`, nbs_survey_area_2025)

  p_species <-
    ggplot() +
    geom_sf(
      data = nbs_layers$akland,
      fill = "black", color = NA) +
    geom_spatraster(
      data = species_stack
    ) +
    geom_sf(
      data = both_survey_areas,
      fill = NA, color = "black"
    ) +
    scale_fill_manual(
      name = expression(CPUE~(kg%.%km^-2)),
      na.translate = FALSE,
      values = c("white", scales::brewer_pal(palette = "Blues")(5))
    ) +
    scale_x_continuous(
      limits = nbs_layers$plot.boundary$x,
      breaks = nbs_layers$lon.breaks
    ) +
    scale_y_continuous(
      limits = nbs_layers$plot.boundary$y,
      breaks = nbs_layers$lat.breaks
    ) +
    ggtitle(label = gapdata$species$REPORT_NAME_SCIENTIFIC[gapdata$species$SPECIES_CODE == spp_codes[ii]]) +
    facet_wrap(~lyr, ncol = 4) +
    theme_bw()

  png(filename =
        here::here("analysis", "nbs2025", paste0(spp_codes[ii], "_idw.png")),
      width = 8, height = 3.5, units = "in", res = 300)
  print(p_species)
  dev.off()

}
