library(gapindex)
library(akgfmaps)
library(cowplot)

channel <- gapindex::get_connected(check_access = FALSE)

species_code <- 10261
select.region <- "GOA"

dat <- gapindex::get_data(
  year_set = c(2023, 2025), 
  survey_set = "GOA",
  spp_codes = species_code,
  channel = channel
)

cpue <- 
  gapindex::calc_cpue(gapdata = dat)

stratum_biomass <- 
  gapindex::calc_biomass_stratum(gapdata = dat, cpue = cpue) |>
  dplyr::mutate(DESIGN_YEAR = ifelse(YEAR < 2025, 1984, 2025))

cpue_sf <- 
  cpue |>
  sf::st_as_sf(
    coords = c("LONGITUDE_DD_START", "LATITUDE_DD_START"),
    crs = "WGS84"
  ) |>
  sf::st_transform(crs = "EPSG:3338") |>
  dplyr::arrange(CPUE_KGKM2)

map_layers <-  akgfmaps::get_base_layers(select.region = "GOA", set.crs = "EPSG:3338")

map_layers_1984 <-  akgfmaps::get_base_layers(select.region = "GOA", set.crs = "EPSG:3338", design.year = 1984)

survey_strata <- 
  dplyr::bind_rows(
    map_layers$survey.strata,
    map_layers_1984$survey.strata
  )

stratum_biomass_sf <- 
  dplyr::inner_join(
    survey_strata,
    stratum_biomass
  ) |>
  dplyr::mutate(CPUE_KGKM2_MEAN = ifelse(is.na(CPUE_KGKM2_MEAN), 0, CPUE_KGKM2_MEAN))


vals <- c(stratum_biomass_sf$CPUE_KGKM2_MEAN, cpue_sf$CPUE_KGKM2)
vals <- vals[vals > 0]

# Make plots

pal_range <- range(vals, na.rm = TRUE)

ggplot() +
  geom_sf(
    data = map_layers$akland,
    color = NA,
    fill = "grey40"
  ) +
  geom_sf(
    data = stratum_biomass_sf, 
    mapping = aes(fill = CPUE_KGKM2_MEAN+1),
    # mapping = aes(fill = CPUE_KGKM2_MEAN),
    color = NA
  ) +
  geom_sf(
    data = stratum_biomass_sf,
    color = "grey20",
    fill = NA,
    alpha = 0.5,
    linewidth = 0.01
  ) +
  geom_sf(
    data = cpue_sf[cpue_sf$CPUE_KGKM2 == 0, ],
    color = "white",
    shape = 4,
    size = rel(1.5)
  ) +
  geom_sf(
    data = cpue_sf[cpue_sf$CPUE_KGKM2 > 0, ],
    mapping = aes(fill = CPUE_KGKM2),
    shape = 21
  ) +
  scale_fill_viridis_c(
    name = expression(CPUE*' '*(kg/km^2)),
    direction = 1,
    na.value = "white",
    limits = pal_range,
    transform = "log10",
    option = "D"
  ) +
  scale_x_continuous(
    limits = map_layers$plot.boundary$x,
    breaks = map_layers$lon.breaks
  ) +
  scale_y_continuous(
    limits = map_layers$plot.boundary$y,
    breaks = map_layers$lat.breaks
  ) +
  facet_wrap(~YEAR,
             ncol = 1) +
  theme_bw() +
  theme(axis.title = element_blank()) +
  ggtitle(dat$species$REPORT_NAME_SCIENTIFIC)



ggplot() +
  geom_sf(
    data = map_layers$akland,
    color = NA,
    fill = "grey40"
  ) +
  geom_sf(
    data = stratum_biomass_sf, 
    mapping = aes(fill = CPUE_KGKM2_MEAN),
    color = NA
  ) +
  geom_sf(
    data = stratum_biomass_sf,
    color = "grey20",
    fill = NA,
    alpha = 0.5,
    linewidth = 0.01
  ) +
  scale_fill_viridis_c(
    name = expression(CPUE*' '*(kg/km^2)),
    direction = 1,
    na.value = "white",
    limits = pal_range,
    transform = "log10",
    option = "D"
  ) +
  scale_x_continuous(
    limits = map_layers$plot.boundary$x,
    breaks = map_layers$lon.breaks
  ) +
  scale_y_continuous(
    limits = map_layers$plot.boundary$y,
    breaks = map_layers$lat.breaks
  ) +
  facet_wrap(~YEAR,
             ncol = 1) +
  theme_bw() +
  theme(axis.title = element_blank())


ggplot() +
  geom_sf(
    data = map_layers$akland,
    color = NA,
    fill = "grey40"
  ) +
  geom_sf(
    data = stratum_biomass_sf,
    mapping = aes(fill = BIOMASS_MT),
  ) +
  # geom_sf(
  #   data = stratum_biomass_sf,
  #   color = "grey20",
  #   fill = NA,
  #   alpha = 0.5,
  #   linewidth = 0.01
  # ) +
  scale_fill_viridis_c(
    name = expression(Biomass*' '*(mt)),
    direction = 1,
    na.value = "white",
    transform = "log10",
    option = "C"
  ) +
  scale_x_continuous(
    limits = map_layers$plot.boundary$x,
    breaks = map_layers$lon.breaks
  ) +
  scale_y_continuous(
    limits = map_layers$plot.boundary$y,
    breaks = map_layers$lat.breaks
  ) +
  facet_wrap(~YEAR,
             nrow = 2) +
  theme_bw() +
  theme(axis.title = element_blank())


