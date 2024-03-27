library(akgfmaps)

bbrkc <- get_crab_strata(select.unit = "bbrkc", set.crs = "EPSG:3338")

pirkc <- get_crab_strata(select.unit = "pirkc", set.crs = "EPSG:3338")

pibkc <- get_crab_strata(select.unit = "pibkc", set.crs = "EPSG:3338")

nsrkc <- get_crab_strata(select.unit = "nsrkc", set.crs = "EPSG:3338")

smbkc <- get_crab_strata(select.unit = "smbkc", set.crs = "EPSG:3338")

ebstc <- get_crab_strata(select.unit = "ebstc", set.crs = "EPSG:3338")

ebssc <- get_crab_strata(select.unit = "ebssc", set.crs = "EPSG:3338")

crab_strata <- dplyr::bind_rows(bbrkc,
                                pirkc,
                                pibkc,
                                nsrkc,
                                smbkc,
                                ebstc,
                                ebssc)

# sf::st_read(here::here("inst", "extdata", "crab_strata", "BBRKC_strata.shp")) |>
#   dplyr::select(-TEMP) |>
#   dplyr::mutate(OBJECTID = 1) |>
#   dplyr::select(OBJECTID, Shape_Leng, Shape_Area, geometry) |>
#   sf::st_write(here::here("inst", "extdata", "crab_strata", "BBRKC_strata.shp"),
#                append = FALSE)
#
# sf::st_read(here::here("inst", "extdata", "crab_strata", "Norton_RKC_Strata.shp")) |>
#   dplyr::mutate(OBJECTID = 1) |>
#   dplyr::select(OBJECTID, Shape_Leng, Shape_Area, geometry) |>
#   sf::st_write(here::here("inst", "extdata", "crab_strata", "Norton_RKC_Strata.shp"),
#                append = FALSE)

ggplot() +
  geom_sf(data = crab_strata,
          mapping = aes(fill = STRATUM)) +
  facet_wrap(~STRATUM)

ggplot() +
  geom_sf(data = bbrkc,
          mapping = aes(fill = factor(OBJECTID))) +
  facet_wrap(~OBJECTID)

ggplot() +
  geom_sf(data = pirkc,
          mapping = aes(fill = factor(OBJECTID))) +
  facet_wrap(~OBJECTID)

ggplot() +
  geom_sf(data = pibkc,
          mapping = aes(fill = factor(OBJECTID))) +
  facet_wrap(~OBJECTID)

ggplot() +
  geom_sf(data = nsrkc,
          mapping = aes(fill = factor(OBJECTID))) +
  facet_wrap(~OBJECTID)

ggplot() +
  geom_sf(data = smbkc,
          mapping = aes(fill = factor(OBJECTID))) +
  facet_wrap(~OBJECTID)

ggplot() +
  geom_sf(data = ebstc,
          mapping = aes(fill = factor(OBJECTID))) +
  facet_wrap(~OBJECTID)

ggplot() +
  geom_sf(data = ebssc,
          mapping = aes(fill = factor(OBJECTID))) +
  facet_wrap(~OBJECTID)


