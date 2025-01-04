# Build Conner 2022 stratum and survey area shapefiles

library(akgfmaps)

conner_2022_ebs <- sf::st_read(here::here("analysis", "bering_sea_spatial_2022", "Bering_Sea_Spatial_Products_2022.gdb"),
            layer = "EBS_strata_Conner2022") |>
  sf::st_transform(crs = "EPSG:3338")


conner_2022_nbs <- sf::st_read(here::here("analysis", "bering_sea_spatial_2022", "Bering_Sea_Spatial_Products_2022.gdb"),
                               layer = "NBS_strata_Conner2022") |>
  sf::st_transform(crs = "EPSG:3338")

dplyr::bind_rows(conner_2022_ebs, conner_2022_nbs) |>
  dplyr::select(-Shape_Length, - Shape_Area) |>
  sf::st_write(here::here("analysis", "bering_sea_spatial_2022", "ebs_strata.shp"),
               append = FALSE)

dplyr::bind_rows(
  conner_2022_nbs |>
    dplyr::group_by(SURVEY = "NBS_SHELF") |>
    dplyr::summarize(Area_KM2 = sum(Area_KM2)),
  conner_2022_ebs |>
    dplyr::group_by(SURVEY = "EBS_SHELF") |>
    dplyr::summarize(Area_KM2 = sum(Area_KM2))
) |>
  sf::st_write(here::here("analysis", "bering_sea_spatial_2022", "ebs_survey_boundary.shp"),
               append = FALSE)

conner_2019_slope <- sf::st_read(here::here("analysis", "bering_sea_spatial_2022", "Bering_Sea_Spatial_Products_2022.gdb"),
                                 layer = "EBS_slope_strata_Conner2019")
