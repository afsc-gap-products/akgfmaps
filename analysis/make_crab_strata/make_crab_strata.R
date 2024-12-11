library(akgfmaps) # 3.6.0
library(ggthemes)


# Function to combine polygons
combine_polygons <- function(x) {

  out <- x |>
    dplyr::mutate(ID_VAR = 1) |>
    dplyr::group_by(ID_VAR) |>
    dplyr::summarise(do_union = TRUE) |>
    dplyr::select(-ID_VAR)

  return(out)

}

map_layers <- akgfmaps::get_base_layers(select.region = "sebs",
                                        set.crs = "EPSG:3338")

sg_area <- sf::st_area(map_layers$survey.grid)

dir.create(here::here("crab_strata_dec2024"), showWarnings = FALSE)

# EBS border
ebs_grid <- sf::st_read(system.file("extdata/bs_grid.shp", package = "akgfmaps")) |>
  dplyr::filter(STATIONID %in% c(
                  akgfmaps::get_survey_stations(
                    select.region = "sebs",
                    include.corners = FALSE
                    ), "Q-24")
                )

ebs_border <- ebs_grid |>
  combine_polygons()


# Create Pribilof and St. Matthew MTCA polygons
prib_mtca_grid <- dplyr::filter(ebs_grid,
                           STATIONID %in%
                             c(
                               paste0(LETTERS[6:10], "-18"),
                               paste0(LETTERS[6:10], "-19"),
                               paste0(LETTERS[6:10], "-20"),
                               paste0(LETTERS[6:10], "-21"),
                               paste0(LETTERS[6:10], "-22")
                             )
)

sm_mtca_grid <- dplyr::filter(ebs_grid,
                         STATIONID %in%
                           c(
                             paste0(LETTERS[15:17], "-23"),
                             paste0(LETTERS[14:17], "-24"),
                             paste0(LETTERS[14:17], "-25"),
                             paste0(LETTERS[14:17], "-26"),
                             paste0(LETTERS[15:17], "-27")
                           )
)

prib_mtca <- combine_polygons(prib_mtca_grid)
sm_mtca <- combine_polygons(sm_mtca_grid)

prib_bkc_single_grid <-
  dplyr::filter(ebs_grid,
                STATIONID %in% c(
                  paste0(LETTERS[3:11], "-01"),
                  paste0(LETTERS[3:11], "-18"),
                  paste0(LETTERS[5:11], "-19"),
                  paste0(LETTERS[5:11], "-20"),
                  paste0(LETTERS[5:11], "-21"),
                  paste0(LETTERS[5:11], "-22"),
                  paste0(LETTERS[6:11], "-23"),
                  paste0(LETTERS[6:11], "-24"),
                  paste0(LETTERS[6:11], "-25"),
                  paste0(LETTERS[7:11], "-26"),
                  "K-27")
  ) |>
  dplyr::filter(!(STATIONID %in% prib_mtca_grid$STATIONID))

prib_rkc_single_grid <-
  dplyr::filter(ebs_grid,
                STATIONID %in% c(
                  paste0(LETTERS[3:11], "-18"),
                  paste0(LETTERS[5:11], "-19"),
                  paste0(LETTERS[5:11], "-20"),
                  paste0(LETTERS[5:11], "-21"),
                  paste0(LETTERS[5:11], "-22"),
                  paste0(LETTERS[6:11], "-23"),
                  paste0(LETTERS[6:11], "-24"),
                  paste0(LETTERS[6:11], "-25"),
                  paste0(LETTERS[7:11], "-26"),
                  "K-27")
  ) |>
  dplyr::filter(!(STATIONID %in% prib_mtca_grid$STATIONID))



sm_bkc_single_grid <- dplyr::filter(ebs_grid,
                               STATIONID %in% c(
                                 paste0(LETTERS[14:19], "-22"),
                                 paste0(LETTERS[14:19], "-23"),
                                 paste0(LETTERS[13:19], "-24"),
                                 paste0(LETTERS[13:19], "-25"),
                                 paste0(LETTERS[13:19], "-26"),
                                 paste0(LETTERS[13:19], "-27"),
                                 paste0(LETTERS[13:19], "-28")
                               )
) |>
  dplyr::filter(!(STATIONID %in% sm_mtca_grid$STATIONID))

prib_rkc_single <- combine_polygons(prib_rkc_single_grid)
prib_bkc_single <- combine_polygons(prib_bkc_single_grid)
sm_bkc_single <- combine_polygons(sm_bkc_single_grid)

# Bairdi east of 166
# east_166 <- sf::st_read(dsn = "NewStrataPolygons.gdb", layer = "Bairdi_East")
east_166_grid <- ebs_grid |>
  dplyr::filter(STATIONID %in%
                  c(
                    paste0(LETTERS[1:15], "-04"),
                    paste0(LETTERS[1:14], "-05"), "Z-05",
                    paste0(LETTERS[1:14], "-06"),
                    paste0(LETTERS[2:14], "-07"),
                    paste0(LETTERS[2:13], "-08"),
                    paste0(LETTERS[3:12], "-09"),
                    paste0(LETTERS[4:11], "-10"),
                    paste0(LETTERS[5:11], "-11"),
                    paste0(LETTERS[5:11], "-12"),
                    paste0(LETTERS[6:11], "-13"),
                    paste0(LETTERS[6:11], "-14"),
                    paste0(LETTERS[7:10], "-15"),
                    paste0(LETTERS[8:10], "-16")
                    )
  )

west_166_grid <- ebs_grid |>
  dplyr::filter(!(STATIONID %in%
                  c(east_166_grid$STATIONID,
                    sm_mtca_grid$STATIONID,
                    prib_mtca_grid$STATIONID))
                )

west_166 <- combine_polygons(west_166_grid)
east_166 <- combine_polygons(east_166_grid)

# Bristol bay RKC ----
# bb_rkc <- sf::st_read(dsn = "NewStrataPolygons.gdb",
#                            layer = "BristolBay")

bb_rkc <- ebs_grid |>
  dplyr::filter(STATIONID %in%
                  c(
                    paste0(LETTERS[2:11], "-01"),
                    paste0(LETTERS[1:11], "-02"),
                    paste0(LETTERS[1:11], "-03"),
                    paste0(LETTERS[1:11], "-04"),
                    paste0(LETTERS[1:11], "-05"), "Z-05",
                    paste0(LETTERS[1:11], "-06"),
                    paste0(LETTERS[2:11], "-07"),
                    paste0(LETTERS[2:11], "-08"),
                    paste0(LETTERS[3:11], "-09"),
                    paste0(LETTERS[4:11], "-10"),
                    paste0(LETTERS[5:11], "-11"),
                    paste0(LETTERS[5:11], "-12"),
                    paste0(LETTERS[6:11], "-13"),
                    paste0(LETTERS[6:11], "-14"),
                    paste0(LETTERS[7:10], "-15"),
                    paste0(LETTERS[8:10], "-16")
                  )
  ) |>
  combine_polygons()

bb_rkc$STRATUM <- "Bristol Bay"
bb_rkc$STOCK <- "Bristol Bay RKC"
bb_rkc$AREA_M2 <- sf::st_area(bb_rkc) |>
  as.numeric()

bb_rkc <- bb_rkc[c("STOCK", "STRATUM", "AREA_M2")]

ggplot() +
  geom_sf(data = bb_rkc) +
  facet_wrap(STOCK~STRATUM)


# Norton Sound RKC ----
norton_rkc <- sf::st_read(system.file("extdata/bs_grid.shp", package = "akgfmaps")) |>
  dplyr::filter(STATIONID %in%
                  c(
                    paste0("V-0", 2:3),
                    paste0("W-0", 2:3),
                    paste0("X-0", 2:3),
                    paste0("Y-0", 2:4),
                    paste0("ZZ-0", 2:5),
                    paste0("AA-0", 2:8),
                    paste0("BB-0", 2:9), "BB-10",
                    paste0("CC-0", 2:9), "CC-10",
                    paste0("DD-0", 2:3),
                    "EE-02", "FF-02"
                  )
                ) |>
  combine_polygons()

norton_rkc$STRATUM <- "Norton Sound"
norton_rkc$STOCK <- "Norton Sound RKC"
norton_rkc$AREA_M2 <- sf::st_area(norton_rkc) |>
  as.numeric()

norton_rkc <- norton_rkc[c("STOCK", "STRATUM", "AREA_M2")]

ggplot() +
  geom_sf(data = norton_rkc) +
  facet_wrap(STOCK~STRATUM)

# Pribilof RKC ----
prib_rkc <- dplyr::bind_rows(prib_mtca, prib_rkc_single)

prib_rkc$STRATUM <- c("Pribilof MTCA", "Pribilof Single")
prib_rkc$STOCK <- "Pribilof Islands RKC"
prib_rkc$AREA_M2 <- sf::st_area(prib_rkc) |>
  as.numeric()

prib_rkc <- prib_rkc[c("STOCK", "STRATUM", "AREA_M2")]

ggplot() +
  geom_sf(data = prib_rkc) +
  facet_wrap(STOCK~STRATUM)

# Pribilof BKC ----
prib_bkc <- dplyr::bind_rows(prib_mtca, prib_bkc_single)

prib_bkc$STRATUM <- c("Pribilof MTCA", "Pribilof Single")
prib_bkc$STOCK <- "Pribilof Islands BKC"
prib_bkc$AREA_M2 <- sf::st_area(prib_bkc) |>
  as.numeric()

prib_bkc <- prib_bkc[c("STOCK", "STRATUM", "AREA_M2")]

ggplot() +
  geom_sf(data = prib_bkc) +
  facet_wrap(STOCK~STRATUM)

# St. Matthew BKC ---
sm_bkc <- dplyr::bind_rows(sm_bkc_single, sm_mtca)

sm_bkc$STRATUM <- c("St. Matthew Single", "St. Matthew MTCA")
sm_bkc$STOCK <- "St. Matthew BKC"
sm_bkc$AREA_M2 <- sf::st_area(sm_bkc) |>
  as.numeric()

sm_bkc <- sm_bkc[c("STOCK", "STRATUM", "AREA_M2")]

ggplot() +
  geom_sf(data = sm_bkc) +
  facet_wrap(STOCK~STRATUM)


# Tanner West ---
tanner_west <- dplyr::bind_rows(west_166, prib_mtca, sm_mtca)
tanner_west$STRATUM <- c("West 166", "Pribilof MTCA", "St. Matthew MTCA")
tanner_west$AREA_M2 <- sf::st_area(tanner_west) |>
  as.numeric()
tanner_west$STOCK <- "Tanner W"
tanner_west <- tanner_west[c("STOCK", "STRATUM", "AREA_M2")]

ggplot() +
  geom_sf(data = tanner_west) +
  facet_wrap(STOCK~STRATUM)

# Tanner East ----
tanner_east <- east_166
tanner_east$STOCK <- "Tanner E"
tanner_east$STRATUM <- "East 166"
tanner_east$AREA_M2 <- sf::st_area(tanner_east) |>
  as.numeric()
tanner_east <- tanner_east[c("STOCK", "STRATUM", "AREA_M2")]

ggplot() +
  geom_sf(data = tanner_east) +
  facet_wrap(STOCK~STRATUM)


# Snow crab

snow <- ebs_grid |>
  dplyr::filter(!(STATIONID %in% c(prib_mtca_grid$STATIONID, sm_mtca_grid$STATIONID))) |>
  combine_polygons() |>
  dplyr::bind_rows(prib_mtca, sm_mtca)

snow$STRATUM <- c("Single", "Pribilof MTCA", "St. Matthew MTCA")
snow$STOCK <- "EBS snow crab"
snow$AREA_M2 <- sf::st_area(snow) |>
  as.numeric()

snow <- snow[c("STOCK", "STRATUM", "AREA_M2")]

ggplot() +
  geom_sf(data = snow) +
  facet_wrap(STOCK~STRATUM)

all_crab <-
  dplyr::bind_rows(bb_rkc,
                   prib_rkc,
                   norton_rkc,
                   prib_bkc,
                   sm_bkc,
                   tanner_east,
                   tanner_west,
                   snow)

ggplot() +
  geom_sf(data = all_crab,
          mapping = aes(fill = STOCK)) +
  facet_wrap(~STOCK)

sf::st_write(obj = all_crab,
             dsn = here::here("analysis", "make_crab_strata", "all_crab_from_akgfmaps_grid.gpkg"),
             overwrite = TRUE,
             delete_dsn = TRUE)
