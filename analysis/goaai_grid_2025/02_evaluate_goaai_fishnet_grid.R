# Compare fishnet grid options for a new 2025 GOA/AI grid
#
# Created by: Sean Rohan <sean.rohan@noaa.gov>
# Last update: April 18, 2024
#
# Description:
#
# This script intersects prospective grids with stratum shapefiles and compares grids based on the
# number of tow paths that cross grid boundaries and the cumulative proportion of grid cells with
# area > 5 km^2. Grids that have more tow paths crossing boundaries are likely to require a more
# complicated reassignment of trawlable/untrawlable designations, while grids with more small cells
# will have less area to land a trawl.

library(akgfmaps) # Version 3.5.1

set_crs <- "EPSG:3338"

dir.create(here::here("analysis", "goaai_grid_2025", "plots"))

# Filtering criteria
min_towpath_length <- 1000 # meters
min_towpath_cruise <- 200100
min_split_length <- 50 # meters

# X and Y offsets for the grid (m)
offsets <- expand.grid(x_off = seq(from = -2000, to = 2000, by = 1000),
                       y_off = seq(from = -2000, to = 2000, by = 1000))

offsets$option <- 1:nrow(offsets)
offsets$creation_date <- Sys.Date()

# Get towpaths created by navmaps

towpaths <- dplyr::bind_rows(
  sf::st_read(dsn = here::here("analysis", "goaai_grid_2025", "data", "ai_towpath.shp")) |>
    dplyr::mutate(tow_region = "AI"),
  sf::st_read(dsn = here::here("analysis", "goaai_grid_2025", "data", "goa_towpath.shp")) |>
    dplyr::filter(BOTTOM_ < 700) |>
    dplyr::mutate(tow_region = "GOA") # Remove 700-1000 m depths from the 3-boat survey
) |>
  dplyr::filter(CRUISE > min_towpath_cruise)

towpaths$LENGTH <- as.numeric(sf::st_length(towpaths))

# Only include towpaths that are > km
towpaths <- dplyr::filter(towpaths, as.numeric(LENGTH) >= min_towpath_length)

# Load base layers and 2025 GOA stratum shapefile
ai_layers <- akgfmaps::get_base_layers(select.region = "ai", set.crs = set_crs)

goa_layers <- akgfmaps::get_base_layers(select.region = "goa", set.crs = set_crs)

ai_layers$survey.grid$AREA_KM2 <- as.numeric(sf::st_area(ai_layers$survey.grid))/1e6

goa_layers$survey.grid$AREA_KM2 <- as.numeric(sf::st_area(goa_layers$survey.grid))/1e6

goa_strata_2025 <- sf::st_read(
  here::here("analysis", "goaai_grid_2025", "data", "goa_strata_2025.gpkg")
) |>
  sf::st_cast(to = "POLYGON") |>
  dplyr::rename(geometry = geom)

names(goa_strata_2025)[names(goa_strata_2025) != "geometry"] <- toupper(names(goa_strata_2025)[names(goa_strata_2025) != "geometry"])

sf::st_crs(goa_strata_2025) <- set_crs

strata_2025 <- dplyr::bind_rows(
  dplyr::mutate(ai_layers$survey.strata,
                REGION = "AI"),
  dplyr::mutate(goa_strata_2025,
                REGION = "GOA")
) |>
  dplyr::select(REGION, STRATUM)

# Calculate number of towpaths that intersect with current survey grid
baseline <- sf::st_intersection(towpaths,
                                dplyr::bind_rows(
                                  dplyr::mutate(ai_layers$survey.grid,
                                                REGION = "AI"),
                                  dplyr::mutate(goa_layers$survey.grid,
                                                REGION = "GOA")
                                ),
                                do_split = TRUE,
                                group_or_split = FALSE
) |>
  sf::st_cast(to = "LINESTRING")

baseline$LENGTH <- as.numeric(sf::st_length(baseline))

baseline <- baseline |>
  dplyr::filter(LENGTH >= min_split_length)

n_lines <- baseline |>
  sf::st_drop_geometry() |>
  dplyr::group_by(REGION) |>
  dplyr::summarise(n = n()) |>
  dplyr::mutate(grid = "Current")

n_lines$`p<5` <- c(sum(ai_layers$survey.grid$AREA_KM2 < 5)/nrow(ai_layers$survey.grid),
                   sum(goa_layers$survey.grid$AREA_KM2 < 5)/nrow(goa_layers$survey.grid))
n_lines$`p<10` <- c(sum(ai_layers$survey.grid$AREA_KM2 < 10)/nrow(ai_layers$survey.grid),
                    sum(goa_layers$survey.grid$AREA_KM2 < 10)/nrow(goa_layers$survey.grid))


hist(goa_layers$survey.grid$AREA_KM2)

area_ecdf <- data.frame()

for(jj in 1:nrow(offsets)) {

  grid_strata <- sf::st_read(dsn = here::here("analysis", "goaai_grid_2025", "grid_options", "grids",
                                              paste0(gsub(x = Sys.Date(),
                                                          pattern = "-",
                                                          replacement = ""),
                                                     "_goaai_grid_2025_",
                                                     offsets$option[jj],
                                                     ".shp"))
  ) |>
    sf::st_intersection(strata_2025) |>
    sf::st_cast(to = "POLYGON")

  grid_strata$AREA_KM2 <- as.numeric(sf::st_area(grid_strata)) / 1e6

  grid_intersect <- sf::st_intersection(towpaths,
                                        grid_strata,
                                        do_split = TRUE,
                                        group_or_split = FALSE) |>
    sf::st_cast(to = "LINESTRING")

  grid_intersect$LENGTH <- grid_intersect |>
    sf::st_length() |>
    as.numeric()

  grid_intersect <- grid_intersect |>
    dplyr::filter(LENGTH > min_split_length)

  n_lines <- grid_intersect |>
    sf::st_drop_geometry() |>
    dplyr::group_by(REGION) |>
    dplyr::summarise(n = n()) |>
    dplyr::mutate(grid = paste0("Option ", offsets$option[jj]),
                  x_off = offsets$x_off[jj],
                  y_off = offsets$y_off[jj]) |>
    dplyr::bind_rows(n_lines)

  cumulative_area <- expand.grid(area = floor(min(grid_strata$AREA_KM2)):ceiling(max(grid_strata$AREA_KM2)),
                                 prop = numeric(1L),
                                 REGION = c("AI", "GOA"),
                                 grid = paste0("Option ", offsets$option[jj]))


  for(kk in 1:nrow(cumulative_area)) {

    cumulative_area$prop[kk] <- sum(grid_strata$AREA_KM2[grid_strata$REGION == cumulative_area$REGION[kk]] <= cumulative_area$area[kk])/length(grid_strata$REGION[grid_strata$REGION == cumulative_area$REGION[kk]])

  }


  area_ecdf <- dplyr::bind_rows(cumulative_area, area_ecdf)

}

# Compare grid cell sizes based on area
area_ecdf_comparison <- area_ecdf |>
  dplyr::group_by(REGION, area) |>
  dplyr::summarise(min_prop = min(prop)) |>
  dplyr::inner_join(area_ecdf) |>
  dplyr::mutate(prop_diff = prop - min_prop)

write.csv(area_ecdf_comparison,
          file = here::here("analysis", "goaai_grid_2025", "plots", "ecdf_comparison.csv"),
          row.names = FALSE)

ai_5 <- dplyr::filter(area_ecdf_comparison,
                      area == 5,
                      REGION == "AI") |>
  dplyr::select(REGION, grid, prop) |>
  dplyr::rename(`p<5` = prop)

ai_10 <- dplyr::filter(area_ecdf_comparison,
                       area == 10,
                       REGION == "AI") |>
  dplyr::select(REGION, grid, prop) |>
  dplyr::rename(`p<10` = prop)

goa_5 <- dplyr::filter(area_ecdf_comparison,
                      area == 5,
                      REGION == "GOA") |>
  dplyr::select(REGION, grid, prop) |>
  dplyr::rename(`p<5` = prop)

goa_10 <- dplyr::filter(area_ecdf_comparison,
                       area == 10,
                       REGION == "GOA") |>
  dplyr::select(REGION, grid, prop) |>
  dplyr::rename(`p<10` = prop)

# Write the number of grid intersections for each region tables
dplyr::filter(n_lines, REGION == "AI") |>
  dplyr::full_join(ai_5) |>
  dplyr::full_join(ai_10) |>
  dplyr::select(REGION, grid, x_off, y_off, n, `p<5`, `p<10`) |>
  dplyr::arrange(n, `p<5`, `p<10`) |>
  write.csv(
    file = here::here("analysis", "goaai_grid_2025", "plots", "grid_intersects_ai.csv"),
    row.names = FALSE)

dplyr::filter(n_lines, REGION == "GOA") |>
  dplyr::full_join(goa_5) |>
  dplyr::full_join(goa_10) |>
  dplyr::select(REGION, grid, x_off, y_off, n, `p<5`, `p<10`) |>
  dplyr::arrange(n, `p<5`, `p<10`) |>
  write.csv(
    file = here::here("analysis", "goaai_grid_2025", "plots", "grid_intersects_goa.csv"),
    row.names = FALSE)

plotly::ggplotly(
  ggplot() +
    geom_path(data = area_ecdf,
              mapping = aes(x = area, y = prop-min_prop, color = grid)) +
    facet_wrap(~REGION) +
    scale_x_continuous(name = expression(Area~(km^2))) +
    scale_y_continuous(name = "Proportion - Min. Proportion") +
    scale_color_discrete(name = "Grid") +
    theme_bw()
)

