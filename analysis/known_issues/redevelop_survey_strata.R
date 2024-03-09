library(akgfmaps)

ai_grid_3338 <- akgfmaps::get_base_layers(select.region = "ai", set.crs = "EPSG:3338")$survey.grid

ai_grid_union <- dplyr::select(ai_grid_3338, ID) |>
  dplyr::group_by(ID) |>
  summarise()

ai_grid_union$area <- sf::st_area(ai_grid_union)

ggplot() +
  geom_sf(data = dplyr::arrange(ai_grid_union, -area)[1,],
          linewidth = 3) +
  geom_sf_text(data = dplyr::arrange(ai_grid_union, -area)[1,] |>
            sf::st_centroid(),
            mapping = aes(label = ID))

ggplot() +
  geom_sf(data = dplyr::arrange(ai_grid_union, -area)[2,],
          linewidth = 3) +
  geom_sf_text(data = dplyr::arrange(ai_grid_union, -area)[2,] |>
                 sf::st_centroid(),
               mapping = aes(label = ID))


ggplot() +
  geom_sf(data = dplyr::arrange(ai_grid_union, -area)[2,],
          linewidth = 3) +
  geom_sf_text(data = dplyr::arrange(ai_grid_union, -area)[2,] |>
                 sf::st_centroid(),
               mapping = aes(label = ID))

nmfs_regions <- get_base_layers(select.region = "nmfs", set.crs = "EPSG:3338")

nmfs_regions <- sf::st_make_valid(nmfs_regions)


# nmfs_regions <- sf::st_read("C:/Users/sean.rohan/Work/afsc/WIP/fix_grids/NMFS Reporting Areas.shp")

plot(nmfs_regions)


ggplot() +
  geom_sf(data = dplyr::filter(nmfs_regions, REP_AREA %in% c(508:524, 550))) +
  geom_sf_text(data = dplyr::filter(nmfs_regions, REP_AREA %in% c(508:524, 550)),
               mapping = aes(label = REP_AREA))

nmfs_ebs <- dplyr::filter(nmfs_regions, REP_AREA %in% c(508:524, 550)) |>
  dplyr::group_by('EBS') |>
  dplyr::summarise(do_union = TRUE) |>
  sf::st_union(by_feature = TRUE)

nmfs_regions <- st_buffer(nmfs_regions, .0001)
nmfs_regions <- st_difference(nmfs_regions, nmfs_regions['geometry'])

ggplot() +
  geom_sf(data =
  )


