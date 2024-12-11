library(akgfmaps)

slope_layers <- akgfmaps::get_base_layers(select.region = "ebs.slope", set.crs = "EPSG:3338")
ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs", set.crs = "EPSG:3338")
goa_layers <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "EPSG:3338")
ai_layers <- akgfmaps::get_base_layers(select.region = "ai", set.crs = "EPSG:3338")

nmfs_areas <- akgfmaps::get_nmfs_areas(set.crs = "EPSG:3338")

survey_regions <- dplyr::bind_rows(
  dplyr::filter(ebs_layers$survey.strata, SURVEY == "EBS_SHELF"),
  dplyr::mutate(slope_layers$survey.strata,
                SURVEY = "EBS_SLOPE"),
  dplyr::mutate(ai_layers$survey.strata,
                SURVEY = "AI"),
  dplyr::mutate(goa_layers$survey.strata,
                SURVEY = "GOA")
)

unimak_pass <- sf::st_as_sf(data.frame(x = -165.5039329,
                                       y = 54.3475432),
                            coords = c("x", "y"),
                            crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338") |>
  sf::st_buffer(dist = 100000) |>
  sf::st_bbox()



bbox <- st_polygon(list(rbind(c(unimak_pass[1], unimak_pass[2]),
                         c(unimak_pass[1], unimak_pass[4]),
                         c(unimak_pass[3], unimak_pass[4]),
                         c(unimak_pass[3], unimak_pass[2]),
                         c(unimak_pass[1], unimak_pass[2])))) %>%
  st_sfc(crs = 3338) %>%
  st_as_sf()

nmfs_masked_centroid <- sf::st_intersection(nmfs_areas, bbox) |>
  sf::st_centroid()

ggplot() +
  geom_sf(data = ebs_layers$akland) +
  geom_sf(data = survey_regions,
          mapping = aes(fill = SURVEY),
          alpha = 0.5,
          color = NA) +
  geom_sf(data = slope_layers$bathymetry, 
          mapping = aes(color = factor(METERS)), 
          linewidth = 1.2) +
  geom_sf(data = nmfs_areas, fill = NA, linewidth = 1.5) +
  geom_sf_text(data = nmfs_masked_centroid, 
               mapping = aes(label = REP_AREA)) +
  coord_sf(xlim = c(unimak_pass['xmin'], unimak_pass['xmax']),
           ylim = c(unimak_pass['ymin'], unimak_pass['ymax'])) +
  scale_color_brewer(name = "Depth (m)", palette = "Blues", direction = 1) +
  scale_fill_manual(name = "Survey", values = c('EBS_SHELF'= "#E69F00", 
                               'EBS_SLOPE' = "#56B4E9", 
                               'AI' = "#009E73", 
                               'GOA' = "#000000")) +
  theme_bw() +
  theme(axis.title = element_blank())
