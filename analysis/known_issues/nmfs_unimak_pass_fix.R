# Survey area changes/transfers

library(akgfmaps) # Version 4

nmfs_areas <- akgfmaps::get_nmfs_areas(set.crs = "EPSG:3338")

goa_1984 <- akgfmaps::get_base_layers(select.region = "goa", design.year = 1984, set.crs = "EPSG:3338")

ai_1991 <- akgfmaps::get_base_layers(select.region = "ai", set.crs = "EPSG:3338")

ebs <- akgfmaps::get_base_layers(select.region = "sebs", set.crs = "EPSG:3338")

ebs_slope <- akgfmaps::get_base_layers(select.region = "ebs.slope", set.crs = "EPSG:3338")


goa_1984$survey.area$REGION <- "GOA 1984"
ai_1991$survey.area$REGION <- "AI"
ebs$survey.area$REGION <- "EBS Shelf"
ebs_slope$survey.area$REGION <- "EBS Slope"

comb_surveys <- dplyr::bind_rows(goa_1984$survey.area, ai_1991$survey.area, ebs$survey.area, ebs_slope$survey.area)

unimak_pass <- sf::st_as_sf(data.frame(x = -165.5039329,
                                       y = 54.3475432),
                            coords = c("x", "y"),
                            crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338") |>
  sf::st_buffer(dist = 100000)

unimak_pass_poly <- unimak_pass

unimak_pass <- unimak_pass |>
  sf::st_bbox()

color_values <- c("GOA 1984" = "#E69F00",
                  "AI" = "#56B4E9",
                  "EBS Shelf" = "#009E73",
                  "EBS Slope" = "#F0E442",
                  "Transfer from GOA to AI" = "red",
                  "Transfer from AI to GOA" = "red",
                  "Dropped from GOA, \nnot added to a survey" = "red",
                  "Dropped from GOA, \nalready in EBS shelf" = "red",
                  "Overlap to be addressed by\nEBS and AI redesign" = "red")

# 509
change_509 <- dplyr::filter(nmfs_areas, REP_AREA == 509) |>
  sf::st_intersection(goa_1984$survey.area) |>
  dplyr::mutate(AREA_KM2 = as.numeric(sf::st_area(geometry)/1e6)) |>
  dplyr::select(REP_AREA, SURVEY_DEFINITION_ID, AREA_ID, AREA_KM2)

change_509$AREA_KM2*1e6/goa_1984$survey.area$AREA_M2 * 100

ggplot() +
  geom_sf(data = ai_1991$akland) +
  geom_sf(data = comb_surveys,
          color = NA,
          alpha = 0.5,
          mapping = aes(fill = REGION)) +
  geom_sf(data = change_509,
          color = NA,
          mapping = aes(fill = "Dropped from GOA, \nnot added to a survey")) +
  geom_sf(data = nmfs_areas,
          fill = NA,
          color = "black") +
  geom_sf_text(data = nmfs_areas |> sf::st_intersection(unimak_pass_poly) |> sf::st_centroid(),
               fill = NA,
               color = "black",
               mapping = aes(label = REP_AREA)) +
  ggtitle(label = "Changes in NMFS Area 509") +
  scale_x_continuous(limits = c(unimak_pass['xmin'], unimak_pass['xmax'])) +
  scale_y_continuous(limits = c(unimak_pass['ymin'], unimak_pass['ymax'])) +
  scale_fill_manual(name = "Change", values = color_values) +
  theme_bw() +
  theme(axis.title = element_blank())


# 517
change_517 <-  dplyr::filter(nmfs_areas, REP_AREA == 517) |>
  sf::st_intersection(goa_1984$survey.area) |>
  dplyr::mutate(AREA_KM2 = as.numeric(sf::st_area(geometry)/1e6)) |>
  dplyr::select(REP_AREA, SURVEY_DEFINITION_ID, AREA_ID, AREA_KM2)

change_517$AREA_KM2*1e6/goa_1984$survey.area$AREA_M2 * 100

ggplot() +
  geom_sf(data = ai_1991$akland) +
  geom_sf(data = comb_surveys,
          color = NA,
          alpha = 0.5,
          mapping = aes(fill = REGION)) +
  geom_sf(data = change_517,
          color = NA,
          mapping = aes(fill = "Dropped from GOA, \nalready in EBS shelf")) +
  geom_sf(data = nmfs_areas,
          fill = NA,
          color = "black") +
  geom_sf_text(data = nmfs_areas |> sf::st_intersection(unimak_pass_poly) |> sf::st_centroid(),
               fill = NA,
               color = "black",
               mapping = aes(label = REP_AREA)) +
  ggtitle(label = "Changes in NMFS Area 517") +
  scale_x_continuous(limits = c(unimak_pass['xmin'], unimak_pass['xmax'])) +
  scale_y_continuous(limits = c(unimak_pass['ymin'], unimak_pass['ymax'])) +
  scale_fill_manual(name = "Change", values = color_values) +
  theme_bw() +
  theme(axis.title = element_blank())


# 519
change_519 <-  dplyr::filter(nmfs_areas, REP_AREA == 519) |>
  sf::st_intersection(goa_1984$survey.area) |>
  dplyr::mutate(AREA_KM2 = as.numeric(sf::st_area(geometry)/1e6)) |>
  dplyr::select(REP_AREA, SURVEY_DEFINITION_ID, AREA_ID, AREA_KM2)

change_519$AREA_KM2*1e6/goa_1984$survey.area$AREA_M2 * 100
change_519$AREA_KM2*1e6/ai_1991$survey.area$AREA_M2 * 100

change_519_bbox <- sf::st_bbox(change_519)
ggplot() +
  geom_sf(data = comb_surveys,
          color = NA,
          alpha = 0.5,
          mapping = aes(fill = REGION)) +
  geom_sf(data = change_519,
          mapping = aes(fill = "Transfer from GOA to AI")) +
  geom_sf(data = nmfs_areas,
          fill = NA,
          color = "black") +
  geom_sf(data = ai_1991$akland) +
  geom_sf_text(data = nmfs_areas |> sf::st_intersection(unimak_pass_poly) |> sf::st_centroid(),
               fill = NA,
               color = "black",
               mapping = aes(label = REP_AREA)) +
  ggtitle(label = "Changes in NMFS Area 519") +
  scale_x_continuous(limits = c(unimak_pass['xmin'], unimak_pass['xmax'])) +
  scale_y_continuous(limits = c(unimak_pass['ymin'], unimak_pass['ymax'])) +
  scale_fill_manual(name = "Change", values = color_values) +
  theme_bw() +
  theme(axis.title = element_blank())

overlap_519 <- sf::st_intersection(ai_1991$survey.area, ebs_slope$survey.area) |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry))) |>
  dplyr::select(AREA_M2)

overlap_519$AREA_M2/ebs_slope$survey.area$AREA_M2 * 100
overlap_519$AREA_M2/ai_1991$survey.area$AREA_M2 * 100

ggplot() +
  geom_sf(data = comb_surveys,
          color = NA,
          alpha = 0.5,
          mapping = aes(fill = REGION)) +
  geom_sf(data = overlap_519,
          mapping = aes(fill = "Overlap to be addressed by\nEBS and AI redesign")) +
  geom_sf(data = nmfs_areas,
          fill = NA,
          color = "black") +
  geom_sf(data = ai_1991$akland) +
  geom_sf_text(data = nmfs_areas |> sf::st_intersection(unimak_pass_poly) |> sf::st_centroid(),
               fill = NA,
               color = "black",
               mapping = aes(label = REP_AREA)) +
  ggtitle(label = "Survey overlap in NMFS Area 519") +
  scale_x_continuous(limits = c(unimak_pass['xmin'], unimak_pass['xmax'])) +
  scale_y_continuous(limits = c(unimak_pass['ymin'], unimak_pass['ymax'])) +
  scale_fill_manual(name = "Change", values = color_values) +
  theme_bw() +
  theme(axis.title = element_blank())


# 610
change_610 <- dplyr::filter(nmfs_areas, REP_AREA == 610) |>
  sf::st_intersection(ai_1991$survey.area) |>
  # sf::st_intersection(goa_1984$survey.area) |>
  dplyr::mutate(AREA_KM2 = as.numeric(sf::st_area(geometry)/1e6)) |>
  dplyr::select(REP_AREA, SURVEY_DEFINITION_ID, AREA_ID, AREA_KM2)

change_610$AREA_KM2*1e6/goa_1984$survey.area$AREA_M2 * 100
change_610$AREA_KM2*1e6/ai_1991$survey.area$AREA_M2 * 100

change_610_bbox <- sf::st_bbox(change_610)

ggplot() +
  geom_sf(data = comb_surveys,
          color = NA,
          alpha = 0.5,
          mapping = aes(fill = REGION)) +
  geom_sf(data = change_610,
          color = NA,
          mapping = aes(fill = "Transfer from AI to GOA")) +
  geom_sf(data = nmfs_areas,
          fill = NA,
          color = "black") +
  geom_sf(data = ai_1991$akland) +
  geom_sf_text(data = nmfs_areas |>
                 dplyr::filter(REP_AREA == 610) |>
                 sf::st_crop(change_610_bbox) |>
                 sf::st_centroid(),
               fill = NA,
               color = "black",
               mapping = aes(label = REP_AREA)) +
  ggtitle(label = "Changes in NMFS Area 610") +
  scale_x_continuous(limits = c(change_610_bbox['xmin'], change_610_bbox['xmax'])) +
  scale_y_continuous(limits = c(change_610_bbox['ymin'], change_610_bbox['ymax'])) +
  scale_fill_manual(name = "Change", values = color_values) +
  theme_bw() +
  theme(axis.title = element_blank())
