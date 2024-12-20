# Test akgfmaps V4 functions

library(akgfmaps)
library(ggthemes)
library(navmaps)

test_gbl_v4 <- function(select.region, set.crs, design.year = NULL, channel = NULL, ...) {

  layers <- get_base_layers(select.region = select.region,
                            set.crs = set.crs,
                            design.year = design.year)

  if(!is.null(channel)) {

    layers$survey.strata$SF_AREA_M2 <- sf::st_area(layers$survey.strata) |>
      as.numeric()

    layers$survey.area$SF_AREA_M2 <- sf::st_area(layers$survey.area) |>
      as.numeric()

    strata <- RODBC::sqlQuery(channel = channel,
                              query = paste0("SELECT SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID AS STRATUM, AREA_NAME, AREA_TYPE AS GP_AREA_TYPE, AREA_KM2 AS GP_AREA_KM2 FROM GAP_PRODUCTS.AREA
  WHERE SURVEY_DEFINITION_ID IN (", paste(unique(layers$survey.strata$SURVEY_DEFINITION_ID), collapse = ", "),
                                             ") AND DESIGN_YEAR IN (", paste(unique(layers$survey.strata$DESIGN_YEAR), collapse = ", "),
                                             ") AND AREA_ID IN (", paste(unique(layers$survey.strata$STRATUM), collapse = ", "), ")")
    )

    layers$survey.strata <- dplyr::left_join(layers$survey.strata, strata)

    layers$survey.strata$PCT_DIFF_SHP_GP <- (layers$survey.strata$AREA_M2 - layers$survey.strata$GP_AREA_KM2*1e6)/layers$survey.strata$AREA_M2*100
    layers$survey.strata$PCT_DIFF_SHP_SF <- (layers$survey.strata$AREA_M2 - layers$survey.strata$SF_AREA_M2)/layers$survey.strata$AREA_M2*100
    layers$survey.strata$PCT_DIFF_GP_SF <- (layers$survey.strata$GP_AREA_KM2*1e6 - layers$survey.strata$SF_AREA_M2)/(layers$survey.strata$GP_AREA_KM2*1e6)*100

    area <- RODBC::sqlQuery(channel = channel,
                            query = paste0("SELECT SURVEY_DEFINITION_ID, DESIGN_YEAR, AREA_ID, AREA_NAME, AREA_TYPE AS GP_AREA_TYPE, AREA_KM2 AS GP_AREA_KM2 FROM GAP_PRODUCTS.AREA
  WHERE SURVEY_DEFINITION_ID IN (", paste(unique(layers$survey.area$SURVEY_DEFINITION_ID), collapse = ", "),
                                           ") AND DESIGN_YEAR IN (", paste(unique(layers$survey.area$DESIGN_YEAR), collapse = ", "),
                                           ") AND AREA_ID IN (", paste(unique(layers$survey.area$AREA_ID), collapse = ", "), ")")
    )

    layers$survey.area <- dplyr::left_join(layers$survey.area, area)

    layers$survey.area$PCT_DIFF_SHP_GP <- (layers$survey.area$AREA_M2 - layers$survey.area$GP_AREA_KM2*1e6)/layers$survey.area$AREA_M2*100
    layers$survey.area$PCT_DIFF_SHP_SF <- (layers$survey.area$AREA_M2 - layers$survey.area$SF_AREA_M2)/layers$survey.area$AREA_M2*100
    layers$survey.area$PCT_DIFF_GP_SF <- (layers$survey.area$GP_AREA_KM2*1e6 - layers$survey.area$SF_AREA_M2)/layers$survey.area$GP_AREA_KM2*100



  }

  p1 <- ggplot() +
    geom_sf(data = layers$akland) +
    geom_sf(data = layers$survey.area,
            mapping = aes(color = "Survey area",
                          fill = "Survey area"),
            alpha = 0.7) +
    geom_sf(data = layers$survey.strata,
            mapping = aes(color = "Survey strata"),
            fill = NA) +
    geom_sf(data = layers$survey.grid,
            mapping = aes(color = "Survey grid"),
            fill = NA) +
    geom_sf(data = layers$graticule,
            alpha = 0.3) +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    scale_x_continuous(limits = layers$plot.boundary$x,
                       breaks = layers$lon.breaks) +
    scale_y_continuous(limits = layers$plot.boundary$y,
                       breaks = layers$lat.breaks) +
    theme_bw()

  p_stratum <- ggplot() +
    geom_sf(data = layers$akland) +
    geom_sf(data = layers$survey.strata,
            mapping = aes(fill = factor(STRATUM)),
            color = NA) +
    geom_sf(data = layers$graticule,
            alpha = 0.3) +
    scale_fill_viridis_d() +
    scale_color_brewer() +
    scale_x_continuous(limits = layers$plot.boundary$x,
                       breaks = layers$lon.breaks) +
    scale_y_continuous(limits = layers$plot.boundary$y,
                       breaks = layers$lat.breaks) +
    theme_bw()

  return(list(plot_all = p1,
              plot_stratum = p_stratum,
              layers = layers))

}

channel <- navmaps::get_connected(schema = "AFSC")

# channel <- NULL

goa_layers <- test_gbl_v4(select.region = "goa",
                          set.crs = "EPSG:3338",
                          design.year = 2025,
                          channel = channel)
goa_layers$plot_all
goa_layers$plot_stratum

goa_layers$layers$survey.strata |>
  sf::st_write(dsn = "examine_goa_2025.gpkg")

goa_layers_1984 <- test_gbl_v4(select.region = "goa", set.crs = "EPSG:3338", design.year = 1984, channel = channel)
goa_layers_1984$plot_all
goa_layers_1984$plot_stratum
goa_layers_1984$layers$survey.strata

ai_layers <- test_gbl_v4(select.region = "ai", set.crs = "EPSG:3338", channel = channel)
ai_layers$plot
ai_layers$plot_all
ai_layers$layers$survey.strata$PCT_DIFF_SHP_GP

ai_sebs_layers <- test_gbl_v4(select.region = c("ai", "sebs"), set.crs = "EPSG:3338")
ai_sebs_layers$plot_all

ai_sebs_layers <- test_gbl_v4(select.region = c("ai", "sebs", "goa"), set.crs = "EPSG:3338")
ai_sebs_layers$plot

ebs_layers <- test_gbl_v4(select.region = "ebs", set.crs = "EPSG:3338", channel = channel)
ebs_layers$plot_all
ebs_layers$layers$survey.strata$PCT_DIFF_SHP_GP

ai_west_layers <- test_gbl_v4(select.region = c("ai.west", "ai.central"), set.crs = "EPSG:3338")
ai_west_layers$plot

ai_west_layers$p2

ai_goa_layers <- test_gbl_v4(select.region = c("ai.east", "goa.west"), set.crs = "EPSG:3338")

ai_goa_layers$p2

# ggplot() +
#   geom_sf(data = ai_goa_layers$layers$inpfc.strata,
#           mapping = aes(fill = INPFC_STRATUM)) +
#   coord_sf(xlim = ai_goa_layers$layers$plot.boundary$x,
#            ylim = ai_goa_layers$layers$plot.boundary$y)
