library(remotes)

remotes::install_github("afsc-gap-products/akgfmaps@dev2")

library(akgfmaps)


# Is select.unit an appropriate name for the function and argument? Does the documentation look correct?
?get_crab_strata

# The function returns crab strata and nothing else. Should additional layers be returned?
# I think get_crab_strata() could have an option to return all of the same layers as get_base_layers(),
# but with crab strata instead of groundfish strata. A bit hestiant because it would make the
# functions somewhat redundant.

# If all layers were returned, should the default be to return the whole EBS, specific subregions,
# or focal areas (e.g. PIRKC would set boundaries around the PIRKC management unit around the Pribs)?

bbrkc_strata <- get_crab_strata(select.unit = "bbrkc", set.crs = "EPSG:3338")

# Do the column names (STRATUM, AREA_M2) make sense?
# Note that all-caps will be the norm for all fields (except geometry) in akgfmaps 4.
# Undecided about whether the area column will be a units or numeric classs.
bbrkc_strata


# This function shows each of the files in different projections
test_get_crab_strata <- function() {

  crab_units <- data.frame(unit = rep(c("bbrkc", "pirkc", "pibkc", "smbkc", "nsrkc", "ebssc", "ebstc"), 2),
                           region = rep(c("sebs", "sebs", "sebs", "sebs", "nbs", "sebs", "sebs"), 2),
                           crs = c(rep(3338, 7), rep(4269, 7)))

  base_layers <- list(
    'sebs' = list(`3338` = get_base_layers(select.region  = "sebs", set.crs = "EPSG:3338"),
                  `4269` = get_base_layers(select.region  = "sebs", set.crs = "EPSG:4269")),
    'nbs' = list(`3338` = get_base_layers(select.region  = "nbs", set.crs = "EPSG:3338"),
                 `4269` = get_base_layers(select.region  = "nbs", set.crs = "EPSG:4269"))
  )

  crab_maps <-  vector(mod = "list",
                       length = nrow(crab_units))

  crab_layers <-  vector(mod = "list",
                         length = nrow(crab_units))

  for(ii in 1:nrow(crab_units)) {

    crab_layers[[ii]] <- get_crab_strata(select.unit = crab_units$unit[ii],
                                         set.crs = crab_units$crs[ii])

    crab_maps[[ii]]  <- ggplot() +
      geom_sf(data = base_layers[[crab_units$region[ii]]][[as.character(crab_units$crs)[ii]]]$akland) +
      geom_sf(data = base_layers[[crab_units$region[ii]]][[as.character(crab_units$crs)[ii]]]$survey.grid,
              mapping = aes(color = "grid")) +
      geom_sf(data = crab_layers[[ii]],
              mapping = aes(color = "strata"), fill = NA, linewidth = 1.5) +
      geom_sf(dat = base_layers[[crab_units$region[ii]]][[as.character(crab_units$crs)[ii]]]$survey.area,
              mapping = aes(color = "area"), fill = NA) +
      geom_sf(data = base_layers[[crab_units$region[ii]]][[as.character(crab_units$crs)[ii]]]$graticule) +
      scale_color_manual(name = "Layer", values = c("grid" = "black", "strata" = "red", "area" = "blue")) +
      ggtitle(label = paste0("select.region = ", crab_units$unit[ii],
                             ", set.crs = ", crab_units$crs[ii],
                             "(", ii, "/", nrow(crab_units), ")")) +
      scale_x_continuous(limits = base_layers[[crab_units$region[ii]]][[as.character(crab_units$crs)[ii]]]$plot.boundary$x,
                         breaks = base_layers[[crab_units$region[ii]]][[as.character(crab_units$crs)[ii]]]$lon.breaks) +
      scale_y_continuous(limits = base_layers[[crab_units$region[ii]]][[as.character(crab_units$crs)[ii]]]$plot.boundary$y,
                         breaks = base_layers[[crab_units$region[ii]]][[as.character(crab_units$crs)[ii]]]$lat.breaks) +
      theme_minimal()

    print(crab_maps[[ii]])

  }




  return(list(crab_maps = crab_maps,
              crab_layers = crab_layers))

}

crab_maps_and_layers <- test_get_crab_strata()

