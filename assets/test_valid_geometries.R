library(akgfmaps)


check_values <- expand.grid(valid_regions = c("bs.south",
                              "sebs",
                              "bs.all",
                              "ebs",
                              "bs.north",
                              "nbs",
                              "ecs",
                              "ebs.ecs",
                              "ai",
                              "ai.west",
                              "ai.central",
                              "ai.east",
                              "goa",
                              "goa.west",
                              "goa.east",
                              "ebs.slope",
                              "bssa1",
                              "bssa2",
                              "bssa3",
                              "bssa4",
                              "bssa5",
                              "bssa6"),
            check_crs = c("EPSG:3338", "WGS84")
)


invalid_geoms <- data.frame()

for(ii in 1:nrow(check_values)) {

  print(paste0(ii, "/", nrow(check_values)))

  map_layers <- akgfmaps::get_base_layers(select.region = as.character(check_values$valid_regions[ii]),
                                          set.crs = as.character(check_values$check_crs[ii]),
                                          fix.invalid.geom = FALSE)

  layer_names <- c("akland", "survey.area", "survey.strata", "survey.grid", "bathymetry", "graticule", "inpfc.strata")

  keep_names <- layer_names[which(layer_names %in% names(map_layers))]

  for(jj in 1:length(keep_names)) {

    if(is.null(map_layers[[keep_names[jj]]])) {
      next
    }

    all_valid <- all(sf::st_is_valid(map_layers[[keep_names[jj]]]))

    if(!all_valid) {

      invalid_geoms <- dplyr::bind_rows(invalid_geoms,
                                        data.frame(region = as.character(check_values$valid_regions[ii]),
                                                   crs = as.character(check_values$check_crs[ii]),
                                                   layer = keep_names[jj])
      )

    }

  }


}
