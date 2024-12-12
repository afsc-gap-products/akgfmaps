library(akgfmaps)

test_get_base_layers <- function() {


  region_df <- expand.grid(region = c("ai", "sebs", "ebs", "nbs", "goa", "ebs.slope", "ecs"),
              crs = c(4269, 3338))

  region_maps <-  vector(mod = "list",
                         length = nrow(region_df))

  for(ii in 1:nrow(region_df)) {

    map_layers <- get_base_layers(select.region = region_df$region[ii],
                                  set.crs = region_df$crs[ii])

    if(region_df$region[ii] %in% c("ebs.slope", "ecs")) {

      region_maps[[ii]]  <- ggplot() +
        geom_sf(data = map_layers$akland) +
        geom_sf(data = map_layers$bathymetry,
                mapping = aes(color = "bathymetry")) +
        geom_sf(data = map_layers$survey.strata,
                mapping = aes(color = "strata"), fill = NA) +
        geom_sf(dat = map_layers$survey.area,
                mapping = aes(color = "area"), fill = NA, linewidth = 1.5) +
        geom_sf(data = map_layers$graticule) +
        scale_color_manual(values = c("bathymetry" = "purple", "grid" = "black", "strata" = "blue", "area" = "red")) +
        ggtitle(label = paste0("select.region = ", region_df$region[ii],
                               " set.crs = ", region_df$crs[ii],
                               "(", ii, "/", nrow(region_df), ")")) +
        scale_x_continuous(limits = map_layers$plot.boundary$x,
                           breaks = map_layers$lon.breaks) +
        scale_y_continuous(limits = map_layers$plot.boundary$y,
                           breaks = map_layers$lat.breaks) +
        theme_minimal()


    } else {

      region_maps[[ii]]  <- ggplot() +
        geom_sf(data = map_layers$akland) +
        geom_sf(data = map_layers$survey.grid,
                mapping = aes(color = "grid")) +
        geom_sf(data = map_layers$bathymetry,
                mapping = aes(color = "bathymetry")) +
        geom_sf(data = map_layers$survey.strata,
                mapping = aes(color = "strata"), fill = NA) +
        geom_sf(dat = map_layers$survey.area,
                mapping = aes(color = "area"), fill = NA, linewidth = 1.5) +
        geom_sf(data = map_layers$graticule) +
        scale_color_manual(values = c("bathymetry" = "purple", "grid" = "black", "strata" = "blue", "area" = "red")) +
        ggtitle(label = paste0("select.region = ", region_df$region[ii],
                               " set.crs = ", region_df$crs[ii],
                               "(", ii, "/", nrow(region_df), ")")) +
        scale_x_continuous(limits = map_layers$plot.boundary$x,
                           breaks = map_layers$lon.breaks) +
        scale_y_continuous(limits = map_layers$plot.boundary$y,
                           breaks = map_layers$lat.breaks) +
        theme_minimal()

    }

    print(region_maps[[ii]])

  }


}


test_get_base_layers()




test_get_crab_strata() {

  units <-


}

get_bsierp_regions()
