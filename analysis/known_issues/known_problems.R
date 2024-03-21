# A few known issues and inconsistencies as of December 2023

library(akgfmaps)
library(cowplot)

slope_layers <- akgfmaps::get_base_layers(select.region = "ebs.slope", set.crs = "EPSG:3338")
ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs", set.crs = "EPSG:3338")
goa_layers <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "EPSG:3338")
ai_layers <- akgfmaps::get_base_layers(select.region = "ai", set.crs = "EPSG:3338")
chukchi_layers <- akgfmaps::get_base_layers(select.region = "ecs", set.crs = "EPSG:3338")
ebs_survey_bathy <- akgfmaps::get_survey_bathymetry(select.region = "ebs", set.crs = "EPSG:3338")

bss1 <- akgfmaps::get_base_layers(select.region = "bssa1", set.crs = "EPSG:3338")[c("survey.strata", "plot.boundary")]
bss2 <- akgfmaps::get_base_layers(select.region = "bssa2", set.crs = "EPSG:3338")[c("survey.strata", "plot.boundary")]  
bss3 <- akgfmaps::get_base_layers(select.region = "bssa3", set.crs = "EPSG:3338")[c("survey.strata", "plot.boundary")]
bss4 <- akgfmaps::get_base_layers(select.region = "bssa4", set.crs = "EPSG:3338")[c("survey.strata", "plot.boundary")]
bss5 <- akgfmaps::get_base_layers(select.region = "bssa5", set.crs = "EPSG:3338")[c("survey.strata", "plot.boundary")]
bss6 <- akgfmaps::get_base_layers(select.region = "bssa6", set.crs = "EPSG:3338")[c("survey.strata", "plot.boundary")]

bss1$survey.strata$SURVEY <- bss2$survey.strata$SURVEY <- bss3$survey.strata$SURVEY <- bss4$survey.strata$SURVEY <- bss5$survey.strata$SURVEY <- bss6$survey.strata$SURVEY <- "EBS_SLOPE"


# 1. Some shapefiles have descriptive and traceable metadata, others do not. -----------------------

# NO CODE

# 2. Stratum field names don't match ---------------------------------------------------------------
names(slope_layers$survey.strata)
names(ebs_layers$survey.strata)
names(goa_layers$survey.strata)
names(ai_layers$survey.strata)
names(chukchi_layers$survey.strata)

# 3. Survey field names don't match ----------------------------------------------------------------
names(slope_layers$survey.grid) # No grid
names(ebs_layers$survey.grid)
names(goa_layers$survey.grid)
names(ai_layers$survey.grid)
names(chukchi_layers$survey.grid) # No grid

# 4. Survey area field names don't match -----------------------------------------------------------
names(slope_layers$survey.area)
names(ebs_layers$survey.area)
names(goa_layers$survey.area)
names(ai_layers$survey.area)
names(chukchi_layers$survey.area)

# 5. Bathmetry names match but the contours are inaccurate -----------------------------------------
names(slope_layers$bathymetry)
names(ebs_layers$bathymetry)
names(goa_layers$bathymetry)
names(ai_layers$bathymetry)
names(chukchi_layers$bathymetry)
names(ebs_survey_bathy)

# 6. Survey bathymetry versus bathyemtry  file -----------------------------------------------------
# In the EBS, there's a 'survey bathymetry' file that likely has more accurate contours except around 
# islands. Neither has contours around Nunivak and St. Lawrence. 
# The survey bathymetry shapefile lacks descriptive field information (e.g. depth values for lines)

head(ebs_survey_bathy)
head(ebs_layers$bathymetry)

ggplot() +
  geom_sf(data = dplyr::bind_rows(dplyr::mutate(ebs_layers$bathymetry, name = "Regional bathymetry"), 
                                  dplyr::mutate(ebs_survey_bathy, name = "'Survey' bathymetry")),
          mapping = aes(color = name)) +
  geom_sf(data = ebs_layers$akland, color = NA) +
  coord_sf(xlim = c(ebs_layers$plot.boundary$x),
           ylim = c(ebs_layers$plot.boundary$y)) +
  scale_x_continuous(breaks = ebs_layers$lon.breaks) +
  scale_y_continuous(breaks = ebs_layers$lat.breaks) +
  scale_color_manual(values = c("red", "black")) +
  theme_bw()

# 7. Field classes differ --------------------------------------------------------------------------
class(ebs_layers$survey.strata$Stratum)
class(bss1$survey.strata$STRATUM)


# 8. EBS shelf and slope survey don't snap together ------------------------------------------------
cowplot::plot_grid(
  ggplot() +
    geom_sf(data = dplyr::bind_rows(bss1$survey.strata, 
                                    ebs_layers$survey.strata),
            mapping = aes(fill = SURVEY),
            color = NA,
            alpha = 0.5) + 
    coord_sf(xlim = bss1$plot.boundary$x,
             ylim = bss1$plot.boundary$y) +
    scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
    ggtitle(label = "Slope sub-area 1") +
    theme_bw() +
    theme(legend.position = "none"),
  ggplot() +
    geom_sf(data = dplyr::bind_rows(bss2$survey.strata, 
                                    ebs_layers$survey.strata),
            mapping = aes(fill = SURVEY),
            color = NA,
            alpha = 0.5) + 
    coord_sf(xlim = bss2$plot.boundary$x,
             ylim = bss2$plot.boundary$y) +
    scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
    ggtitle(label = "Slope sub-area 2") +
    theme_bw() +
    theme(legend.position = "none"),
  ggplot() +
    geom_sf(data = dplyr::bind_rows(bss3$survey.strata, 
                                    ebs_layers$survey.strata),
            mapping = aes(fill = SURVEY),
            color = NA,
            alpha = 0.5) + 
    coord_sf(xlim = bss3$plot.boundary$x,
             ylim = bss3$plot.boundary$y) +
    scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
    ggtitle(label = "Slope sub-area 3") +
    theme_bw() +
    theme(legend.position = "none"),
  ggplot() +
    geom_sf(data = dplyr::bind_rows(bss4$survey.strata, 
                                    ebs_layers$survey.strata),
            mapping = aes(fill = SURVEY),
            color = NA,
            alpha = 0.5) + 
    coord_sf(xlim = bss4$plot.boundary$x,
             ylim = bss4$plot.boundary$y) +
    scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
    ggtitle(label = "Slope sub-area 4") +
    theme_bw() +
    theme(legend.position = "none"),
  ggplot() +
    geom_sf(data = dplyr::bind_rows(bss5$survey.strata, 
                                    ebs_layers$survey.strata),
            mapping = aes(fill = SURVEY),
            color = NA,
            alpha = 0.5) + 
    coord_sf(xlim = bss5$plot.boundary$x,
             ylim = bss5$plot.boundary$y) +
    scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
    ggtitle(label = "Slope sub-area 5") +
    theme_bw() +
    theme(legend.position = "none"),
  ggplot() +
    geom_sf(data = dplyr::bind_rows(bss6$survey.strata, 
                                    ebs_layers$survey.strata),
            mapping = aes(fill = SURVEY),
            color = NA,
            alpha = 0.5) + 
    coord_sf(xlim = bss6$plot.boundary$x,
             ylim = bss6$plot.boundary$y) +
    scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
    ggtitle(label = "Slope sub-area 6") +
    theme_bw() +
    theme(legend.position = "none")
)


# 9. Survey areas don't snap together around Unimak Pass -------------------------------------------
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

ggplot() +
  geom_sf(data = ebs_layers$akland) +
  geom_sf(data = survey_regions,
          mapping = aes(fill = SURVEY),
          alpha = 0.5,
          color = NA) +
  coord_sf(xlim = c(unimak_pass['xmin'], unimak_pass['xmax']),
           ylim = c(unimak_pass['ymin'], unimak_pass['ymax'])) +
  scale_fill_manual(values = c('EBS_SHELF'= "#E69F00", 
                               'EBS_SLOPE' = "#56B4E9", 
                               'AI' = "#009E73", 
                               'GOA' = "#000000")) +
  theme_bw()


# 10. Hole in survey strata around Islands of Four Mountains ---------------------------------------

islands_of_four <- sf::st_as_sf(data.frame(x = -170.0252331,
                                           y = 52.8358947),
                                coords = c("x", "y"),
                                crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338") |>
  sf::st_buffer(dist = 70000) |>
  sf::st_bbox()

ggplot() +
  geom_sf(data = ebs_layers$akland) +
  geom_sf(data = survey_regions,
          mapping = aes(fill = SURVEY),
          alpha = 0.5,
          color = NA) +
  geom_sf(data = ai_layers$survey.grid, fill = NA) +
  geom_sf(data = goa_layers$survey.grid, fill = NA) +
  coord_sf(xlim = c(islands_of_four['xmin'], islands_of_four['xmax']),
           ylim = c(islands_of_four['ymin'], islands_of_four['ymax'])) +
  scale_fill_manual(values = c('EBS_SHELF'= "#E69F00", 
                               'EBS_SLOPE' = "#56B4E9", 
                               'AI' = "#009E73", 
                               'GOA' = "#000000")) +
  theme_bw()



# 11. EBS area follows depth/land contours in some areas but not others ----------------------------
# Is there a historical reason for this?

nunivak <- sf::st_as_sf(data.frame(x = -169,
                        y = 60.3302564),
             coords = c("x", "y"),
             crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338") |>
  sf::st_buffer(dist = 250000) |>
  sf::st_bbox()

ggplot() +
  geom_sf(data = ebs_layers$akland) +
  geom_sf(data = ebs_layers$survey.area,
          mapping = aes(fill = SURVEY),
          alpha = 0.5,
          color = NA) +
  coord_sf(xlim = c(nunivak['xmin']+2e5, nunivak['xmax']),
           ylim = c(nunivak['ymin'], nunivak['ymax'])) +
  scale_fill_manual(values = c('EBS_SHELF'= "#E69F00", 
                               'NBS_SHELF' = "#0072B2")) +
  theme_bw()


#12. Invalid shapefiles ----------------------------------------------------------------------------

library(akgfmaps)

file_paths <- list.files(system.file("extdata", package = "akgfmaps"), full.names = TRUE, pattern = ".shp")

file_paths <- file_paths[!grepl(pattern = ".xml", x = file_paths)]

valid_geom <- logical()
valid_geom_wgs84 <- logical()

for(ii in 1:length(file_paths)) {
  
  dat <- sf::st_read(file_paths[ii])
  
  dat_wgs84 <- sf::st_transform(dat, crs = "WGS84")
  
  valid_geom <- c(valid_geom,  all(sf::st_is_valid(dat)))
  valid_geom_wgs84 <- c(valid_geom_wgs84,  all(sf::st_is_valid(dat_wgs84)))
  
  
}

basename(file_paths[!valid_geom])
basename(file_paths[!valid_geom_wgs84])

invalid_paths <- file_paths[!valid_geom]

invalid_info <- vector(mode = "list", length = length(invalid_paths))

for(jj in 1:length(invalid_paths)) {
  
  dat <- sf::st_read(file_paths[jj])
  
  dat$reason <- sf::st_is_valid(dat, reason = TRUE)
  
  invalid_info[[jj]] <- dat
  
}
