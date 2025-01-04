# Example: Access data directly from a geopackage
# SCRUG, akgfmaps, January 22, 2025
# Created by Sean Rohan <sean.rohan@noaa.gov>

# Loading layers directly from built-in Geopackage
# Geopackages are an open, standards-based, platform-independent format for storing geospatial data.
# They are based on SQLite, can store multiple layers and data types in one file.

library(akgfmaps)
library(dplyr)

# Filepath to geopackage with AFSC bottom trawl survey layers
gpkg_fpath <- system.file("extdata", "afsc_bottom_trawl_surveys.gpkg", package = "akgfmaps")

# Inpsect the layers at the dsn (data source name).
# There are four layers in individual tables (called 'layers' in sf)
sf::st_layers(dsn = gpkg_fpath)

# Load all of of the features from the survey_area layer
survey_areas <- sf::st_read(dsn = gpkg_fpath,
                            layer = "survey_area")

survey_areas

# Plot the survey areas
ggplot() +
  geom_sf(data = survey_areas,
          mapping = aes(color = SURVEY_NAME),
          fill = NA) +
  facet_wrap(~DESIGN_YEAR) +
  theme(legend.position = "bottom")

# Filter to retreive the NBS survey area
nbs_area <- dplyr::filter(survey_areas, SURVEY_DEFINITION_ID == 143)

ggplot() +
  geom_sf(data = nbs_area,
          mapping = aes(color = SURVEY_NAME)) +
  facet_wrap(~DESIGN_YEAR) +
  theme(legend.position = "bottom")

# Alternatively, we can retrieve features from the geopackage using a SQLite query.
# In this case, retrieve GOA stratum polygons for the 1984 and 2025 design years.
goa_strata <- sf::st_read(dsn = gpkg_fpath,
                          query =
                          "SELECT
                            AREA_TYPE,
                            SURVEY_DEFINITION_ID,
                            DESIGN_YEAR,
                            AREA_ID AS STRATUM,
                            AREA_M2,
                            GEOM
                            FROM SURVEY_STRATA
                            WHERE SURVEY_DEFINITION_ID = 47"
                          )

head(goa_strata)
table(goa_strata$DESIGN_YEAR)

# Plotting the 1984 and 2025 GOA survey strata
ggplot() +
  geom_sf(data = goa_strata,
          mapping = aes(fill = factor(STRATUM)),
          color = NA) +
  scale_fill_viridis_d(option = "H") +
  facet_wrap(~DESIGN_YEAR, nrow = 2) +
  theme(legend.position = "none",
        axis.title = element_blank())


# Land layers
sf::st_layers(dsn = system.file("extdata", "land_layers.gpkg", package = "akgfmaps"))
