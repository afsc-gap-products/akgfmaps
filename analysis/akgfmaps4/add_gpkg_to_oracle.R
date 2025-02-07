library(akgfmaps)
library(gapindex)

user <- "rohans"

gpkg_path <- system.file("extdata", "afsc_bottom_trawl_surveys.gpkg", package = "akgfmaps")

channel <- gapindex::get_connected()

# View the layers in the geopackage
gpkg_layers <- sf::st_layers(gpkg_path)

for(ii in 1:length(gpkg_layers)) {

  # Convert Alaska Albers Equal Area to NAD83
  station_table <- sf::st_read(
    gpkg_path,
    layer = gpkg_layers$name[ii]
  ) |>
    sf::st_transform(crs = "NAD83") |>
    dplyr::mutate(SURVEY_DEFINITION_ID = as.integer(SURVEY_DEFINITION_ID),
                  AREA_ID = as.integer(SURVEY_DEFINITION_ID),
                  DESIGN_YEAR = as.integer(DESIGN_YEAR))

  # Convert feature geometries to well-known text
  station_table$WKT_NAD83 <- sf::st_as_text(station_table$geom)

  # Well-known binary could be a more compact alternative to WKT using this
  wkb_example <- sf::st_as_binary(station_table$geom)

  # Drop table if it exists
  try(
    RODBC::sqlDrop(channel = channel,
                     sqtable = paste0(user, gpkg_layers$name[ii]))
  )

  RODBC::sqlSave(channel = channel,
                 dat = sf::st_drop_geometry(station_table),
                 tablename = paste0(user, gpkg_layers$name[ii]),
                 append = TRUE, # need append = TRUE because of access permissions
                 rownames = FALSE,
                 colnames = FALSE,
                 verbose = FALSE,
                 safer = FALSE,
                 addPK = TRUE,
                 fast = TRUE,
                 test = FALSE,
                 nastring = NULL)
}

