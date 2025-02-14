library(akgfmaps)
library(gapindex)
library(xlsx)
library(DBI)

user <- "ROHANS"

gpkg_path <- system.file("extdata", "afsc_bottom_trawl_surveys.gpkg", package = "akgfmaps")

channel <- gapindex::get_connected()


con <- odbc::dbConnect(drv = odbc::odbc(), dsn = "AFSC", uid = "ROHANS", pwd = "Lepidopsetta999#$")

dbReadTable(conn = con, statement = "SURVEY_AREA")
dbListTables(con)

sf::st_read(dsn = con, layer = "SURVEY_AREA", geometry_column = "WKT_NAD83")


# View the layers in the geopackage
gpkg_layers <- sf::st_layers(gpkg_path)

for(ii in 1:nrow(gpkg_layers)) {

  # Convert Alaska Albers Equal Area to NAD83
  station_table <- sf::st_read(
    gpkg_path,
    layer = gpkg_layers$name[ii]) |>
    sf::st_transform(crs = "NAD83") |>
    dplyr::mutate(SURVEY_DEFINITION_ID = as.integer(SURVEY_DEFINITION_ID),
                  AREA_ID = as.integer(SURVEY_DEFINITION_ID),
                  DESIGN_YEAR = as.integer(DESIGN_YEAR))

  # Convert feature geometries to well-known text
  station_table$WKT_NAD83 <- sf::st_as_text(station_table$geom)

  apply(station_table, MARGIN = 2, FUN = function(x) {unlist(sf::st_as_binary(x, EWKB = TRUE))})

  # Well-known binary could be a more compact alternative to WKT using this
  # station_table$wkb_example <- sf::st_as_binary(station_table$geom)

  # Drop table if it exists
  try(
    RODBC::sqlDrop(channel = channel,
                     sqtable = paste0(user, ".", toupper(gpkg_layers$name[ii]))),
    silent = TRUE
  )

  # Excel file needs to be manually loaded loaded when VARCHAR2 exceeds maximum length
  try(
    write.xlsx(x = sf::st_drop_geometry(station_table),
             file = here::here("analysis", "akgfmaps4", paste0(gpkg_layers$name[ii], ".xlsx")),
             row.names = FALSE),
      silent = TRUE
      )

  failed_load <- try(
    RODBC::sqlSave(channel = channel,
                   dat = station_table,
                   # dat = station_table,
                   tablename = paste0(user, ".", toupper(gpkg_layers$name[ii])),
                   append = TRUE, # need append = TRUE because of access permissions
                   rownames = FALSE,
                   colnames = FALSE,
                   verbose = FALSE,
                   safer = TRUE,
                   addPK = TRUE,
                   fast = TRUE,
                   test = FALSE,
                   nastring = NULL),
    silent = TRUE
  )

  if(is(failed_load, "try-error")) {
    try(
      RODBC::sqlClear(channel = channel,
                     sqtable = paste0(user, ".", toupper(gpkg_layers$name[ii]))),
      silent = TRUE
    )

  }

}


test <- RODBC::sqlQuery(channel = channel,
                        query = "select * from rohans.survey_strata")

test$WKT_NAD83

st_as_sfc(test$WKT_NAD83[[1]], crs = 3338)

sf::st_as_


sf::st_as_sf(test[[32,]], wkt = "WKT_NAD83", crs = "NAD83")
