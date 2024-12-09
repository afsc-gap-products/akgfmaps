#' Get crab stratum layers
#'
#' Load crab stratum polygons for EBS/NBS red king crab (Bristol Bay, Pribilof Islands, Norton Sound), EBS blue king crab (Pribilof Islands, St. Matthew Island), EBS snow crab, and EBS Tanner crab (West of 166W, East of 166W). Species and areas are described below.
#'
#' @param select.stock Character vector indicating the name of the stock(s) to return. Options are Bristol Bay RKC ('bbrkc'), Pribilof Islands RKC ('pirkc'), Pribilof Islands BKC ('pibkc'), St. Matthew's BKC ('smbkc'), Norton Sound RKC ('nsrkc'), eastern Bering Sea snow crab ('ebssc'), and eastern Bering Sea Tanner crab ('ebstc').
#' @param select.region Select region
#' @param set.crs Which coordinate reference system should be used? If 'auto', Alaska Albers Equal Area coordinate reference system (EPSG:3338) is automatically assigned.
#' @export
#' @examples \dontrun{
#' library(akgfmaps)
#'
#' # Get all stock stratum boundaries by region
#' ebs_stocks <- akgfmaps::get_crab_strata(select.region = "ebs", set.crs = "EPSG:3338")
#'
#' ggplot() +
#'   geom_sf(data = ebs_stocks,
#'           mapping = aes(fill = STRATUM)) +
#'   facet_wrap(~STOCK)
#'
#' # Get strata for a specific stock, such as Pribilof Islands red king crab ('pirkc')
#' pirkc <- akgfmaps::get_crab_strata(select.stock = "pirkc", set.crs = "EPSG:3338")
#'
#' ggplot() +
#'   geom_sf(data = pirkc,
#'           mapping = aes(fill = STRATUM))}

get_crab_strata <- function(select.stock = NULL,
                            select.region = "ebs",
                            set.crs) {

  select_region <- tolower(select.region)

  stock_null <- is.null(select.stock)

  if(set.crs == "auto") {
    set.crs = "EPSG:3338"
  }

  # Automatically select stocks when region is provided but select.stock is NULL
  if(select.region %in% c("bs.south", "sebs") & stock_null)  {

    select_stock <- c("bbrkc", "pirkc", "pibkc", "smbkc", "ebssc", "ebstc")

  }

  if(select.region == "nbs" & stock_null) {

    select_stock <- "nsrkc"

  }

  if(select.region %in% c("ebs", "bs.all") & stock_null) {

    select_stock <- c("bbrkc", "pirkc", "nsrkc", "pibkc", "smbkc", "ebssc", "ebstc")

  }


  if(!stock_null) {

    valid_stocks <- c("bbrkc", "pirkc", "pibkc", "smbkc", "nsrkc", "ebssc", "ebstc")

    select_stock <- tolower(select.stock)

    stopifnot("get_crab_strata: Unit must be one of 'bbrkc', 'pirkc', 'pibkc', 'nsrkc', 'ebssc', 'ebstc', 'smbkc'" = all(select_stock %in% valid_stocks))

  }

  crab_strata <-
    sf::st_read(
      dsn = system.file("extdata", "all_crab_from_akgfmaps_grid.gpkg",
                        package = "akgfmaps")
    ) |>
    sf::st_transform(crs = set.crs) |>
    fix_geometry()

  for(ii in 1:length(select_stock)) {

    stock_name <- switch(select_stock[ii],
                         "bbrkc" = "Bristol Bay RKC",
                         "pirkc" = "Pribilof Islands RKC",
                         "pibkc" = "Pribilof Islands BKC",
                         "nsrkc" = "Norton Sound RKC",
                         "ebssc" = "snow crab",
                         "ebstc" = c("Tanner E", "Tanner W"),
                         "smbkc" = "St. Matthew BKC")

    sel_stock <- crab_strata[crab_strata$STOCK %in% stock_name, ]

    if(ii == 1) {
      output <- sel_stock
    } else {
      output <- rbind(output, sel_stock)
    }

  }

  return(output)

}
