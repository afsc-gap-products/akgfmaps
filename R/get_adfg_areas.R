#' Alaska Department of Fish and Game (ADFG) management areas
#'
#' Retrieve ADFG management area POLYGON grid.
#'
#' @param set.crs Which coordinate reference system should be used? If 'auto', Alaska Albers Equal Area coordinate reference system (EPSG:3338) is automatically assigned.
#' @param subset.fields Logical. Should all of the columns from the original file be returned (including blank and duplicate fields).
#' @return ADFG management area sf POLYGON.
#' @export

get_adfg_areas <- function(set.crs = "auto", subset.fields = TRUE) {

  if(set.crs == "auto") {
    set.crs = "EPSG:3338"
  }

  layer <- sf::st_read(here::here("inst/extdata/Alaska_Marine_Management_Areas.gdb"),
                       layer = "Alaska_Marine_Areas_AK_prj",
                       quiet = TRUE) |>
    dplyr::filter(Area_Type == "ADFG Stat Area") |>
    unique()

  if(subset.fields) {

    layer <- layer |>
      dplyr::select(AREA_TYPE = Area_Type,
                    STAT_AREA,
                    FISHERY_GR,
                    REGION_CODE = REGION_COD,
                    REGISTRATI,
                    REGISTRA_1,
                    # REGISTRA_2,
                    REGISTRA_3,
                    DISTRICT_N,
                    DISTRICT_C,
                    DISTRICT_I,
                    SUBDISTRIC,
                    SUBDISTR_1,
                    # SUBDISTR_2,
                    SECTION_NA,
                    SECTION_CO,
                    # SECTION_ID,
                    SECTOR_NAM,
                    SECTOR_COD,
                    INSIDE_OUT,
                    WATERS_COD,
                    FMP_AREA_C,
                    NMFS_REPOR,
                    NMFS_REP_1,
                    # NMFS_REP_2,
                    IFQ_IPHC_A,
                    IFQ_SABLEF,
                    COAR_AREA_,
                    AREA_M2 = Shape_Area
      )

  }

  sf::st_geometry(layer)<- "geometry"

  layer <- layer |>
    sf::st_transform(crs = set.crs) |>
    sf::st_make_valid() |>
    fix_geometry()

  return(layer)

}
