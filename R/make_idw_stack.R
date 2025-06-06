#' Make a stack of IDW maps by grouping variable
#'
#' This function can be used to make inverse-distance-weighted plots for the eastern Bering Sea and northern Bering Sea
#'
#' @param x Data frame which contains at minimum: CPUE, LATITUDE, and LONGITUDE. Can be passed as vectors instead (see below). Default value: \code{NA}
#' @param COMMON_NAME Common name
#' @param LATITUDE Latitude (degrees north)
#' @param LONGITUDE Longitude (degrees east; Western hemisphere is negative)
#' @param CPUE_KGHA Catch per unit effort in kilograms per hectare
#' @param region Character vector indicating which plotting region to use. Options: bs.south, bs.north, bs.all
#' @param grid.cell Numeric vector of length two specifying dimensions of grid cells for extrpolation grid, in units for the output CRS. Default = c(5000,5000) correponds with 5x5 km for EPSG:3338
#' @param grouping.vars Character vector indicating of columns in the input data frame to use for grouping input variable layers.
#' @param extrap.box Optional. Vector specifying the dimensions of the extrapolation grid. Elements of the vector should be named to specify the minimum and maximum x and y values c(xmin, xmax, ymin, ymax). If not provided, the extrapolation area will be set to the extent of the survey.area bounding box with the output CRS.
#' @param extrapolation.grid.type Type of object to use for the extrapolation grid, default = "stars". "stars" = returns a 'stars' object; "sf" = sf object with layer masked to survey area extent and converted to collection of sf POLYGON and MULTIPOLYGON geometries; "sf.simple" = same as "sf", but with polygons vertices smoothed using rmapshaper::ms_simplify
#' @param set.breaks Either a numeric vector of breaks to use for plotting or a character vector indicating which classIntervals() algorithm to use for break selection. See Description for information about break selection.  Users are strongly encouraged to specify their own numeric vector of breaks based on the properties of their data.
#' @param in.crs Character vector containing the coordinate reference system for projecting the extrapolation grid.
#' @param out.crs Character vector containing the coordinate reference system for projecting the extrapolation grid. The default is Alaska Albers Equal Area (EPSG:3338).
#' @param log.transform Character vector indicating whether CPUE values should be log-transformed for IDW. Default = FALSE.
#' @param idw.nmax Maximum number of adjacent stations to use for interpolation. Default = 8
#' @param use.survey.bathymetry Logical indicating if historical survey bathymetry should be used instead of continuous regional bathymetry. Default = TRUE
#' @details
#' This function include an argument for algorithmic break selection using the `set.breaks` argument. However, algorithmic break selection is provided for convenience purposes and **users are strongly encouraged to select their own breaks based on the properties of their data**. No single algorithm in the package can be expected to perform well in all cases. See ?classInt::classIntervals for a algorithmic break selection method options.
#' @return Returns a list containing:
#' (1) map_layers: Layer of shapefiles returned by akgfmaps::get_base_layers()
#' (2) extrapolation.stack: a stack of extrapolated spatial objects with estimated values on a discrete scale as a 'stars' (when extrapolation.grid.type is "stars") or 'sf' (when extrapolation.grid.type is "sf" or "sf.simple") object;
#' (3) continuous.grid: extrapolation grid with estimates on a continuous scale;
#' (4) region: the region;
#' (5) crs: coordinate reference system as a PROJ6 (WKT2:2019) string.
#' @author Sean Rohan \email{sean.rohan@@noaa.gov}
#' @import gstat
#' @importFrom classInt classIntervals
#' @export

make_idw_stack <- function(x = NA,
                           COMMON_NAME = NA,
                           LATITUDE = NA,
                           LONGITUDE = NA,
                           CPUE_KGHA = NA,
                           region = "bs.south",
                           extrap.box = NULL,
                           extrapolation.grid.type = "stars",
                           set.breaks = "jenks",
                           grouping.vars,
                           grid.cell = c(5000,5000),
                           in.crs = "+proj=longlat",
                           out.crs = "EPSG:3338",
                           log.transform = FALSE,
                           idw.nmax = 4,
                           use.survey.bathymetry = TRUE) {

  var1.var <- var1.pred <- id_temp <- NULL

  .check_region(select.region = region, type = "survey")

  stopifnot("make_idw_map: extra.grid.type must be 'stars', 'sf', or 'sf.simple'" = extrapolation.grid.type %in% c("stars", "sf", "sf.simple"))

  # Convert vectors to data frame if x is not a data.frame or tbl-----------------------------------
  if(!is.data.frame(x)) {

    stopifnot("make_idw_map: LATITUDE must be a numeric vector." = is.numeric(LATITUDE))
    stopifnot("make_idw_map: LONGITUDE must be a numeric vector." = is.numeric(LONGITUDE))
    stopifnot("make_idw_map: CPUE_KGHA must be a numeric vector." = is.numeric(CPUE_KGHA))

    x <- data.frame(COMMON_NAME = COMMON_NAME,
                    LATITUDE = LATITUDE,
                    LONGITUDE = LONGITUDE,
                    CPUE_KGHA = CPUE_KGHA)
  }

  x <- as.data.frame(x)

  # Load map layers---------------------------------------------------------------------------------
  map_layers <- get_base_layers(select.region = region, set.crs = out.crs)

  # Set up mapping region---------------------------------------------------------------------------
  if(is.null(extrap.box)) {
    extrap.box <- sf::st_bbox(map_layers$survey.area)
  }

  # Assign CRS to handle automatic selection--------------------------------------------------------
  if(out.crs == "auto") {
    out.crs <- map_layers$crs
  }

  # Use survey bathymetry---------------------------------------------------------------------------
  if(use.survey.bathymetry) {
    map_layers$bathymetry <- get_survey_bathymetry(select.region = region, set.crs = out.crs)
  }

  # Generate extrapolation grid---------------------------------------------------------------------
  extrap_raster <- terra::rast(xmin = extrap.box['xmin'],
                                     xmax = extrap.box['xmax'],
                                     ymin = extrap.box['ymin'],
                                     ymax = extrap.box['ymax'],
                                     ncol = (extrap.box['xmax']-extrap.box['xmin'])/grid.cell[1],
                                     nrow = (extrap.box['ymax']-extrap.box['ymin'])/grid.cell[2],
                                     crs = out.crs)

  loc_df <- suppressWarnings(terra::crds(extrap_raster, df = TRUE, na.rm = FALSE)) |>
    sf::st_as_sf(coords = c("x", "y"),
                 crs = out.crs)

  # Assign CRS to input data------------------------------------------------------------------------
  unique_groups <- x[grouping.vars] |>
    unique()

  x <- sf::st_as_sf(x,
                    coords = c(x = "LONGITUDE", y = "LATITUDE"),
                    crs = sf::st_crs(in.crs)) |>
    sf::st_transform(crs = map_layers$crs)

  for(ii in 1:nrow(unique_groups)) {

    if(length(grouping.vars) > 1) {

      x_sub <- merge(x, unique_groups[ii, ], by = grouping.vars)

    } else{

      x_sub <- x[which(x[[grouping.vars]] == unique_groups[ii, ]), ]

    }

    # Inverse distance weighting----------------------------------------------------------------------
    idw_fit <- gstat::gstat(formula = CPUE_KGHA~1, locations = x_sub, nmax = idw.nmax)

    # Predict station points--------------------------------------------------------------------------
    stn.predict <- predict(idw_fit, x_sub)

    # Predict, rasterize, mask------------------------------------------------------------------------

    extrap.grid <- predict(idw_fit, loc_df) |>
      sf::st_transform(crs = sf::st_crs(x)) |>
      stars::st_rasterize() |>
      sf::st_join(map_layers$survey.area, join = st_intersects)

    # Format breaks for plotting----------------------------------------------------------------------
    # Automatic break selection based on character vector.
    alt.round <- 0 # Set alternative rounding factor to zero based on user-specified breaks

    if(is.character(set.breaks[1])) {
      set.breaks <- tolower(set.breaks)

      # Set breaks ----
      break.vals <- classIntervals(x$CPUE_KGHA, n = 5, style = set.breaks)$brks

      # Setup rounding for small CPUE ----
      alt.round <- floor(-1*(min((log10(break.vals)-2)[abs(break.vals) > 0])))

      set.breaks <- c(-1, round(break.vals, alt.round))
    }

    # Ensure breaks go to zero------------------------------------------------------------------------
    if(min(set.breaks) > 0) {
      set.breaks <- c(0, set.breaks)
    }

    if(min(set.breaks) == 0) {
      set.breaks <- c(-1, set.breaks)
    }

    # Ensure breaks span the full range---------------------------------------------------------------
    if(max(set.breaks) < max(x$CPUE_KGHA)){
      set.breaks[length(set.breaks)] <- max(x$CPUE_KGHA) + 1
    }


    # Trim breaks to significant digits to account for differences in range among species-------------
    dig.lab <- 7
    set.levels <- cut(x$CPUE_KGHA, set.breaks, right = TRUE, dig.lab = dig.lab)

    if(alt.round > 0) {
      while(dig.lab > alt.round) { # Rounding for small CPUE
        dig.lab <- dig.lab - 1
        set.levels <- cut(x$CPUE_KGHA, set.breaks, right = TRUE, dig.lab = dig.lab)
      }
    } else { # Rounding for large CPUE
      while(length(grep("\\.", set.levels)) > 0) {
        dig.lab <- dig.lab - 1
        set.levels <- cut(x$CPUE_KGHA, set.breaks, right = TRUE, dig.lab = dig.lab)
      }
    }

    continuous.grid <- extrap.grid

    continuous.grid <- continuous.grid["var1.pred"]
    names(continuous.grid) <- paste(unique_groups[ii,], collapse = "_")

    # Cut extrapolation grid to support discrete scale------------------------------------------------
    extrap.grid$var1.pred <- cut(extrap.grid$var1.pred, set.breaks, right = TRUE, dig.lab = dig.lab)

    # Which breaks need commas?-----------------------------------------------------------------------
    sig.dig <- round(set.breaks[which(nchar(round(set.breaks)) >= 4)])

    # Drop brackets, add commas, create 'No catch' level to legend labels-----------------------------
    make_level_labels <- function(vec) {
      vec <- as.character(vec)
      vec[grep("-1", vec)] <- "No catch"
      vec <- sub("\\(", "\\>", vec)
      vec <- sub("\\,", "-", vec)
      vec <- sub("\\]", "", vec)

      if(length(sig.dig) > 0) {

        sig.dig.format <- trimws(
          format(
            sort(sig.dig,
                 decreasing = TRUE),
            scientific = FALSE,
            nsmall=0,
            big.mark=",")
        )

        sig.dig.desc <- trimws(
          format(
            sort(sig.dig,
                 decreasing = TRUE),
            scientific = FALSE)
        )

        for(j in 1:length(sig.dig)) {
          vec <- sub(pattern = sig.dig.desc[j],
                     replacement = sig.dig.format[j],
                     x = vec)
        }
      }
      return(vec)
    }

    # Assign level names to breaks for plotting-----------------------------------------------------
    extrap.grid$var1.pred <- factor(make_level_labels(extrap.grid$var1.pred),
                                    levels = make_level_labels(levels(set.levels)))

    if(ii == 1) {

      continuous.stack <- continuous.grid
    } else {
      continuous.stack <- c(continuous.stack, continuous.grid)
    }

    if(extrapolation.grid.type %in% c("sf", "sf.simple")) {

      extrap.grid <- extrap.grid |>
        sf::st_as_sf()

      extrap.grid <- subset(extrap.grid, select = -var1.var)

      extrap.grid <- aggregate(extrap.grid,
                               by = list(id_temp = extrap.grid$var1.pred),
                               FUN = function(x) x[1],
                               do_union = TRUE)

      extrap.grid <- subset(extrap.grid, select = -id_temp) |>
        sf::st_intersection(map_layers$survey.area)

      extrap.grid <- extrap.grid[c("var1.pred", "SURVEY_NAME", "SURVEY_DEFINITION_ID",  "geometry")]

      if(length(grouping.vars) > 1) {

        extrap.grid <- cbind(extrap.grid, unique_groups[ii, ])

      } else{

        extrap.grid[grouping.vars] <- unique_groups[ii, ]

      }

      # Simplify geometry using ms_simplify function from the rmapshaper package
      if(extrapolation.grid.type == "sf.simple") {
        extrap.grid <- extrap.grid |>
          rmapshaper::ms_simplify(keep_shapes = TRUE,
                                  keep = 0.04)
      }

      if(ii == 1) {
        extrap.stack <- extrap.grid
      } else {
        extrap.stack <- rbind(extrap.stack, extrap.grid)
      }

    } else {

      extrap.grid <- extrap.grid["var1.pred"]
      names(extrap.grid) <- paste(unique_groups[ii,], collapse = "_")

      if(ii == 1) {
        extrap.stack <- extrap.grid
      } else {
        extrap.stack <- c(extrap.stack, extrap.grid)
      }
    }

  }

  return(list(map_layers = map_layers,
              extrapolation.stack = extrap.stack,
              continuous.stack = continuous.stack,
              region = region,
              crs = out.crs))

}
