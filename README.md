# akgfmaps
An R package for retrieving shapefiles used by NOAA/NMFS/AFSC and producing publication-ready catch-per-unit-effort maps from bottom-trawl survey catch data from Alaska. Current support for the eastern Bering Sea continental shelf, northern Bering Sea, and Chukchi Sea. The package includes shapefile data with North American Datum 1983 (NAD 83) projection. Coordinate reference system (CRS) and CRS transformations use the PROJ6 library. CRS support for both PROJ4 and PROJ6 ('WKT2_2019') strings but some PROJ4 transformations are no longer supported due to deprecation in PROJ6 (e.g. projections that were linked through WGS84 in PROJ4) .

The most recent version of this package was built in R 4.0.2. Build requires that dependencies are compatible with GEOS > 3.0.0, GDAL >3.0.0, and PROJ > 6.0.0.

# Installation

akgfmaps can be installed using the following code:

```{r}
devtools::install_github("sean-rohan-noaa/akgfmaps", build_vignettes = TRUE)
```

The automatic installer may terminate with errors . If errors occur, try to install dependencies individually, then try to install the akgfmaps package. 

Installation errors can occur when packages were built using different versions of R, which may result in non-zero exit status errors that will no affect functionality. Errors can sometimes be suppressed using:

```{r}
devtools::install_github("sean-rohan-noaa/akgfmaps", build_vignettes = TRUE, R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
```

# Vignettes

Vignettes are accessible using:

```{r}
browseVignettes('akgfmaps')
```

# Troubleshooting

akgfmaps provides limited backwards compatibility when using older (pre April-June 2020) versions of packages sp, sf, gstat, rgdal, raster, and stars. To determine if you have the proper package versions installed, run the following in a new session:

```{r}
library(sf)
```

You should see a message indicating which versions of GEOS, GDAL, and PROJ are installed. If you have the necessary dependencies installed, versions should be: GEOS > 3.0.0, GDAL >3.0.0, and PROJ > 6.0.0. If version requirements are not met you will need to update sp, sf, gstat, rgdal, raster, and stars.