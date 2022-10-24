# akgfmaps
An R package for retrieving shapefiles used by NOAA/NMFS/AFSC and producing catch-per-unit-effort maps from bottom-trawl survey catch data from Alaska. Current support for the eastern Bering Sea continental shelf, northern Bering Sea, Chukchi Sea, Gulf of Alaska, and Aleutian Islands.

Build requires that dependencies are compatible with GEOS > 3.0.0, GDAL >3.0.0, and PROJ > 6.0.0.

# Installation

akgfmaps can be installed using the following code:

```{r}
devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
```

# Vignettes

Vignettes can be accessed using:

```{r}
browseVignettes('akgfmaps')
```

You should see a message indicating which versions of GEOS, GDAL, and PROJ are installed. If you have the necessary dependencies installed, versions should be: GEOS > 3.0.0, GDAL >3.0.0, and PROJ > 6.0.0. If version requirements are not met you will need to update sp, sf, gstat, rgdal, raster, and stars.

# Troubleshooting Installation: Non-zero exit status

If installation errors occur, try installing dependencies separately before installing the akgfmaps package. 

Installation errors can occur when packages were built using different versions of R, which may result in non-zero exit status errors that will not affect functionality. Errors can sometimes be suppressed using

```{r}
devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE, R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
```

## Legal disclaimer

This repository is a software product and is not official communication of the National Oceanic and Atmospheric Administration (NOAA), or the United States Department of Commerce (DOC). All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the DOC or DOC bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the DOC. The DOC seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by the DOC or the United States Government.
