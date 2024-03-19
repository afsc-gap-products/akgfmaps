# akgfmaps
An R package for retrieving shapefiles used by NOAA/NMFS/AFSC to produce maps for bottom-trawl survey regions in Alaska. Current support for the eastern Bering Sea shelf, eastern Bering Sea slope, northern Bering Sea, Chukchi Sea, Gulf of Alaska, and Aleutian Islands.

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

# Access shapefiles without R

Survey shapefiles and associated metadata for the eastern Bering Sea shelf, northern Bering Sea, eastern Bering Sea slope, Gulf of Alaska, Aleutian Islands, and Chukchi Sea are available [here](./assets/regional_shapefiles). Please note that survey shapefiles change over time as bathymetry information improves and when survey designs change.

## Legal disclaimer

This repository is a software product and is not official communication of the National Oceanic and Atmospheric Administration (NOAA), or the United States Department of Commerce (DOC). All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the DOC or DOC bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the DOC. The DOC seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by the DOC or the United States Government.
