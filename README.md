# akgfmaps

The akgfmaps package is used to vector geometries that are commonly needed for mapping and spatial analysis in Alaska marine management areas, marine statistical areas, and fishery-independent survey regions in Alaska. 

The package includes:

- Groundfish and crab bottom trawl survey areas in the eastern Bering Sea shelf (EBS), eastern Bering Sea slope (BSS), northern Bering Sea (NBS), eastern Chukchi Sea (ECS), Gulf of Alaska (GOA), and Aleutian Islands (AI).
- Longline survey areas in the BSS, GOA, and AI.
- National Marine Fisheries Service (NMFS) statistical area layer.
- Alaska Department of Fish and Game (ADFG) management and statistical area layer.
- Alaska Ecosystem Status Report (ESR) area and subarea layers.
- Bering Sea Integrated Ecosystem Research Program (BSIERP) area layer.
- Historical International North Pacific Fisheries Commission (INPFC) groundfish area layer for the Aleutian Islands and Gulf of Alaska. 
- Functions for common spatial tasks performed by Alaska fisheries researchers (e.g. generating two-dimensional grids in multiple coordinate reference systems, producing qualitative distribution maps in the EBS and NBS).

Please [submit an issue](https://github.com/afsc-gap-products/akgfmaps/issues) if you encounter problems or errors with the package.

See [NEWS](./NEWS) for information about package updates.

# Installation

The package can installed for R versions >= 4.0 using the using the following code:

```{r}
remotes::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
```

Spatial features are saved locally during package installation. Therefore, the package must be re-installed to access newer versions of spatial features when they are updated.

# Vignettes

Vignettes can be accessed using:

```{r}
browseVignettes('akgfmaps')
```

# GeoPackage downloads

[GeoPackages](https://www.geopackage.org/) containing [current and historical bottom trawl survey features](./inst/extdata/afsc_bottom_trawl_surveys.gpkg), [current EBS/NBS crab stratum polygons](./inst/all_crab_from_akgfmaps_grid.gpkg), and [current coastline polygons](./inst/extdata/land_layers.gpkg) are available for download.

Zip files containing GeoPackages and version metadata for AFSC bottom trawl survey regions are also available [here](https://github.com/afsc-gap-products/akgfmaps/tree/main/assets/bts_geopackages).

# Why GeoPackages?

akgfmaps primarily uses [GeoPackages (.gpkg)](https://www.geopackage.org/) instead of [shapefiles (.shp)](https://en.wikipedia.org/wiki/Shapefile) because they provide a more efficient and reliable format for storing geospatial data. Unlike shapefiles, which require multiple files and have strict limitations on field names, character encoding, and file size, GeoPackages can store vector and raster data in a single SQLite database file. GeoPackages support larger datasets, richer attribute types, and full Unicode, which makes them suitable for long-term data management and ensures consistency with other AFSC data products.


# Vector geometries in the package

The table below lists the vector geometry layers (polygons) in the package by region (AI = Aleutian Islands, BSS = eastern Bering Sea Slope, ECS = eastern Chukchi Sea, GOA = Gulf of Alaska, NBS = Northern Bering Sea, SEBS = southeastern Bering Sea continental shelf). 

| Region  | Layer | Function | Contributor/Creator | Source | 
|---------|-------|----------|-------------|--------|
| All | Alaska Coastline (1:63360) - Full | get_base_layers() | Alaska DNR | [Link](https://data-soa-dnr.opendata.arcgis.com/datasets/SOA-DNR::alaska-coastline/explore?layer=4&location=60.861513%2C30.585938%2C5.84) |
| All | Alaska Coastline (1:63360) - Simplified | get_base_layers() | Alaska DNR | [Link](https://data-soa-dnr.opendata.arcgis.com/datasets/SOA-DNR::alaska-coastline/explore?layer=4&location=60.861513%2C30.585938%2C5.84) |
| All | Russia Coastline | get_base_layers() | A. Grieg (AFSC Ret.) | |
| All | Canada Coastline | get_base_layers() | A. Grieg (AFSC Ret.) | |
| All | ADFG Statistical/Management Areas | get_adfg_areas() | [M. Callahan (PSMFC/AKFIN)](https://github.com/MattCallahan-NOAA) | [Link](http://www.adfg.alaska.gov/index.cfm?adfg=fishingCommercialByFishery.statmaps) |
| All | NMFS Statistical Areas | get_nmfs_areas() | [A. Jahn (AKRO)](https://github.com/abby-jahn) | [Link](https://www.ecfr.gov/cgi-bin/text-idx?mc=true&node=pt50.13.679&rgn=div5#ap50.13.679.0000_0nbspnbspnbsp.1) |
| All | Ecosystem Status Report Areas/Subareas | get_esr_regions() | [M. Callahan (PSMFC/AKFIN)](https://github.com/MattCallahan-NOAA) | [Link](https://apps-afsc.fisheries.noaa.gov/refm/reem/ecoweb/index.php) |
| All | North Pacific bathymetry (0-1000 m) | get_base_layers() | A. Grieg (AFSC Ret.) | GEBCO 1998 |
| AI | Bottom trawl survey area | get_base_layers() | [N. Laman (AFSC)](https://github.com/Ned-Laman-NOAA) | |
| AI | Groundfish survey strata | get_base_layers() | [N. Laman (AFSC)](https://github.com/Ned-Laman-NOAA) | |
| AI | Bottom trawl survey grid | get_base_layers() | [N. Laman (AFSC)](https://github.com/Ned-Laman-NOAA) | |
| AI/BSS/GOA | Longline survey stations | get_base_layers() | K. Echave (AFSC) | [Link](https://repository.library.noaa.gov/view/noaa/11869) |
| AI/BSS/GOA | Longline survey strata | get_base_layers() | K. Echave (AFSC) | [Link](https://repository.library.noaa.gov/view/noaa/11869) |
| AI/BSS/GOA | Longline survey area | get_base_layers() | K. Echave (AFSC) | [Link](https://repository.library.noaa.gov/view/noaa/11869) |
| BSS/NBS/SEBS | BSIERP Regions | get_bsierp_regions() | [M. Callahan (PSMFC/AKFIN)](https://github.com/MattCallahan-NOAA) | [Link](https://doi.org/10.5065/D6DF6P6C) |
| BSS | Groundfish survey area | get_base_layers() | A. Grieg (AFSC Ret.) | |
| BSS | Groundfish survey strata | get_base_layers() | A. Grieg (AFSC Ret.) | |
| ECS  | Bottom trawl survey area | get_base_layers() | B. Lauth (AFSC Ret.) | |
| ECS  | Bottom trawl survey grid | get_base_layers() | B. Lauth (AFSC Ret.) | |
| GOA | Bottom trawl survey area | get_base_layers() | [N. Laman (AFSC)](https://github.com/Ned-Laman-NOAA) | |
| GOA | Groundfish survey strata | get_base_layers() | [N. Laman (AFSC)](https://github.com/Ned-Laman-NOAA) | |
| GOA | Bottom trawl survey grid | get_base_layers() | [N. Laman (AFSC)](https://github.com/Ned-Laman-NOAA) | |
| NBS | Bottom trawl survey area | get_base_layers() | [J. Conner (AFSC)](https://github.com/Jason-Conner-NOAA) | [Link](https://github.com/afsc-gap-products/bering-sea-spatial) |
| NBS | Groundfish survey strata | get_base_layers() | [J. Conner (AFSC)](https://github.com/Jason-Conner-NOAA) | [Link](https://github.com/afsc-gap-products/bering-sea-spatial) |
| NBS | Bottom trawl survey grid | get_base_layers() | J. Benson (AFSC Ret.) | |
| NBS | Norton Sound RKC strata | get_crab_strata() | [E. Ryznar (AFSC)](https://github.com/eryznar) | |
| SEBS | Bottom trawl survey area | get_base_layers() | [J. Conner (AFSC)](https://github.com/Jason-Conner-NOAA) | [Link](https://github.com/afsc-gap-products/bering-sea-spatial) |
| SEBS | Groundfish survey strata | get_base_layers() | [J. Conner (AFSC)](https://github.com/Jason-Conner-NOAA) | [Link](https://github.com/afsc-gap-products/bering-sea-spatial) |
| SEBS | Bottom trawl survey grid | get_base_layers() | J. Benson (AFSC Ret.) | |
| SEBS | Bristol Bay RKC strata | get_crab_strata() | [E. Ryznar (AFSC)](https://github.com/eryznar) | |
| SEBS | EBS Snow crab strata | get_crab_strata() | [E. Ryznar (AFSC)](https://github.com/eryznar) | |
| SEBS | EBS Tanner crab strata | get_crab_strata() | [E. Ryznar (AFSC)](https://github.com/eryznar) | |
| SEBS | Pribilof Island BKC strata | get_crab_strata() | [E. Ryznar (AFSC)](https://github.com/eryznar) | |
| SEBS | Pribilof Island RKC strata | get_crab_strata() | [E. Ryznar (AFSC)](https://github.com/eryznar) | |
| SEBS | St. Matthew's BKC strata | get_crab_strata() | [E. Ryznar (AFSC)](https://github.com/eryznar) | |

## Legal disclaimer

This repository is a software product and is not official communication of the National Oceanic and Atmospheric Administration (NOAA), or the United States Department of Commerce (DOC). All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the DOC or DOC bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the DOC. The DOC seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by the DOC or the United States Government.
