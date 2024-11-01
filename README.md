# akgfmaps
An R package for retrieving shapefiles (vector geometries) used to produce maps for bottom trawl survey regions, management areas, and statistical areas in Alaska. The package includes groundfish and crab bottom trawl survey layers for the eastern Bering Sea shelf, eastern Bering Sea slope, northern Bering Sea, Chukchi Sea, Gulf of Alaska, and Aleutian Islands. The package also include layers for National Marine Fisheries Service (NMFS) statistical areas, Alaska Department of Fish and Game (ADFG) management and statistical areas, Alaska Ecosystem Status Report (ESR) regions, Bering Sea Integrated Ecosystem Research Program (BSIERP) regions, and legacy International North Pacific Fisheries Commission (INPFC) groundfish areas. 

See [NEWS](./NEWS) for information about package updates.

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

# Download shapefiles

Survey shapefiles and associated metadata are available [here](./assets/region_shapefiles). Please note that survey shapefiles change over time as information improves.

# Layers in the package

The table below lists vector geometry layers (POLYGONS) that are included in the package by region 

| Region  | Layer | Function | Provided by | Source | 
|---------|-------|----------|-------------|--------|
| All | Alaska Coastline (1:63360) - Full | get_base_layers() | Alaska DNR | [Link](https://data-soa-dnr.opendata.arcgis.com/datasets/SOA-DNR::alaska-coastline/explore?layer=4&location=60.861513%2C30.585938%2C5.84) |
| All | Alaska Coastline (1:63360) - Simplified | get_base_layers() | Alaska DNR | [Link](https://data-soa-dnr.opendata.arcgis.com/datasets/SOA-DNR::alaska-coastline/explore?layer=4&location=60.861513%2C30.585938%2C5.84) |
| All | Russia Coastline | get_base_layers() | A. Grieg (AFSC Ret.) | |
| All | Canada Coastline | get_base_layers() | A. Grieg (AFSC Ret.) | |
| All | ADFG Statistical/Management Areas | get_adfg_areas() | [M. Callahan (PSMFC/AKFIN)](https://github.com/MattCallahan-NOAA) | [Link] |
| All | NMFS Statistical Areas | get_nmfs_areas() | [A. Jahn (AKRO)](https://github.com/abby-jahn) | |
| All | Ecosystem Status Report Areas/Subareas | get_esr_regions() | [M. Callahan (PSMFC/AKFIN)](https://github.com/MattCallahan-NOAA) | |
| All | North Pacific bathymetry (0-1000 m) | get_base_layers() | A. Grieg (AFSC Ret.) | |
| AI | Bottom trawl survey area | get_base_layers() | [N. Laman (AFSC)](https://github.com/Ned-Laman-NOAA) | |
| AI | Bottom trawl survey strata | get_base_layers() | [N. Laman (AFSC)](https://github.com/Ned-Laman-NOAA) | |
| AI | Bottom trawl survey grid | get_base_layers() | [N. Laman (AFSC)](https://github.com/Ned-Laman-NOAA) | |
| BSS/NBS/SEBS | BSIERP Regions | get_bsierp_regions() | [M. Callahan (PSMFC/AKFIN)](https://github.com/MattCallahan-NOAA) | |
| BSS | Bottom trawl survey area | get_base_layers() | A. Grieg (AFSC Ret.) | |
| BSS | Bottom trawl survey strata | get_base_layers() | A. Grieg (AFSC Ret.) | |
| CS  | Bottom trawl survey area | get_base_layers() | B. Lauth (AFSC Ret.) | |
| CS  | Bottom trawl survey grid | get_base_layers() | B. Lauth (AFSC Ret.) | |
| GOA | Bottom trawl survey area | get_base_layers() | [N. Laman (AFSC)](https://github.com/Ned-Laman-NOAA) | |
| GOA | Bottom trawl survey strata | get_base_layers() | [N. Laman (AFSC)](https://github.com/Ned-Laman-NOAA) | |
| GOA | Bottom trawl survey grid | get_base_layers() | [N. Laman (AFSC)](https://github.com/Ned-Laman-NOAA) | |
| NBS | Bottom trawl survey area | get_base_layers() | [J. Conner (AFSC)](https://github.com/Jason-Conner-NOAA) | |
| NBS | Bottom trawl survey strata | get_base_layers() | [J. Conner (AFSC)](https://github.com/Jason-Conner-NOAA) | |
| NBS | Bottom trawl survey grid | get_base_layers() | J. Benson (AFSC Ret.) | |
| NBS | Norton Sound RKC strata | get_crab_strata() | [ [E. Ryznar (AFSC)](https://github.com/eryznar) | |
| SEBS | Bottom trawl survey area | get_base_layers() | [J. Conner (AFSC)](https://github.com/Jason-Conner-NOAA) | |
| SEBS | Bottom trawl survey strata | get_base_layers() | [J. Conner (AFSC)](https://github.com/Jason-Conner-NOAA) | |
| SEBS | Bottom trawl survey grid | get_base_layers() | J. Benson (AFSC Ret.) | |
| SEBS | Bristol Bay RKC strata | get_crab_strata() | [E. Ryznar (AFSC)](https://github.com/eryznar) | |
| SEBS | EBS Snow crab strata | get_crab_strata() | [E. Ryznar (AFSC)](https://github.com/eryznar) | |
| SEBS | EBS Tanner crab strata | get_crab_strata() | [E. Ryznar (AFSC)](https://github.com/eryznar) | |
| SEBS | Pribilof Island BKC strata | get_crab_strata() | [E. Ryznar (AFSC)](https://github.com/eryznar) | |
| SEBS | Pribilof Island RKC strata | get_crab_strata() | [E. Ryznar (AFSC)](https://github.com/eryznar) | |
| SEBS | St. Matthew's BKC strata | get_crab_strata() | [E. Ryznar (AFSC)](https://github.com/eryznar) | |

## Legal disclaimer

This repository is a software product and is not official communication of the National Oceanic and Atmospheric Administration (NOAA), or the United States Department of Commerce (DOC). All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the DOC or DOC bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation, or favoring by the DOC. The DOC seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by the DOC or the United States Government.
