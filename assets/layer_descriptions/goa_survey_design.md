# Gulf of Alaska bottom trawl survey design

The AFSC/RACE/GAP Gulf of Alaska bottom trawl survey implemented a new design for the 2025 survey. The akgfmaps package contains vector geometries for three survey design years (1984, 2024, and 2025) that are detailed below. In akgfmaps version > 4.1.0, all of these survey designs can be accessed using `get_base_layers(design.year = {year})`.

### DESIGN_YEAR 1984

Survey design used during Gulf of Alaska bottom trawl surveys prior to 2025. Strata are defined by INPFC areas Shumagin (919), Chirikof (929), Kodiak (939), Yakutat (949), and Southeastern Alaska (959), consistent depth bins (1 – 100 m, 101 – 200 m, 201 – 300 m, 301 – 500 m, 501 – 700 m, 701  – 1000 m) and habitat type (shelf, gully, slope).

### DESIGN_YEAR 2024
 Same survey design as in 1984, but excludes areas outside NMFS areas 610 (Western Regulatory), 620 (Chirikof), 630 (Kodiak), 640 (West Yakutat), and 650 (Southeast Outside). The majority of the excised areas are in NMFS areas 519 (primarily Unimak Pass) and 659 (Southeast Inside). This is the current DESIGN_YEAR value used for the Gulf of Alaska bottom trawl survey time series from 1990 – 2023.

### DESIGN_YEAR 2025
New survey design based on Oyafuso et al. (2022)*. Strata are defined by NMFS areas Western Regulatory Area (610), Chirikof (620), Kodiak (630), West Yakutat (640) and Southeast Outside (650) and depth zones that differ among NMFS areas. This is the current DESIGN_YEAR value used for the Gulf of Alaska bottom trawl survey time series from 2025-present.


### Accessing different design years using get_base_layers

```
library(akgfmaps)

# 1984
goa_1984 <- get_base_layers(select.region = "goa", design.year = 1984, set.crs = 3338)

# 2024
goa_2024 <- get_base_layers(select.region = "goa", design.year = 2024, set.crs = 3338)

# 2025
goa_2025 <- get_base_layers(select.region = "goa", design.year = 2025, set.crs = 3338)
```


## References

Oyafuso, Z.S., L. A.K. Barnett, M.C. Siple, and S. Kotwicki. 2022. A flexible approach to optimizing the Gulf of Alaska groundfish bottom trawl survey design for abundance estimation. U.S. Dep. Commer., NOAA Tech. Memo. NMFS-AFSC-434, 142 p. <https://doi.org/10.25923/g5zd-be29>
