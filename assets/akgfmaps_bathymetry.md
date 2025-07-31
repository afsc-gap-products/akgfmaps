# Bathymetry in akgfmaps

## Raster

Bathymetry rasters in akgfmaps are compiled from digital terrain models (DTMs) covering the eastern Bering Sea shelf, eastern Bering Sea ([GEBCO Bathymetric Compilation Group 2024, 2024](https://doi.org/10.5285/1c44ce99-0a0d-5f4f-e063-7086abc0ea0f); [Zimmermann and Prescott, 2018](https://doi.org/10.3390/geosciences8050184)), Norton Sound ([Prescott and Zimmermann, 2015](https://doi.org/10.7289/V5V69GJ9)), Aleutian Islands ([Zimmermann et al., 2013](https://repository.library.noaa.gov/view/noaa/4426); [Zimmermann and Prescott, 2021](https://doi.org/10.1111/fog.12519)), and Gulf of Alaska ([Zimmermann et al., 2019](https://doi.org/10.3390/geosciences9100409)). Spatial coverage of individual DTMs overlap and the depth value for each location is assigned using the DTM that corroborates most closely with nearby bottom trawl survey depth observations.

## Contours

[Bathymetry contours](./inst/extdata/bathymetry.gpkg) in akgfmaps were derived from 1 x 1 km rasters by using the Contour List tool in ArcGIS Pro (ESRI, 2024) to obtain depth contour lines for 50, 100, 200, 300, 400, 500, 600, 800, 1000, 1200, 1500, 2000, and 3000 m isobaths. Contour lines were simplified using the Douglas-Peuker algorithm with a 0.1 retention proportion using ms_simplify() in the rmapshaper package. Contours with length \< 50 km were removed and obvious errors were manually removed.

## Disclaimer

Bathymetric data in akgfmaps are not to be used for navigation. For navigation products, please refer to U.S. Nautical Charts available from the NOAA Office of Coast Survey [(https://www.nauticalcharts.noaa.gov/)](https://www.nauticalcharts.noaa.gov/).

## References

ESRI, 2024. *ArcGIS Pro 3.4.0.*

GEBCO Bathymetric Compilation Group 2024, 2024. *The GEBCO_2024 Grid - a continuous terrain model of the global oceans and land.* <https://doi.org/10.5285/1c44ce99-0a0d-5f4f-e063-7086abc0ea0f>

Prescott, M.M., Zimmermann, M., 2015. *Smooth sheet bathymetry of Norton Sound.* U.S. Dep. Commer., NOAA Tech. Memo. NMFS-AFSC-298. 23. <https://doi.org/10.7289/V5V69GJ9>

Zimmermann, M., Prescott, M.M., 2021. *Passes of the Aleutian Islands: First detailed description.* Fish. Oceanogr. 30, 280–299. <https://doi.org/10.1111/fog.12519>

Zimmermann, M., Prescott, M.M., 2018. *Bathymetry and canyons of the eastern Bering Sea slope.* Geosci. 8. <https://doi.org/10.3390/geosciences8050184>

Zimmermann, M., Prescott, M.M., Haeussler, P.J., 2019. *Bathymetry and geomorphology of Shelikof Strait and the western Gulf of Alaska.* Geosci. 9, 1–31. <https://doi.org/10.3390/geosciences9100409>

Zimmermann, M., Prescott, M.M., Rooper, C.N., 2013. *Smooth Sheet Bathymetry of the Aleutian Islands.* United States Dep. Commer. NOAA Tech. Memo. NMFS-AFSC-, 43. <https://repository.library.noaa.gov/view/noaa/4426>
