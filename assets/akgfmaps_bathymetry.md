# Bathymetry in akgfmaps

As of August 2025, bathymetry data in akgfmaps cover the eastern Bering Sea shelf, eastern Bering sea slope, northern Bering Sea, Gulf of Alaska, and Aleutian Islands from 0-3000 m. Coverage does not include Chukchi Sea, Beaufort Sea, western Bering Sea, Prince William Sound, interior southeast Alaska, and basins deeper than 3000 m.

## Raster (1 x 1 km)

![](/assets/bathy_images/akgfmaps_raster.png)

The 1 x 1 km bathymetry raster in akgfmaps was derived from a compilation of digital elevation models (DEMs) of the eastern Bering Sea and northern Bering Sea ([Zimmermann and Prescott, 2018](https://doi.org/10.3390/geosciences8050184)), Norton Sound ([Prescott and Zimmermann, 2015](https://doi.org/10.7289/V5V69GJ9)), Aleutian Islands ([Zimmermann et al., 2013](https://repository.library.noaa.gov/view/noaa/4426); [Zimmermann and Prescott, 2021](https://doi.org/10.1111/fog.12519)), Gulf of Alaska ([Zimmermann et al., 2019](https://doi.org/10.3390/geosciences9100409)), and surrounding areas of the north Pacific ([GEBCO Bathymetric Compilation Group 2024, 2024](https://doi.org/10.5285/1c44ce99-0a0d-5f4f-e063-7086abc0ea0f)). The native resolution of DTMs was either 100 x 100 m or 15 arc-seconds (GEBCO). Spatial coverage of individual DTMs overlap and the depth value for each location is assigned using the DTM that corroborates most closely with nearby bottom trawl survey depth observations.

## Contours

![](/assets/bathy_images/akgfmaps_contours.png)

[Bathymetry contours/isolines](./inst/extdata/bathymetry.gpkg) in were calculated from a 100 x 100 m raster using `as.contour()` from the [terra R package](https://rspatial.org/) to obtain depth contour lines for 50, 100, 200, 300, 400, 500, 600, 800, 1000, 1200, 1500, 2000, and 3000 m isobaths. Contour lines were simplified using the Douglas-Peuker algorithm with a 0.3 retention proportion using `ms_simplify()` in the [rmapshaper package](https://andyteucher.ca/rmapshaper/). Contours with length \< 40 km were removed.

## Disclaimer

Bathymetric data in akgfmaps are not to be used for navigation. For navigation products, please refer to U.S. Nautical Charts available from the NOAA Office of Coast Survey [(https://www.nauticalcharts.noaa.gov/)](https://www.nauticalcharts.noaa.gov/).

## References

Danielson, S. L., Dobbins, E.L., Jakobsson, M. Johnson, M. A., Weingartner, T. J., Williams, W. J., Zarayskaya, Y., 2023. *Sounding the northern seas*. Eos, 96. <https://doi.org/10.1029/2015EO040975>

GEBCO Bathymetric Compilation Group 2024, 2024. *The GEBCO_2024 Grid - a continuous terrain model of the global oceans and land.* <https://doi.org/10.5285/1c44ce99-0a0d-5f4f-e063-7086abc0ea0f>

NOAA NGDC, 2009. Prince William Sound, Alaska 8/3 Arc-second MHHW Coastal Digital Elevation Model. NOAA National Centers for Environmental Information. <https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ngdc.mgg.dem:735>

Hijmans R (2025). *terra: Spatial Data Analysis*. R package version 1.8-61. <https://rspatial.org/>

Prescott, M.M., Zimmermann, M., 2015. *Smooth sheet bathymetry of Norton Sound.* U.S. Dep. Commer., NOAA Tech. Memo. NMFS-AFSC-298. 23. <https://doi.org/10.7289/V5V69GJ9>

Zimmermann, M., Prescott, M.M., 2021. *Passes of the Aleutian Islands: First detailed description.* Fish. Oceanogr. 30, 280–299. <https://doi.org/10.1111/fog.12519>

Zimmermann, M., Prescott, M.M., 2018. *Bathymetry and canyons of the eastern Bering Sea slope.* Geosci. 8. <https://doi.org/10.3390/geosciences8050184>

Zimmermann, M., Prescott, M.M., Haeussler, P.J., 2019. *Bathymetry and geomorphology of Shelikof Strait and the western Gulf of Alaska.* Geosci. 9, 1–31. <https://doi.org/10.3390/geosciences9100409>

Zimmermann, M., Prescott, M.M., Rooper, C.N., 2013. *Smooth Sheet Bathymetry of the Aleutian Islands.* United States Dep. Commer. NOAA Tech. Memo. NMFS-AFSC-, 43. <https://repository.library.noaa.gov/view/noaa/4426>
