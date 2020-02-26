# akgfmaps
A package to produce publication-quality catch-per-unit-effort maps using R using bottom-trawl survey catch data. Package currently includes support for the eastern Bering Sea continental shelf and northern Bering Sea.

Spatial layers necessary for plotting are included in the package.

This package was built in R 3.6.2.


# Installation

akgfmaps can be installed by starting R and running the code below.

To build with vignettes:
```{r}
devtools::install_github("sean-rohan/akgfmaps", build_vignettes = TRUE)
```

To build without vignettes (faster):
```{r}
devtools::install_github("sean-rohan/akgfmaps")
```


#Vignettes

Once installed, vignettes for akgfmaps are accessible using \code{browseVignettes('akgfmaps')}