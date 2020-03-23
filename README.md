# akgfmaps
An R package for producing publication-ready catch-per-unit-effort maps from bottom-trawl survey catch data from Alaska. The package currently includes support for the eastern Bering Sea continental shelf and northern Bering Sea.

This package was built in R 3.6.2.

# Installation

akgfmaps can be installed using the following code:

```{r}
devtools::install_github("sean-rohan/akgfmaps", build_vignettes = TRUE)
```

The automatic installer may terminate with errors . If errors occur, try to install dependencies individually, then try to install the akgfmaps package. 

Installation errors can occur when packages were built using different versions of R, which may result in non-zero exit status errors that will no affect functionality. Errors can sometimes be suppressed using:

```{r}
devtools::install_github("sean-rohan/akgfmaps", build_vignettes = TRUE, R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
```

# Vignettes

Vignettes are accessible using:

```{r}
browseVignettes('akgfmaps')
```
