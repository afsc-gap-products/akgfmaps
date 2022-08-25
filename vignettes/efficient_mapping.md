---
title: "Make maps quickly"
author: "Sean Rohan"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Make maps quickly}
  %\VignetteEngine{knitr::knitr}
  %\VignetteEncoding{UTF-8}
---



## Make maps quickly and efficiently using pipe operators

The following code shows how to use akgfmaps to quickly and efficiently make maps without cluttering up the workspace. Prior to this step, it is necessary to determine what breaks, colors, labels, and map sizes to use for each species.


```r
library(akgfmaps)

akgfmaps:::YFS2017 %>% 
  make_idw_map(region = "bs.all",
               set.breaks = "jenks",
               in.crs = "+proj=longlat",
             out.crs = "EPSG:3338", # Set output coordinate reference system
             grid.cell = c(20000, 20000)) %>% # 20x20km grid
  add_map_labels() %>% 
  change_fill_color(new.scheme = "purple2", show.plot = TRUE) %>%
  create_map_file(file.prefix = NA, 
                            file.path = NA, 
                            try.change_text_size = TRUE, # 12x9 is a pre-defined size
                            width = 12, 
                            height = 9, 
                            units = "in", 
                            res = 300, 
                            bg = "transparent")
```

```
## [inverse distance weighted interpolation]
## [inverse distance weighted interpolation]
```

```
## [1] "Creating map yellowfin sole_12x9.png"
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```
## png 
##   2
```
