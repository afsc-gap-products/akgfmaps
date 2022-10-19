## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
library(akgfmaps)
yfs2017 <- akgfmaps::YFS2017
eval_plot_breaks(CPUE = yfs2017$CPUE_KGHA, n.breaks = 5)

## ----message=FALSE, warning=FALSE---------------------------------------------
# Create IDW for Jenks
yfs.opt1 <- make_idw_map(x = yfs2017,
             region = "bs.all",
             set.breaks = "jenks",
             in.crs = "+proj=longlat", # Set input coordinate reference system
             out.crs = "EPSG:3338", # Set output coordinate reference system
             grid.cell = c(20000, 20000), # 20x20km grid
             key.title = "yellowfin sole")

yfs.opt2 <- make_idw_map(x = yfs2017,
             region = "bs.all",
             in.crs = "+proj=longlat", # Set input coordinate reference system
               out.crs = "EPSG:3338", # Set output coordinate reference system
               grid.cell = c(20000, 20000),  # 20x20km grid
             set.breaks = "kmeans", 
             key.title = "yellowfin sole")

yfs.opt1$plot
yfs.opt2$plot

## ---- message=FALSE, warning=FALSE--------------------------------------------
yfs.opt3 <- make_idw_map(x = yfs2017,
             region = "bs.all",
            in.crs = "+proj=longlat", # Set input coordinate reference system
             out.crs = "EPSG:3338", # Set output coordinate reference system
             set.breaks = c(0, 25, 100, 250, 500, 1000),
               grid.cell = c(20000, 20000),  # 20x20km grid
             key.title = "yellowfin sole")
yfs.opt3$plot


