## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
library(akgfmaps)

## ----eval=FALSE---------------------------------------------------------------
#  ?make_idw_map

## -----------------------------------------------------------------------------
# Load 2017 EBS and NBS yellowfin sole data
yfs2017 <- akgfmaps::YFS2017
head(yfs2017)

## ---- message=FALSE, warning=FALSE, fig.width=8, fig.height = 6---------------
opt1 <- make_idw_map(x = yfs2017, # Pass data as a data frame
             region = "bs.all", # Predefined bs.all area
             set.breaks = "jenks", # Gets Jenks breaks from classint::classIntervals()
             in.crs = "+proj=longlat", # Set input coordinate reference system
             out.crs = "EPSG:3338", # Set output coordinate reference system
             grid.cell = c(20000, 20000), # 20x20km grid
             key.title = "yellowfin sole") # Include yellowfin sole in the legend title


## -----------------------------------------------------------------------------
opt1$region
opt1$n.breaks
opt1$key.title

## -----------------------------------------------------------------------------
class(opt1$extrapolation.grid)

## -----------------------------------------------------------------------------
class(opt1$continuous.grid)

## ---- warning = FALSE, fig.width=8, fig.height = 6----------------------------
opt1$plot

## ----message=FALSE, warning=FALSE, fig.width=8, fig.height = 6----------------
# Grey Note that the WHOLE list that was returned by make_idw_map should be passed to the function.
opt1.grey <- opt1 |> change_fill_color(new.scheme = "grey", show.plot = FALSE)

# Red
opt1.red <- opt1 |> change_fill_color(new.scheme = "red", show.plot = FALSE)

opt1.grey$plot
opt1.red$plot


## ----warning=FALSE, fig.width=8, fig.height = 6-------------------------------
opt1.grey <- opt1.grey |> add_map_labels()
opt1.grey$plot

## ----warning=FALSE, message=FALSE, fig.width=8, fig.height = 6----------------
# ONLY custom label
opt1.red2 <- opt1.red |> add_map_labels(new.places = data.frame(type = "islands", region = "bs.all", x = -163, y = 57, lab = "Bristol Bay"), lab.replace = TRUE)
opt1.red2$plot


## ----warning=FALSE, message=FALSE, fig.width=8, fig.height = 6----------------
# Add custom label
opt1.red <- opt1.red |> add_map_labels(new.places = data.frame(type = "islands", region = "bs.all", x = -163, y = 57, lab = "Bristol Bay"), lab.replace = FALSE)
opt1.red$plot


## ----warning = FALSE, fig.width=8, fig.height = 6-----------------------------
opt1.grey |>
create_map_file(width = 8, # Passed to png()
                height = 6,  # Passed to png() 
                units = "in",  # Passed to png()
                res = 300,  # Passed to png()
                bg = "transparent", # Passed to png()
                file.path = "./", 
                try.change_text_size = TRUE)

## ----warning=FALSE, fig.width=8, fig.height = 6-------------------------------
opt.grey2 <- opt1.grey |> change_text_size(size.mult = 0.5)
opt.grey2$plot

