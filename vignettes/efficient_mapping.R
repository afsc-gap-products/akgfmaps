## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- message=FALSE, warning=FALSE, fig.width=12, fig.height = 9--------------
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

