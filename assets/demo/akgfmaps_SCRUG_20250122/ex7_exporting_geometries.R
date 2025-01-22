# Exporting shapefiles and viewing layers
# Sean Rohan <sean.rohan@noaa.gov>
# Survey-Centric R Users Group
# December 13, 2023
# https://github.com/afsc-gap-products/akgfmaps
# akgfmaps version 3.4.0

library(akgfmaps)

# Make a .zip file containing regional shapefiles (for distribution to non-R users)
# Includes metadata

generate_region_zip(select.region = "bssa1",
                    set.crs = "WGS84",
                    zip.path = NULL)

# Generate a pdf showing individual layers
generate_layer_guide()

# Generate a pdf showing regional shapefiles
generate_region_guide()
