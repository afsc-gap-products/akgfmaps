# 1. Install akgfmaps using the devtools package ---------------------------------------------------

install.packages("devtools", "dplyr")
devtools::install_github(repo = "afsc-gap-products/akgfmaps")

# 2. Load akgfmaps/verify that you have version 4 installed ------------------------------------
library(akgfmaps)

# Verify that you have akgfmaps version >= 4.0.0
(session <- sessionInfo())

session$loadedOnly$akgfmaps$Version > 4

# Troubleshooting ----------------------------------------------------------------------------------

# Problem: API Rate Limit error
# Solution: Setup a GitHub Personal Access Token in R Studio
# Instructions: https://carpentries.github.io/sandpaper-docs/github-pat.html

# Workaround without API access
#
# 1. Install these packages from CRAN:
#   ggplot2, sf, stars, terra, gstat (>= 2.0-1), stats, utils, methods, colorspace, RColorBrewer,
#   here, ggspatial, rmapshaper, shadowtext, ggthemes, classInt (>= 0.4-1), units
#
# 2. Download the akgfmaps tarball from:
#
#
# 3. Install the akgmaps tarball using install.packages()
