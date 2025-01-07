# Example: Retrieving data for survey areas
# SCRUG, akgfmaps, January 22, 2025
# Created by Sean Rohan <sean.rohan@noaa.gov>

library(akgfmaps)

# 1. get_base_layers() for bottom trawl ------------------------------------------------------------
# Access AFSC bottom trawl survey regions, handle coordinate reference system (CRS)
# transformations, create plot breaks and axis labels.
# Outputs work with any graphical interface that supports 'sf' object classes from the sf package,
# but is optimized to work with ggplot2.

?get_base_layers

# Get layers for one survey region
ebs_layers <- akgfmaps::get_base_layers(
  select.region = "sebs", # EBS shelf bottom traw survey regions
  set.crs = "EPSG:3338", # Coordinate reference system, in this case Alaska Albers Equal Area
  high.resolution.coast = FALSE, # Set to TRUE when coast is used for analyses, e.g. raster masking
  design.year = NULL # Return the most recent/current survey design
)

# Inspect Layers returned
names(ebs_layers)

ggplot() +
  geom_sf(data = ebs_layers$akland) +
  geom_sf(data = ebs_layers$survey.grid, # Survey station grid
          fill = NA) +
  geom_sf(data = ebs_layers$bathymetry) +
  geom_sf(data = ebs_layers$graticule,
          alpha = 0.3) +
  scale_x_continuous(limits = ebs_layers$plot.boundary$x,
                     breaks = ebs_layers$lon.breaks) +
  scale_y_continuous(limits = ebs_layers$plot.boundary$y,
                     breaks = ebs_layers$lat.breaks) +
  theme_bw() +
  theme(panel.grid = element_blank())


# NEW: Return layers for multiple surveys ----
all_bt_layers <- akgfmaps::get_base_layers(
  select.region = c("ebs", "goa", "ai", "ebs.slope", "ecs"), # EBS, EBS slope, NBS, GOA, AI, and ECS
  set.crs = "EPSG:3338", # Coordinate reference system, in this case Alaska Albers Equal Area
  high.resolution.coast = FALSE # Set to TRUE when coast is used for analyses, e.g. raster masking
)

ggplot() +
  geom_sf(data = all_bt_layers$akland) +
  geom_sf(data = all_bt_layers$survey.area,
          mapping = aes(fill = SURVEY_NAME)) +
  geom_sf(data = all_bt_layers$bathymetry) +
  geom_sf(data = all_bt_layers$graticule,
          alpha = 0.3) +
  scale_x_continuous(limits = all_bt_layers$plot.boundary$x,
                     breaks = all_bt_layers$lon.breaks) +
  scale_y_continuous(limits = all_bt_layers$plot.boundary$y,
                     breaks = all_bt_layers$lat.breaks) +
  scale_fill_viridis_d(name = "Survey Name") +
  theme_bw() +
  theme(panel.grid = element_blank())


# NEW: All bottom trawl survey regions have the same fields. Fields match GAP_PRODUCTS. ----
head(all_bt_layers$survey.area)
head(all_bt_layers$survey.grid)
head(all_bt_layers$survey.strata) # STRATUM field matches AREA_ID field in GAP_PRODUCTS and STRATUM
# field in RACEBASE

# NEW: Retrieve layers for a specific design year for a bottom trawl survey ----
#   - Returns the most recent design year when NULL.
#   - Numerous caveats may apply when dealing with old survey designs (e.g., area calculation issues)
#   - Many historical designs are not available and may never be.

goa_1984_layers <- akgfmaps::get_base_layers(
  select.region = "goa.west", # Setup plot for WGOA
  design.year = 1984,
  set.crs = "EPSG:3338"
)

goa_2025_layers <- akgfmaps::get_base_layers(
  select.region = "goa.west",
  design.year = 2025,
  set.crs = "EPSG:3338"
)

# Combine survey area strata
goa_1984_2025_strata <- rbind(
  goa_1984_layers$survey.strata,
  goa_2025_layers$survey.strata
)

# Plot 1984 and 2025 survey strata for the WGOA
ggplot() +
  geom_sf(data = goa_1984$akland) +
  geom_sf(data = goa_1984_2025_strata,
          mapping = aes(color = factor(DESIGN_YEAR)),
          fill = NA,
          alpha = 0.5) +
  scale_color_manual(name = "Design year",
                     values = c("red", "black")) +
  scale_x_continuous(limits = goa_1984$plot.boundary$x,
                     breaks = goa_1984$lon.breaks) +
  scale_y_continuous(limits = goa_1984$plot.boundary$y,
                     breaks = goa_1984$lat.breaks) +
  theme_bw()

goa_1984_2025_grid <- rbind(
  goa_1984_layers$survey.grid,
  goa_2025_layers$survey.grid
)

# Plot 1984 and 2025 survey grid for the WGOA
ggplot() +
  geom_sf(data = goa_1984$akland) +
  geom_sf(data = goa_1984_2025_grid,
          mapping = aes(color = factor(DESIGN_YEAR)),
          fill = NA,
          alpha = 0.5) +
  scale_color_manual(name = "Design year",
                     values = c("red", "black")) +
  scale_x_continuous(limits = goa_1984$plot.boundary$x,
                     breaks = goa_1984$lon.breaks) +
  scale_y_continuous(limits = goa_1984$plot.boundary$y,
                     breaks = goa_1984$lat.breaks) +
  theme_bw()

# EBS shelf survey with corner stations for the 2023 survey
# The EBS survey dropped corner stations for the 2024 survey. Using design.year = 2023 (last year
# with corner stations) returns the latest available design year for the survey grid and strata.
#
# Note that there are warning when selecting a design.year that does not exist; the closest
# preceding year is returned instead.

ebs_2023 <- akgfmaps::get_base_layers(
  select.region = "sebs",
  design.year = 2023,
  set.crs = "EPSG:3338"
)

# DESIGN_YEAR for survey.grid is 2010, the year the grid was redefined and extended to the NBS
head(ebs_2023$survey.grid)

# DESIGN_YEAR for survey.strata is 2022, the year EBS shelf and NBS survey were snapped together and
# areas were recalculated using the Albers equal area projection
head(ebs_2023$survey.strata)


# Error when trying to retrieve layers for a design.year that's earlier than the earliest available
# in the package. It's likely that some of these years cannot be recovered or may not have existed
# in digital format.
akgfmaps::get_base_layers(
  select.region = "sebs",
  design.year = 1982,
  set.crs = "EPSG:3338"
)

ggplot() +
  geom_sf(data = ebs_2023$survey.grid)

# Aleutian Islands bottom trawl survey removed Bowers Ridge in 1991 (start of the standardized
# survey time series)

ai_1980 <- get_base_layers(
  select.region = "ai",
  design.year = 1980,
  set.crs = "EPSG:3338"
)

ai_1991 <- get_base_layers(
  select.region = "ai",
  design.year = 1991,
  set.crs = "EPSG:3338"
)

ai_1980_1991_strata <- rbind(ai_1980$survey.strata, ai_1991$survey.strata)

ggplot() +
  geom_sf(data = ai_1980_1991_strata,
          mapping = aes(fill = factor(STRATUM)),
          color = NA) +
  facet_wrap(~paste0("DESIGN_YEAR = ", DESIGN_YEAR)) +
  scale_fill_viridis_d(option = "H") +
  scale_x_continuous(limits = ai_1980$plot.boundary$x,
                     breaks = ai_1980$lon.breaks) +
  scale_y_continuous(limits = ai_1980$plot.boundary$y,
                     breaks = ai_1980$lat.breaks) +
  theme_bw() +
  theme(legend.position = "none")


# 2. NEW: get_base_layers() for longline survey ----------------------------------------------------
# Access AFSC longline survey layers
# - Currently no access to historical designs using design.year

ll_goa_layers <- akgfmaps::get_base_layers(
  select.region = "ll.goa",
  set.crs = "EPSG:3338",
  design.year = NULL
)

ggplot() +
  geom_sf(data = ll_goa_layers$akland) + # Same land polygon
  geom_sf(data = ll_goa_layers$survey.grid) + # Survey stations rather than a grid
  geom_sf_text(data = ll_goa_layers$survey.grid,
               mapping = aes(label = STATION_NUMBER),
               hjust = 0.5,
               vjust = -0.5) +
  scale_x_continuous(limits = ll_goa_layers$plot.boundary$x,
                     breaks = ll_goa_layers$lon.breaks) +
  scale_y_continuous(limits = ll_goa_layers$plot.boundary$y,
                     breaks = ll_goa_layers$lat.breaks) +
  theme_bw() +
  theme(axis.title = element_blank())

# Example of cleaner station labels using ggrepel

install.packages("ggrepel")

library(ggrepel)

ll_goa_layers$survey.grid[, c('x', 'y')] <- sf::st_coordinates(ll_goa_layers$survey.grid)

ggplot() +
  geom_sf(data = ll_goa_layers$akland) + # Same land polygon
  geom_sf(data = ll_goa_layers$survey.grid) + # Survey stations rather than a grid
  geom_text_repel(data = ll_goa_layers$survey.grid,
                  mapping = aes(x = x,
                                y = y,
                                label = STATION_NUMBER),
                  hjust = 0.5,
                  vjust = -0.5) +
  scale_x_continuous(limits = ll_goa_layers$plot.boundary$x,
                     breaks = ll_goa_layers$lon.breaks) +
  scale_y_continuous(limits = ll_goa_layers$plot.boundary$y,
                     breaks = ll_goa_layers$lat.breaks) +
  theme_bw() +
  theme(axis.title = element_blank())


# 3. NEW: EBS crab stratum boundaries --------------------------------------------------------------
# Access stratum boundaries for EBS and NBS crab stocks
# - Select by stock or return all crab
# - Does not return additional objects, e.g.,

?get_crab_strata

# Layers for a single stock, Bristol Bay red king crab
pirkc_strata <- akgfmaps::get_crab_strata(
  select.stock = "pirkc",
  select.region = "ebs",
  set.crs = "EPSG:3338")

# Layers for all EBS stocks
all_ebs_crab <- akgfmaps::get_crab_strata(
  select.stock = NULL,
  select.region = "ebs",
  set.crs = "EPSG:3338")

head(all_ebs_crab)

ggplot() +
  geom_sf(data = ebs_layers$akland) +
  geom_sf(data = pirkc_strata,
          mapping = aes(fill = STRATUM)) +
  scale_fill_viridis_d() +
  facet_wrap(~STOCK) +
  theme_bw()

# 4. NMFS, ADFG, ESR, and BSIERP layers ------------------------------------------------------------
# - These layers are also available in Matt Callahan's akmarineareas2 package
#   (https://github.com/MattCallahan-NOAA/akmarineareas2)

install.packages("shadowtext")
library(shadowtext)

# Alaska Department of Fish and Game (ADFG) Areas
adfg_areas <- get_adfg_areas(
  set.crs = "EPSG:3338",
  subset.fields = TRUE # Only return a subset of columns
  )

head(adfg_areas)

ggplot() +
  geom_sf(data = adfg_areas)

# National Marine Fisheries Service (NMFS) Statistical Areas
nmfs_areas <- get_nmfs_areas(set.crs = "EPSG:3338")

nmfs_area_labels <- sf::st_centroid(nmfs_areas)
nmfs_area_labels[, c('x', 'y')] <- sf::st_coordinates(nmfs_area_labels)

ggplot() +
  geom_sf(data = nmfs_areas, color = "grey40") +
  geom_shadowtext(data = nmfs_area_labels,
          mapping = aes(x = x, y = y, label = REP_AREA),
          bg.color = "white",
          color = "black") +
  theme_bw()

# Ecosystem Status Report (ESR) regions
esr_areas <- get_esr_regions(
  select.region = "esr_area",
  set.crs = "EPSG:3338"
)

ggplot() +
  geom_sf(data = esr_areas,
          mapping = aes(fill = AREA_NAME)) +
  scale_fill_viridis_d(option = "A") +
  theme_bw()

esr_subareas <- get_esr_regions(
  select.region = "esr_subarea",
  set.crs = "EPSG:3338"
)

ggplot() +
  geom_sf(data = esr_subareas,
          mapping = aes(fill = AREA_NAME)) +
  scale_fill_viridis_d(option = "B") +
  theme_bw()

esr_subareas_inside <- get_esr_regions(
  select.region = "esr_subarea_inside",
  set.crs = "EPSG:3338"
)

ggplot() +
  geom_sf(data = esr_subareas_inside,
          mapping = aes(fill = AREA_NAME)) +
  scale_fill_viridis_d(option = "E") +
  theme_bw()

# Bering Sea Integrated Ecosystem Research Program (BSIERP) regions
bsierp_areas <- get_bsierp_regions(set.crs = "EPSG:3338")

head(bsierp_areas)

ggplot() +
  geom_sf(data = bsierp_areas) +
  geom_sf_text(data = sf::st_centroid(bsierp_areas),
          mapping = aes(label = BSIERP_ID)) +
  theme_bw() +
  theme(axis.title = element_blank())
