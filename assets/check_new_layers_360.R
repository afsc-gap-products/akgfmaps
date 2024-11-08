# Test version of 3.6.0
# remotes::install_github(repo = "afsc-gap-products/akgfmaps@dev2")

library(akgfmaps)
library(akmarineareas2)

crs_nad83 <- 4269
crs_aea <- 3338

# ADFG ----
adfg_dd <- get_adfg_areas(set.crs = crs_nad83)

adfg_aea <- get_adfg_areas(set.crs = crs_aea)

head(adfg_dd)

ggplot() +
  geom_sf(data = adfg_dd, fill = "blue") +
  geom_sf(data = akmarineareas2::adfg_dd, color = "red", fill = NA)

ggplot() +
  geom_sf(data = adfg_aea, fill = "blue") +
  geom_sf(data = akmarineareas2::adfg, color = "red", fill = NA)

# ADFG - All fields ----
adfg_all_fields_dd <- get_adfg_areas(set.crs = crs_nad83, subset.fields = FALSE)

names(adfg_all_fields_dd)

# ESR subareas ----
esr_subarea_dd <- get_esr_regions(set.crs = crs_nad83, select.region = "esr_subarea")

esr_subarea_aea <- get_esr_regions(set.crs = crs_aea, select.region = "esr_subarea")

names(esr_subarea_dd)

ggplot() +
  geom_sf(data = esr_subarea_dd, fill = "blue") +
  geom_sf(data = akmarineareas2::esr_dd, color = "red", fill = NA)

ggplot() +
  geom_sf(data = esr_subarea_aea, fill = "blue") +
  geom_sf(data = akmarineareas2::esr, color = "red", fill = NA)

# ESR areas ----
esr_areas_dd <- get_esr_regions(set.crs = crs_nad83, select.region = "esr_area")

esr_areas_aea <- get_esr_regions(set.crs = crs_aea, select.region = "esr_area")

names(esr_areas_dd)

ggplot() +
  geom_sf(data = esr_areas_dd)

ggplot() +
  geom_sf(data = esr_areas_aea)


# ESR inside areas ----
esr_areas_inside_dd <- get_esr_regions(set.crs = crs_nad83, select.region = "esr_area_inside")

esr_areas_inside_aea <- get_esr_regions(set.crs = crs_aea, select.region = "esr_area_inside")

names(esr_areas_inside_dd)

ggplot() +
  geom_sf(data = esr_areas_inside_dd)

ggplot() +
  geom_sf(data = esr_areas_inside_aea)

# ESR inside subareas ----
esr_subareas_inside_dd <- get_esr_regions(set.crs = crs_nad83, select.region = "esr_subarea_inside")

esr_subareas_inside_aea <- get_esr_regions(set.crs = crs_aea, select.region = "esr_subarea_inside")

names(esr_subareas_inside_dd)

ggplot() +
  geom_sf(data = esr_subareas_inside_dd)

ggplot() +
  geom_sf(data = esr_subareas_inside_aea)


# BSIERP ----
bsierp_dd <- get_bsierp_regions(set.crs = crs_nad83)

bsierp_aea <- get_bsierp_regions(set.crs = crs_aea)

names(bsierp_dd)

head(bsierp_dd)

ggplot() +
  geom_sf(data = bsierp_dd, fill = "blue") +
  geom_sf(data = akmarineareas2::bsierp_dd, color = "red", fill = NA)

ggplot() +
  geom_sf(data = bsierp_aea, fill = "blue") +
  geom_sf(data = akmarineareas2::bsierp, color = "red", fill = NA)

