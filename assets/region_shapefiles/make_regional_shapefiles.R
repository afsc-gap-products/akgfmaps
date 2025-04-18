# Generate zip files with regional shapefiles
# Run for every new release or when shapefiles are updated
library(akgfmaps)

todays_date <- gsub(x = Sys.Date(), pattern = "-", replacement = "")

akgfmaps::generate_region_zip(select.region = "nbs",
                              zip.path = here::here("assets", "region_shapefiles", paste0("akgfmaps_shapefiles_nbs_", todays_date, ".zip")))
akgfmaps::generate_region_zip(select.region = "sebs",
                              zip.path = here::here("assets", "region_shapefiles", paste0("akgfmaps_shapefiles_sebs_", todays_date, ".zip")))
akgfmaps::generate_region_zip(select.region = "ebs.slope",
                              zip.path = here::here("assets", "region_shapefiles", paste0("akgfmaps_shapefiles_ebsslope_", todays_date, ".zip")))
akgfmaps::generate_region_zip(select.region = "goa",
                              zip.path = here::here("assets", "region_shapefiles", paste0("akgfmaps_shapefiles_goa_", todays_date, ".zip")))
akgfmaps::generate_region_zip(select.region = "ai",
                              zip.path = here::here("assets", "region_shapefiles", paste0("akgfmaps_shapefiles_ai_", todays_date, ".zip")))
akgfmaps::generate_region_zip(select.region = "ecs",
                              zip.path = here::here("assets", "region_shapefiles", paste0("akgfmaps_shapefiles_chukchi_", todays_date, ".zip")))
