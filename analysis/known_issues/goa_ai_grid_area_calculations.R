# AI and GOA grid area calculations 
library(akgfmaps)

crs_alaska_aea <- "EPSG:3338"
crs_nad83 <- "EPSG:4269"


# Gulf of Alaska ----
goa_grid <- sf::st_read("G:/GOA/GOA 2021/Files for boats/ArcGIS/GOAGRID_2021/goagrid_with_2019_data.shp")

goa_grid_alaska_aea <- sf::st_transform(goa_grid, crs = crs_alaska_aea)
goa_grid_nad83 <- sf::st_transform(goa_grid, crs = crs_nad83)

sf::st_area(goa_grid) - sf::st_area(goa_grid_alaska_aea)

hist(as.numeric(sf::st_area(goa_grid)) - goa_grid$AREA)
hist(as.numeric(sf::st_area(goa_grid)) - goa_grid$AREA_KM2*1e6)
hist(as.numeric(sf::st_area(goa_grid)) - goa_grid$AREA)

# AREA field was calculated incorrectly using a precision of six digits, i.e., precision varies with scale.
plot(goa_grid$AREA, as.numeric(sf::st_area(goa_grid)) - goa_grid$AREA, xlab = "GOA Grid AREA (m^2)", ylab = "Recaculated minus GOA Grid AREA (m^2)")

# There is likely another precision issue with AREA_KM2, but I'm not sure about the mechanism.
plot(goa_grid$AREA_KM2*1e6, as.numeric(sf::st_area(goa_grid)) - goa_grid$AREA_KM2*1e6, xlab = "GOA Grid AREA_KM2 (m^2)", ylab = "Recaculated minus GOA Grid AREA_KM2 (m^2)")


# Aleutian Islands ----
ai_grid <- sf::st_read("G:/ALEUTIAN/AI 2022/ArcGIS/Chart Products/aigrid_trawable_thru2018_Emily.shp")

ai_layers <- akgfmaps::get_base_layers(select.region = "ai", set.crs = "EPSG:3338")

ai_grid_alaska_aea <- sf::st_transform(ai_grid, crs = crs_alaska_aea)
ai_grid_nad83 <- sf::st_transform(ai_grid, crs = crs_nad83)

sf::st_area(ai_grid) - sf::st_area(ai_grid_alaska_aea)
sf::st_area(ai_grid) - sf::st_area(ai_grid_nad83)


ai_layers$survey.grid$RECALC <- as.numeric(sf::st_area(ai_layers$survey.grid))

ai_grid$RECALC_EMILY <- as.numeric(sf::st_area(ai_grid))

dplyr::anti_join(
as.data.frame(ai_layers$survey.grid) |>
  dplyr::select(ID, AIGRID_ID, RECALC, AREA),
as.data.frame(ai_grid) |>
  dplyr::select(ID, AIGRID_ID, RECALC_EMILY, AREA)
)

hist(as.numeric(sf::st_area(ai_grid)) - ai_grid$AREA)
hist(as.numeric(sf::st_area(ai_grid_nad83)) - ai_grid$AREA)
hist(as.numeric(sf::st_area(ai_grid_alaska_aea)) - ai_grid$AREA)

# AREA field was calculated incorrectly using a precision of six digits, i.e., precision varies with scale
plot(ai_grid$AREA, as.numeric(sf::st_area(ai_grid)) - ai_grid$AREA, xlab = "AI Grid AREA (m^2)", ylab = "Recaculated minus AI Grid AREA (m^2)")

ragg::agg_png(here::here("plots", "ai_grid_comparison.png"), width = 500, height = 200, units = "mm", res = 600)
print(
ggplot() +
  geom_sf(data = ai_layers$survey.grid, 
          color = "red", fill = NA, linewidth = 0.1) +
  geom_sf(data = ai_grid, fill = NA, linewidth = 0.1)
)
dev.off()



ggplot() +
  geom_sf(data = ai_layers$survey.grid, fill = NA, linewidth = 0.1) +
  geom_sf_text(data =  
            sf::st_centroid(
ai_layers$survey.grid[which(is.na(as.numeric(sf::st_equals( ai_layers$survey.grid, ai_grid)))),]),
mapping = aes(label = AIGRID_ID),
color = "red")

ggplot() +
  geom_sf(data = ai_layers$survey.grid, fill = NA, linewidth = 0.1) +
  geom_sf_text(data =  
                 sf::st_centroid(
                   ai_grid[which(is.na(as.numeric(sf::st_equals(ai_grid, ai_layers$survey.grid)))),]),
               mapping = aes(label = AIGRID_ID),
               color = "red")

ai_layers$survey.grid[which(is.na(as.numeric(sf::st_equals( ai_layers$survey.grid, ai_grid)))),]
ai_grid[which(is.na(as.numeric(sf::st_equals( ai_layers$survey.grid, ai_grid)))),]


grid_mismatch <- dplyr::inner_join(
as.data.frame(ai_layers$survey.grid),
as.data.frame(ai_grid),
by = "AIGRID_ID") |>
  dplyr::filter(AIGRID_ID %in% c(9872, 9867, 5329, 5331))

sf::st_coordinates(grid_mismatch$geometry.x) == sf::st_coordinates(grid_mismatch$geometry.y)

grid_mismatch[1,]

ggplot() +
  geom_sf(data = ai_layers$survey.strata,
          mapping = aes(fill = factor(STRATUM)),
          color = NA) +
  geom_sf(data = ai_layers$survey.strata |>
            dplyr::filter(STRATUM == 812) |>
            sf::st_centroid(),
          color = "black") +
  geom_sf(data = ai_layers$survey.strata |>
                                     dplyr::filter(STRATUM == 721),
                                   fill = "grey30", 
          color = NA) +
    geom_sf(data = data.frame(x = 179.6225, y = 54.30867) |>
              sf::st_as_sf(coords = c("x", "y"), crs = "EPSG:4326") |>
              sf::st_transform(crs = crs_alaska_aea),
            color = "blue") +
  geom_sf(data = dplyr::filter(ai_layers$survey.grid, AIGRID_ID %in% c(9873, 9869)) |>
            sf::st_centroid(),
          color = "red")
  
  
ai_grid |>
    dplyr::filter(STRATUM == 721) |>
  sf::st_area() |>
  sum() |>
  as.numeric()/1e6
  
  nrow(ai_layers$survey.grid |>
         dplyr::filter(STRATUM == 812))
  
  nrow(ai_grid |>
         dplyr::filter(STRATUM == 812))
  
ai_new_area <- ai_layers$survey.strata |>
    dplyr::group_by(STRATUM) |>
    dplyr::summarise(AREA_KM2 = sum(AREA_KM2),
                     NEW_AREA_KM2 = sum(NEW_AREA_KM2),
                     AREA = sum(AREA)/1e6)

ai_new_area$AREA_KM2-ai_new_area$NEW_AREA_KM2

as.data.frame(ai_new_area)
  