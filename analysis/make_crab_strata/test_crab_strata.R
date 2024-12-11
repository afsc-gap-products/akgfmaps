devtools::install_github(repo = "afsc-gap-products/akgfmaps@dev2")

library(akgfmaps)

ebs_stocks <- akgfmaps::get_crab_strata(select.region = "ebs", set.crs = "EPSG:3338")

ggplot() +
  geom_sf(data = ebs_stocks,
          mapping = aes(fill = STRATUM)) +
  facet_wrap(~STOCK)

nbs_stocks <- akgfmaps::get_crab_strata(select.region = "nbs", set.crs = "EPSG:3338")

ggplot() +
  geom_sf(data = nbs_stocks,
          mapping = aes(fill = STRATUM)) +
  facet_wrap(~STOCK)

nsrkc <- akgfmaps::get_crab_strata(select.stock = "nsrkc", set.crs = 4269)

ggplot() +
  geom_sf(data = nsrkc)

akgfmaps::get_crab_strata(select.stock = "pirkc", set.crs = 4269)

pirkc <- akgfmaps::get_crab_strata(select.stock = "pirkc", set.crs = "EPSG:3338")

ggplot() +
  geom_sf(data = pirkc,
          mapping = aes(fill = STRATUM))

akgfmaps::get_crab_strata(select.stock = "pibkc", set.crs = 4269)

akgfmaps::get_crab_strata(select.stock = "ebstc", set.crs = 4269)

akgfmaps::get_crab_strata(select.stock = "bbrkc", set.crs = 4269)

akgfmaps::get_crab_strata(select.stock = "ebstc", set.crs = 4269)
