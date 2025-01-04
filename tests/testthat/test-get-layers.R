library(akgfmaps)

testthat::test_that("Test regions exists",
                    {
                      expect_equal(length(suppressWarnings(akgfmaps::get_base_layers(select.region = c("sebs", "nbs", "ai", "goa", "ecs"), set.crs = "auto"))), 13)
                    }
)

testthat::test_that("Test get survey bathymetry ",
                    {
                      expect_true(class(get_survey_bathymetry(select.region = "ebs", set.crs = "auto"))[1] == "sf")
                    }
)

testthat::test_that("Test Bering Sea grid filtering",
                    {
                      bs_south <- suppressWarnings(akgfmaps::get_base_layers(select.region = "bs.south",
                                                                             set.crs = "WGS84"));
                      bs_north <- suppressWarnings(akgfmaps::get_base_layers(select.region = "nbs",
                                                                             set.crs = "WGS84"));
                      expect_equal(length(bs_north$survey.grid$geometry), 144);
                      expect_equal(length(bs_south$survey.grid$geometry), 350)
                    }
)

testthat::test_that("Test ADFG",
                    {
                      adfg <- suppressWarnings(akgfmaps::get_adfg_areas(set.crs = "auto"));
                      adfg_full  <- suppressWarnings(akgfmaps::get_adfg_areas(set.crs = "auto", subset.fields = FALSE));
                      expect_equal(nrow(adfg), 1736);
                      expect_equal(ncol(adfg), 26);
                      expect_equal(ncol(adfg_full), 40);
                      expect(
                        all(sf::st_is_valid(adfg)),
                        failure_message = "At least one invalid geometry in ADFG Areas"
                      )
                    }
)

testthat::test_that("Test NMFS Areas",
                    {
                      nmfs <- suppressWarnings(
                        akgfmaps::get_nmfs_areas(set.crs = "EPSG:3338")
                      );
                      testthat::expect(
                        all(
                          unique(nmfs$REP_AREA) %in%
                            c(400, 508, 509, 512, 513, 514, 516, 517, 518, 519, 521, 523, 524, 530,
                              541, 542, 543, 550, 610, 620, 630, 640, 649, 650, 659)
                        ),
                        failure_message = "Missing one or more reporting areas in NMFS Areas");
                      expect_equal(nrow(nmfs), 25);
                      expect_equal(length(unique(nmfs$REP_AREA)), 25);
                      expect(all(sf::st_is_valid(nmfs)),
                             failure_message = "At least one NMFS Areas invalid geometry is in valid in WGS84. ")
                    }
)

testthat::test_that("Test INPFC",
                    {
                      inpfc_goa <- suppressWarnings(
                        akgfmaps::get_inpfc_strata(select.region = "goa", set.crs = "WGS84")
                      );
                      expect_equal(nrow(inpfc_goa), 5);
                      expect(all(sf::st_is_valid(inpfc_goa)),
                             failure_message = "At least one GOA INPFC area geometry is invalid in valid in WGS84.")
                      inpfc_ai <- suppressWarnings(
                        akgfmaps::get_inpfc_strata(select.region = "ai", set.crs = "WGS84")
                      );
                      expect_equal(nrow(inpfc_ai), 4);
                      expect(all(sf::st_is_valid(inpfc_ai)),
                             failure_message = "At least one AI INPFC area geometry is invalid in valid in WGS84.")
                    }
)


testthat::test_that("Test ESR areas and subareas",
                    {
                      esr_subarea <- akgfmaps::get_esr_regions(select.region = "esr_subarea",
                                                               set.crs = "WGS84");
                      esr_area <- akgfmaps::get_esr_regions(select.region = "esr_area",
                                                            set.crs = "WGS84");
                      esr_subarea_inside <- akgfmaps::get_esr_regions(select.region = "esr_subarea_inside",
                                                                      set.crs = "WGS84");
                      esr_area_inside <- akgfmaps::get_esr_regions(select.region = "esr_area_inside",
                                                                   set.crs = "WGS84");
                      expect(all(c(sf::st_is_valid(esr_subarea),
                                   sf::st_is_valid(esr_area),
                                   sf::st_is_valid(esr_subarea_inside),
                                   sf::st_is_valid(esr_area_inside))),
                             failure_message = "At least one ESR area/subarea geometry is invalid in valid in WGS84.")
                    }
)

testthat::test_that("Test crab strata",
                    {
                      pirkc <- akgfmaps::get_crab_strata(select.stock = "pirkc", set.crs = "EPSG:32603");
                      pibkc <- akgfmaps::get_crab_strata(select.stock = "pibkc", set.crs = "EPSG:32603");
                      nsrkc  <- akgfmaps::get_crab_strata(select.stock = "nsrkc", set.crs = "EPSG:32603");
                      bbrkc  <- akgfmaps::get_crab_strata(select.stock = "bbrkc", set.crs = "EPSG:32603");
                      smbkc  <- akgfmaps::get_crab_strata(select.stock = "smbkc", set.crs = "EPSG:32603");
                      ebssc  <- akgfmaps::get_crab_strata(select.stock = "ebssc", set.crs = "EPSG:32603");
                      ebstc  <- akgfmaps::get_crab_strata(select.stock = "ebstc", set.crs = "EPSG:32603");
                      expect(
                        all(
                          c(
                            sf::st_is_valid(pirkc),
                            sf::st_is_valid(pibkc),
                            sf::st_is_valid(bbrkc),
                            sf::st_is_valid(smbkc),
                            sf::st_is_valid(ebssc),
                            sf::st_is_valid(ebstc)
                          )
                        ),
                        failure_message = "At least one crab area geometry is invalid in EPSG:32603 (UTM Zone 3 North CRS)"
                      )
                    }
)

testthat::test_that("Test BSIERP strata",
                    {
                      bsierp <- suppressWarnings(akgfmaps::get_bsierp_regions(set.crs = "NAD83"));
                      expect(all(sf::st_is_valid(bsierp)),
                             failure_message = "At lesat one BSIERP region geometry is invalid in NAD83.")
                    }
)
