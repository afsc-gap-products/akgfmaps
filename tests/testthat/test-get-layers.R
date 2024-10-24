library(akgfmaps)

testthat::test_that("Test regions exists",
                    {expect_equal(length(suppressWarnings(akgfmaps::get_base_layers(select.region = "bs.south", set.crs = "auto"))), 12);
                      expect_equal(length(suppressWarnings(akgfmaps::get_base_layers(select.region = "bs.north", set.crs = "auto"))), 12);
                      expect_equal(length(suppressWarnings(akgfmaps::get_base_layers(select.region = "ecs", set.crs = "auto"))), 12);
                      expect_equal(length(suppressWarnings(akgfmaps::get_base_layers(select.region = "bs.all", set.crs = "auto"))), 12);
                      expect_equal(length(suppressWarnings(akgfmaps::get_base_layers(select.region = "goa", set.crs = "auto"))), 12);
                      expect_equal(length(suppressWarnings(akgfmaps::get_base_layers(select.region = "goa.east", set.crs = "auto"))), 12);
                      expect_equal(length(suppressWarnings(akgfmaps::get_base_layers(select.region = "goa.west", set.crs = "auto"))), 12);
                      expect_equal(length(suppressWarnings(akgfmaps::get_base_layers(select.region = "ai", set.crs = "auto"))), 12);
                      expect_equal(length(suppressWarnings(akgfmaps::get_base_layers(select.region = "ai.west", set.crs = "auto"))), 12);
                      expect_equal(length(suppressWarnings(akgfmaps::get_base_layers(select.region = "ai.central", set.crs = "auto"))), 12);
                      expect_equal(length(suppressWarnings(akgfmaps::get_base_layers(select.region = "ai.east", set.crs = "auto"))), 12)})

testthat::test_that("Test get survey bathymetry ",
                    {expect_true(class(get_survey_bathymetry(select.region = "ebs", set.crs = "auto"))[1] == "sf")})

testthat::test_that("Test Bering Sea grid filtering",
                    {bs_south <- suppressWarnings(akgfmaps::get_base_layers(select.region = "bs.south",
                                                                            set.crs = "WGS84"));
                    bs_north <- suppressWarnings(akgfmaps::get_base_layers(select.region = "nbs",
                                                                                       set.crs = "WGS84"));
                    expect_equal(length(bs_north$survey.grid$geometry), 144);
                    expect_equal(length(bs_south$survey.grid$geometry), 350)})
