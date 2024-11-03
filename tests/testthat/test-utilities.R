library(akgfmaps)

testthat::test_that("Test data frame CRS transformation",
                    {expect_match(transform_data_frame_crs(data.frame(x = -170, y = 54, set.crs = "EPSG:3338"),
                                                           in.crs = "EPSG:4326",
                                                           out.crs = "EPSG:3338")$set.crs, "EPSG:3338")})

testthat::test_that("Check breaks function",
                    {expect_equal(ncol(eval_plot_breaks(CPUE = 1:20, n.breaks = 4, styles = "quantile")), 6)})

testthat::test_that("Check stations",
                    {expect_equal(length(get_survey_stations(select.region = "sebs",
                                                             include.corners = TRUE)), 376);
                      expect_equal(length(get_survey_stations(select.region = "sebs",
                                                              include.corners = TRUE)), 350);
                      expect_equal(length(get_survey_stations(select.region = "nbs",
                                                              include.corners = TRUE)), 144)})



