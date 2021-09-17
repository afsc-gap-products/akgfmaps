library(akgfmaps)

testthat::test_that("Test data frame CRS transformation",
                    {expect_match(transform_data_frame_crs(data.frame(x = -170, y = 54, set.crs = "EPSG:3338"))$set.crs, "EPSG:3338")})

testthat::test_that("Check breaks function",
                    {expect_equal(ncol(eval_plot_breaks(CPUE = 1:20, n.breaks = 4, styles = "quantile")), 6)})

testthat::test_that("Test normal score transformations",
                    {expect_equal(suppressWarnings(backtransform_normal(scores = akgfmaps::normal_transform(1:10)$z_score, 
                                                                        z_score = akgfmaps::normal_transform(1:10),
                                                                        make_plot = FALSE)), 1:10)})

testthat::test_that("Check stations",
                    {expect_equal(length(get_survey_stations(select.region = "sebs")), 376);
                      expect_equal(length(get_survey_stations(select.region = "nbs")), 143)})