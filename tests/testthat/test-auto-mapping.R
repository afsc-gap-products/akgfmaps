library(akgfmaps)

testthat::test_that("Test auto-mapping functions interpolation", 
                    {test_map <- suppressWarnings(make_idw_map(x = akgfmaps:::YFS2017, grid.cell = c(0.2, 0.2)));
                    test_labs <- suppressWarnings(add_map_labels(x = test_map));
                    test_resize <- suppressWarnings(akgfmaps::change_text_size(x = test_map, size.mult = 2));
                    test_raster_mask <- rasterize_and_mask(raster::raster(nrows = 100, 
                                                             ncols = 100, 
                                                             xmn = test_map$map_layers$plot.boundary$x[1],
                                                             xmx = test_map$map_layers$plot.boundary$x[2],
                                                             ymn = test_map$map_layers$plot.boundary$y[1],
                                                             ymx = test_map$map_layers$plot.boundary$y[2], 
                                                             vals = rnorm(1e4)), 
                                                           amask = test_map$map_layers$survey.area);
                    expect_equal(test_map$n.breaks[1], 6);
                    expect_true(length(test_labs$map_layers$place.labels) > 2);
                    expect_true(sum(!is.na(test_raster_mask@data@values)) == 3813);
                    expect_true(is.list(suppressWarnings(change_fill_color(x = test_map, new.scheme = "blue"))));
                    expect_true(test_resize$plot$theme$axis.text$size/test_map$plot$theme$axis.text$size == 2.25);
                    })