#' Function to interpolate a 2D grid of environmental data set using ordinary kriging
#' 
#' @param x Data frame with columns for an environmental variable, vessel, cruise, haul, stationid, latitude, longitude, and cdepth.
#' @param extrapolation_2d_grid A data frame containing latitude, longitude and cdepth columns. Default NA automatically loads extrapolation grid.
#' @param par_name Character vector indicating the name of the variable column.
#' @param sel_year Numeric vector indicating the survey year.
#' @param vgm_init List of initial parameters to pass to gstat::vgm function
#' @param log_transform Should variables be log_transformed prior to subsequent transformation?
#' @param transform What transformation method should be applied? Options "none" = No transformation; nst" = normal score transform based on cdf using akgfmaps::normal_transform() with backtransformation using akgfmaps::backtransform_normal(), based on GSLIB nscore and backtr; "boxcox" - Box-Cox transformation and back transformation; "z" = z-score transform to mean zero, variance one without using cdf.
#' @param nst_options List with options (tails, make_plot) for backtransformation when transform = "nst". See documentation for akgfmaps::backtransform_normal().
#' @param nmax Numeric. Maximum number of data points to use for kriging (default = 500.
#' @param save_csv Logical. Should the output be saved to a csv file?
#' @param suffix Character vector added to the end of the filename if save_csv is TRUE. 
#' @export

make_2d_grid <- function(x,
                         extrapolation_2d_grid = NA,
                         par_name, 
                         sel_year,
                         vgm_init, 
                         log_transform = FALSE, 
                         transform = "none",
                         nst_options = list(tails = "none", make_plot = FALSE),
                         nmax = 500,
                         make_variogram = FALSE,
                         save_csv = TRUE,
                         suffix = "") {
  
# x = dplyr::filter(kdz_nepheloid_df, cruise == cruises[i], updown == "Downcast", haul%%2 == 0)
# extrapolation_2d_grid = extrap_grid
# par_name = "nepheloid_index"
# sel_year = floor(cruises/100)
# vgm_init = list(psill = 1, mod = "Bes", nugget = 0.1)
# log_transform = FALSE
# transform = "nst"
# nst_options = list(tails = "none", make_plot = FALSE)
# nmax = Inf
# make_variogram = FALSE
# save_csv = TRUE
# suffix = ""
  
  # Load output grid locations ---------------------------------------------------------------------
  if(is.na(extrapolation_2d_grid)[1]) {
    extrapolation_2d_grid <- read.csv(file = "./output/extrapolation_2d_grid.csv", stringsAsFactors = FALSE)
  }
  
  coordinates(extrapolation_2d_grid) <- ~longitude + latitude
  
  x$raw_var <- eval(parse(text = paste0("x$", par_name)))
  x$trans_var <- x$raw_var
  
  # Transformation ---------------------------------------------------------------------------------
  source(here::here("R", "transform_gau_fn.R")) #./R/transform_gau_fn.R")
  
  if(transform != "none") {
    x_trans_list <- transform_gau_fn(x = x, 
                                     log_transform = log_transform, 
                                     transform = transform)
    x <- x_trans_list$x
    scale_vars <- x_trans_list$scale_vars
    bc_lambda <- x_trans_list$bc_lambda
  }
  
  # Histogram if raw and transformed variables -----------------------------------------------------
  hist_raw <- ggplot() + 
    geom_histogram(aes(x = x$raw_var)) +
    scale_x_continuous(name = paste0("Raw ", sel_year, " ", par_name))
  hist_trans <- ggplot() + 
    geom_histogram(aes(x = x$trans_var)) +
    scale_x_continuous(name = paste0("Transform ", sel_year, " ", par_name))
  
  print(cowplot::plot_grid(hist_raw,
                           hist_trans,
                           nrow = 2))
  
  dat <- x
  
  coordinates(dat) <- ~longitude + latitude
  
  # Fit variogram ----
  bes_variogram <- variogram(trans_var ~ 1, dat)
  bes_variogram <- variogram(trans_var ~ 1, dat, 
                             cutoff = max(bes_variogram$dist), 
                             width = max(bes_variogram$dist)/50)
  
  bes_mod <- vgm(psill = vgm_init$psill, 
                 model = "Bes",
                 range = bes_variogram$dist[10], 
                 nugget = vgm_init$nugget)
  bes_mod <- fit.variogram(bes_variogram, 
                           bes_mod, 
                           fit.sills = TRUE, 
                           fit.ranges = TRUE, 
                           fit.kappa = TRUE)
  
  # Krige ----
  start_time <- Sys.time()
  krige_bes <- gstat(formula = trans_var ~ 1, 
                     locations = dat, 
                     model = bes_mod, 
                     nmax = nmax)
  output_2d_grid <- predict(krige_bes, 
                            newdata = extrapolation_2d_grid)
  
  # Back transformation ----
  if(transform == "boxcox") {
    print("Box-cox back-transform")
    output_2d_grid$var1.pred <- forecast::InvBoxCox(output_2d_grid$var1.pred, lambda = bc_lambda)
  }
  
  # Normal score transform (based on GSLIB normal score transformation)
  if(transform == "nst") {
    print("Normal score back-transform")
    output_2d_grid$var1.pred <- akgfmaps::backtransform_normal(scores = output_2d_grid$var1.pred,
                                                               z_score = scale_vars,
                                                               tails = nst_options$tails,
                                                               make_plot = nst_options$make_plot)
  }
  
  if(transform == "z") {
    print("Z back-transform")
    output_2d_grid$var1.pred <- output_2d_grid$var1.pred * attr(scale_vars, "scaled:scale") + attr(scale_vars,"scaled:center")
  }
  
  if(log_transform) {
    print("Log back-transform")
    output_2d_grid$var1.pred <- exp(output_2d_grid$var1.pred)
  }
  
  # Generate 2d_grid ----
  output_2d_grid <- sf::st_as_sf(output_2d_grid)
  
  output_2d_grid2 <- cbind(as.data.frame(output_2d_grid) %>%
                             dplyr::select(-geometry), 
                           as.data.frame(st_coordinates(output_2d_grid))) %>%
    dplyr::rename(longitude = X,
                  latitude = Y)
  
  # Histogram of output distribution ---------------------------------------------------------------
  hist_out <- ggplot() + 
    geom_histogram(aes(x = output_2d_grid2$var1.pred)) +
    scale_x_continuous(name = paste0("Output ", sel_year, " ", par_name))
  
  if(!dir.exists("./plots/diagnostics/")) {
    dir.create("./plots/diagnostics/", par_name)
  }
  
  png(file = paste0("./plots/diagnostics/2d_grid_", par_name, "_", sel_year, "_fit_transform.png"),
      width = 7, 
      height = 7,
      units = "in",
      res = 300)
  print(cowplot::plot_grid(hist_raw,
                           hist_trans,
                           hist_out,
                           nrow = 3))
  dev.off()
  
  # Rename output columns --------------------------------------------------------------------------
  names(output_2d_grid2)[1] <- par_name
  names(output_2d_grid2)[2] <- paste0(par_name, ".var")
  
  # Create directory to store grids ----
  if(!dir.exists("./output/grids/")) {
    dir.create("./output/grids/", par_name)
  }
  
  if(!dir.exists(paste0("./output/grids/", par_name))) {
    dir.create(paste0("./output/grids/", par_name))
  }
  
  # Store grid as a .csv
  if(save_csv) {
    write.csv(output_2d_grid2, 
              file = paste0("./output/grids/", par_name, "/2d_grid_", par_name, "_", sel_year, suffix, ".csv"), 
              row.names = FALSE)
  }
  
  end_time <- Sys.time()
  print(difftime(end_time, start_time, units = "min"))
  
  return(output_2d_grid2)
  
}