## load data
obj <- set_RLum(class = "RLum.Results",
                data = list(vn_values = rep(x = c(1,2), each = 50)))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_MoranScatterplot("error"),
               "'object' should be of class 'RLum.Results', 'numeric' or")
  expect_error(plot_MoranScatterplot(obj, df_neighbour = "error"),
               "'df_neighbour' should be of class 'data.frame'")
  expect_error(plot_MoranScatterplot(obj, df_neighbour = data.frame()),
               "'df_neighbour' should be a data frame with 3 columns")
  expect_error(plot_MoranScatterplot(obj, show_legend = "error"),
               "'show_legend' should be of class 'logical'")
  expect_error(plot_MoranScatterplot(obj, str_y_def = "error"),
               "'str_y_def' should be one of 'mean_neighbours' or")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_silent(plot_MoranScatterplot(obj))
  expect_silent(plot_MoranScatterplot(1:100,
                                      pch = "show_location_ids"))
  expect_silent(plot_MoranScatterplot(1:100,
                                      pch = "show_n_neighbours"))
  expect_silent(plot_MoranScatterplot(obj, str_y_def = "weighted_sum",
                                      pch = 16))

  ## problematic values for the log transform
  expect_warning(plot_MoranScatterplot(1:100 - 1, log = "xy",
                                       show_legend = FALSE),
                 "x-axis values rescaled because of log transform")
  expect_warning(plot_MoranScatterplot(1:100 - 2, log = "xy",
                                       show_legend = FALSE),
                 "x-axis values rescaled because of log transform")

  obj.na <- obj
  obj.na@data$vn_values[c(24, 73)] <- NA
  expect_warning(plot_MoranScatterplot(obj.na, show_legend = TRUE),
                 "Grain observations removed from plot because no neighbours")
})
