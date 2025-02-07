## load data
obj <- set_RLum(class = "RLum.Results",
                data = list(vn_values = rep(x = c(1,2), each = 50)))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_SingleGrainDisc("error"),
               "'object' should be of class 'RLum.Results', 'numeric' or")
  expect_error(plot_SingleGrainDisc(obj, bo_show_coordinates = "error"),
               "'bo_show_coordinates' should be of class 'logical'")
  expect_error(plot_SingleGrainDisc(obj, bo_show_location_ids = "error"),
               "'bo_show_location_ids' should be of class 'logical'")
  expect_error(plot_SingleGrainDisc(obj, bo_show_legend = "error"),
               "'bo_show_legend' should be of class 'logical'")
  expect_error(plot_SingleGrainDisc(obj, bo_show_neighbours = "error"),
               "'bo_show_neighbours' should be of class 'logical'")
  expect_error(plot_SingleGrainDisc(obj, df_neighbour = "error"),
               "'df_neighbour' should be of class 'data.frame'")
  expect_error(plot_SingleGrainDisc(obj, df_neighbour = data.frame()),
               "'df_neighbour' should be a data frame with 3 columns")
  expect_error(plot_SingleGrainDisc(obj, bo_show_positioning_holes = "error"),
               "'bo_show_positioning_holes' should be of class 'logical'")
  expect_error(plot_SingleGrainDisc(obj, str_transform = "error"),
               "'str_transform' should be one of 'sqrt', 'lin' or 'log'")

  expect_error(plot_SingleGrainDisc(obj, df_neighbour = iris[0, 1:3],
                         bo_show_neighbour = TRUE),
               "'bo_show_neighbours' is TRUE but 'df_neighbour' is empty")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_silent(plot_SingleGrainDisc(obj))
  expect_silent(plot_SingleGrainDisc(1:100,
                          bo_show_location_ids = TRUE,
                          bo_show_neighbours = TRUE,
                          bo_show_positioning_holes = FALSE,
                          bo_show_legend = TRUE))
  expect_silent(plot_SingleGrainDisc(c(rep(NA, 99), 100),
                          bo_show_coordinates = TRUE))

  expect_silent(plot_SingleGrainDisc(obj, str_transform = "lin"))
  expect_silent(plot_SingleGrainDisc(obj, str_transform = "log",
                                     col = "cornflowerblue", pch = 16))

  obj.na <- obj
  obj.na@data$vn_values[c(24, 73)] <- NA
  expect_silent(plot_SingleGrainDisc(obj.na, bo_show_legend = TRUE))
})
