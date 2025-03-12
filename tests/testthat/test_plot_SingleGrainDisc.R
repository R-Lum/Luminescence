## load data
obj <- set_RLum(class = "RLum.Results",
                data = list(vn_values = rep(x = c(1,2), each = 50)))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_SingleGrainDisc("error"),
               "'object' should be of class 'RLum.Results', 'numeric' or")
  expect_error(plot_SingleGrainDisc(1:50),
               "'object' should have length 100")
  expect_error(plot_SingleGrainDisc(set_RLum(class = "RLum.Results",
                                             data = list(vn_values = rep(NA, 100)))),
               "'object' should contain at least one non-NA value")
  expect_error(plot_SingleGrainDisc(obj, show_coordinates = "error"),
               "'show_coordinates' should be a single logical value")
  expect_error(plot_SingleGrainDisc(obj, show_location_ids = "error"),
               "'show_location_ids' should be a single logical value")
  expect_error(plot_SingleGrainDisc(obj, show_neighbours = "error"),
               "'show_neighbours' should be a single logical value")
  expect_error(plot_SingleGrainDisc(obj, df_neighbours = "error"),
               "'df_neighbours' should be of class 'data.frame'")
  expect_error(plot_SingleGrainDisc(obj, df_neighbours = data.frame()),
               "'df_neighbours' should be a data frame with 3 columns")
  expect_error(plot_SingleGrainDisc(obj, ignore_borders = "error"),
               "'ignore_borders' should be a single logical value")
  expect_error(plot_SingleGrainDisc(obj, show_positioning_holes = "error"),
               "'show_positioning_holes' should be a single logical value")
  expect_error(plot_SingleGrainDisc(obj, str_transform = "error"),
               "'str_transform' should be one of 'sqrt', 'lin' or 'log'")

  expect_error(plot_SingleGrainDisc(obj, df_neighbours = iris[0, 1:3],
                         show_neighbour = TRUE),
               "'show_neighbours' is TRUE but 'df_neighbours' is empty")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_silent(plot_SingleGrainDisc(obj))
  expect_silent(plot_SingleGrainDisc(1:100,
                                     show_location_ids = TRUE,
                                     show_neighbours = TRUE,
                                     show_positioning_holes = FALSE,
                                     legend = TRUE))
  expect_silent(plot_SingleGrainDisc(1:100,
                                     show_neighbours = TRUE,
                                     ignore_borders = TRUE))
  expect_silent(plot_SingleGrainDisc(c(rep(NA, 99), 100),
                                     show_coordinates = TRUE))

  expect_silent(plot_SingleGrainDisc(obj, str_transform = "lin"))
  expect_silent(plot_SingleGrainDisc(obj, str_transform = "log",
                                     col = "cornflowerblue", pch = 16))

  obj.na <- obj
  obj.na@data$vn_values[c(24, 73)] <- NA
  expect_silent(plot_SingleGrainDisc(obj.na, legend = TRUE))
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not(getRversion() >= "4.4.0")

  SW({
  vdiffr::expect_doppelganger("SingleGrainDisc defaults",
                              plot_SingleGrainDisc(obj))
  vdiffr::expect_doppelganger("SingleGrainDisc options",
                              plot_SingleGrainDisc(1:100,
                                                   show_location_ids = TRUE,
                                                   show_neighbours = TRUE,
                                                   show_positioning_holes = FALSE,
                                                   ignore_borders = TRUE,
                                                   show_coordinates = TRUE))
  })
})
