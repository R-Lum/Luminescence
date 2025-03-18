## load data
set.seed(1)
density <- density(rnorm(100, mean = 450, sd = 20))
filter1 <- matrix(c(density$x, density$y / max(density$y)), ncol = 2)
filter2 <- matrix(c(200:799, rep(c(0, 0.8, 0), each = 200)), ncol = 2)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_FilterCombinations("error"),
               "'filters' should be of class 'list'")
  expect_error(plot_FilterCombinations(list()),
               "'filters' cannot be an empty list")
  expect_error(plot_FilterCombinations(list("error")),
               "All elements of 'filters' should be of class 'data.frame'")
  expect_error(plot_FilterCombinations(list(data.frame(a = c(100, 200),
                                                       b = c(0.2, 1.1)))),
               "Transmission values > 1 found, check your data")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_silent(plot_FilterCombinations(filters = list(filter1, filter2)))
  expect_silent(plot_FilterCombinations(filters = list(filter1, filter2),
                                        interactive = TRUE))
  expect_silent(plot_FilterCombinations(list(list(filter1))))

  ## filter thickness and reflection factor provided
  expect_silent(plot_FilterCombinations(list(list(filter1, d = 2),
                                             list(filter2, d = 2, P = 0.9))))
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not(getRversion() >= "4.4.0")

  SW({
  vdiffr::expect_doppelganger("FilterCombinations defaults",
                              plot_FilterCombinations(list(filter1, filter2)))
  vdiffr::expect_doppelganger("FilterCombinations values",
                              plot_FilterCombinations(list(
                                  list(filter1, d = 2, P = 0.8),
                                  Rectangle = list(filter2, d = 2, P = 0.6))))
  })
})
