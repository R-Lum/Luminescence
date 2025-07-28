## example data
set.seed(1234)
object <- rnorm(1000, 100, 10)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_OSLAgeSummary("error"),
               "[plot_OSLAgeSummary()] 'object' should be of class 'RLum.Results'",
               fixed = TRUE)
  expect_error(plot_OSLAgeSummary(set_RLum("RLum.Results", data = list(),
                                           originator = "error")),
               "Object originator 'error' not supported")

  empty <- set_RLum("RLum.Results", originator = NA_character_)
  expect_error(plot_OSLAgeSummary(empty),
               "Object originator 'NA' not supported")
  expect_error(plot_OSLAgeSummary(object, level = 0),
               "'level' should be a positive scalar")
  expect_error(plot_OSLAgeSummary(object, digits = 1.2),
               "'digits' should be a positive integer scalar")
  expect_error(plot_OSLAgeSummary(c(-1, 0, NaN, 10000)),
               "'object' contains missing values")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ##run as numeric
  SW({
  results <- expect_s4_class(plot_OSLAgeSummary(object), "RLum.Results")
  })
  expect_warning(plot_OSLAgeSummary(c(object, Inf), verbose = FALSE),
                 "Inf values found in 'object', removed")

  ##run from S4-class
  object1 <- set_RLum("RLum.Results",
                     data = list(A = object), originator = ".calc_BayesianCentralAgeModel")
  object2 <- set_RLum("RLum.Results",
                      data = list(A = object), originator = ".calc_IndividualAgeModel")

  SW({
  expect_s4_class(plot_OSLAgeSummary(object1), "RLum.Results")
  expect_s4_class(plot_OSLAgeSummary(object2), "RLum.Results")
  })

  ##run with rug option
  expect_silent(plot_OSLAgeSummary(object, verbose = FALSE, rug = TRUE))

  ##check the results
  expect_length(results, 3)
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not(getRversion() >= "4.4.0")

  set.seed(1)

  SW({
  vdiffr::expect_doppelganger("default",
                              plot_OSLAgeSummary(object))
  vdiffr::expect_doppelganger("level",
                              plot_OSLAgeSummary(object, level = 0.3))
  })
})
