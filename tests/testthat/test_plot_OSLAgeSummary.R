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
               "'object' was created by an unsupported function")

  empty <- set_RLum("RLum.Results", originator = NA_character_)
  expect_error(plot_OSLAgeSummary(empty),
               "'object' was created by an unsupported function")
  expect_error(plot_OSLAgeSummary(object, level = 0),
               "'level' should be a single positive value")
  expect_error(plot_OSLAgeSummary(object, digits = 1.2),
               "'digits' should be a single positive integer value")
  expect_error(plot_OSLAgeSummary(c(-1, 0, NaN, 10000)),
               "'object' contains missing values")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_warning(plot_OSLAgeSummary(c(object, Inf), verbose = FALSE),
                 "Inf values found in 'object', removed")

  ##run from S4-class
  object1 <- set_RLum("RLum.Results",
                     data = list(Ages = object), originator = "combine_De_Dr")
  object2 <- set_RLum("RLum.Results",
                      data = list(A = object), originator = ".calc_IndividualAgeModel")

  SW({
  expect_s4_class(plot_OSLAgeSummary(object1, polygon_density = 5),
                  "RLum.Results")
  expect_s4_class(plot_OSLAgeSummary(object2, polygon_density = 5),
                  "RLum.Results")
  })
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  expect_snapshot_RLum(plot_OSLAgeSummary(object, verbose = FALSE,
                                          polygon_density = 5),
                       tolerance = 1.5e-6)
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  set.seed(1)

  SW({
  vdiffr::expect_doppelganger("default",
                              plot_OSLAgeSummary(object))
  vdiffr::expect_doppelganger("level",
                              plot_OSLAgeSummary(object, level = 0.3))
  vdiffr::expect_doppelganger("rug",
                              plot_OSLAgeSummary(object, rug = TRUE))
  vdiffr::expect_doppelganger("polygon density",
                              plot_OSLAgeSummary(object, polygon_density = 5))
  })
})
