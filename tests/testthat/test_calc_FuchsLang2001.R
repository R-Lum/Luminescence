## load data
data(ExampleData.DeValues, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_FuchsLang2001("error"),
               "[calc_FuchsLang2001()] 'data' should be of class 'data.frame'",
               fixed = TRUE)
  expect_error(calc_FuchsLang2001(data.frame()),
               "'data' cannot be an empty data.frame")
  expect_error(calc_FuchsLang2001(iris[, 1, drop = FALSE]),
               "'data' should have 2 columns")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6

  expect_snapshot_RLum(temp <- calc_FuchsLang2001(data = ExampleData.DeValues$BT998,
                                          cvThreshold = 5,
                                          plot = FALSE,
                                          verbose = FALSE),
                       tolerance = snapshot.tolerance)

  ## using an RLum.Results object as input
  SW({
  expect_snapshot_RLum(calc_FuchsLang2001(data = temp, startDeValue = 24,
                                          plot = FALSE),
                       tolerance = snapshot.tolerance)
  })
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  SW({
  vdiffr::expect_doppelganger("default",
                              calc_FuchsLang2001(ExampleData.DeValues$BT998))
  vdiffr::expect_doppelganger("cvDefault startDeValue",
                              calc_FuchsLang2001(ExampleData.DeValues$BT998,
                                                 cvDefault = 3,
                                                 startDeValue = 5))
  })
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 818
  expect_silent(calc_FuchsLang2001(data.frame(ED = c(rep(0, 4), 10),
                                              ED_Error = rnorm(5) + 1),
                                   verbose = FALSE))

  ## issue 917
  expect_output(calc_FuchsLang2001(data.frame(ED = NA,
                                              ED_Error = rnorm(5) + 1),
                                   verbose = FALSE),
                "need finite 'xlim' values")
})
