## load data
data(ExampleData.DeValues, envir = environment())
df <- ExampleData.DeValues$CA1

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_Histogram("error"),
               "'data' should be of class 'data.frame' or 'RLum.Results'")
  expect_error(plot_Histogram(iris[0, ]),
               "'data' cannot be an empty data.frame")
  expect_error(plot_Histogram(data.frame()),
               "'data' cannot be an empty data.frame")
  expect_error(plot_Histogram(set_RLum("RLum.Results")),
               "'data' cannot be an empty RLum.Results")
  expect_error(plot_Histogram(df, summary = 5),
               "'summary' should be of class 'character'")
  expect_error(plot_Histogram(df, summary.pos = list()),
               "'summary.pos' should be of class 'numeric' or 'character'")
  expect_error(plot_Histogram(df, summary.pos = 5),
               "'summary.pos' should have length 2")
  expect_error(plot_Histogram(df, summary.pos = "error"),
               "'summary.pos' should be one of 'sub', 'left', 'center', 'right'")
  expect_error(plot_Histogram(df, colour = "black"),
               "'colour' should have length 4")
  expect_error(plot_Histogram(df, ylim = c(0, 1)),
               "'ylim' should have length 4")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_silent(plot_Histogram(df))
  expect_silent(plot_Histogram(df, normal_curve = TRUE))

  ## more coverage
  expect_silent(plot_Histogram(df, main = "Title", xlab = "x", ylab = "y",
                               xlim = c(0, 120), ylim = c(0, 0.1, 0, 0.1),
                               pch = 1, breaks = "Sturges",
                               normal_curve = TRUE,
                               summary.pos = c(20, 0.017),
                               summary = c("n", "mean", "mean.weighted",
                                           "median", "sdrel")))

  ## interactive
  expect_silent(plot_Histogram(df, interactive = TRUE,
                               normal_curve = TRUE, se = TRUE))

  ## missing values
  df.na <- df
  df.na[10, 1] <- NA
  expect_output(plot_Histogram(set_RLum("RLum.Results", data = list(df.na)),
                               summary.pos = "bottom"),
                "1 NA value excluded")
  df.na[20, 1] <- NA
  expect_output(plot_Histogram(df.na),
                "2 NA values excluded")
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not(getRversion() >= "4.4.0")

  SW({
  vdiffr::expect_doppelganger("Histogram normal",
                              plot_Histogram(df, normal_curve = TRUE))
  vdiffr::expect_doppelganger("Histogram summary sub",
                              plot_Histogram(df, summary.pos = "sub",
                                             summary = c("n", "serel", "kurtosis")))
  vdiffr::expect_doppelganger("Histogram summary left",
                              plot_Histogram(df, summary.pos = "left",
                                             summary = c("mean", "skewness")))
  })
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 744
  expect_silent(plot_Histogram(df[, 1, drop = FALSE]))
  expect_silent(plot_Histogram(cbind(df[, 1, drop = FALSE], NA)))
})
