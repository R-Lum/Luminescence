data(ExampleData.DeValues, envir = environment())
df <- ExampleData.DeValues$CA1

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_Histogram("error"),
               "'data' should be of class 'data.frame' or 'RLum.Results'")
  expect_error(plot_Histogram(iris[0, ]),
               "'data' contains no data")
  expect_error(plot_Histogram(data.frame()),
               "'data' contains no data")
  expect_error(plot_Histogram(set_RLum("RLum.Results")),
               "'data' contains no data")
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
  expect_silent(plot_Histogram(df, summary.pos = "topleft"))
  expect_silent(plot_Histogram(df, summary.pos = "top"))
  expect_silent(plot_Histogram(df, summary.pos = "topright"))
  expect_silent(plot_Histogram(df, summary.pos = "left"))
  expect_silent(plot_Histogram(df, summary.pos = "center"))
  expect_silent(plot_Histogram(df, summary.pos = "right"))
  expect_silent(plot_Histogram(df, summary.pos = "bottomleft"))
  expect_silent(plot_Histogram(df, summary.pos = "bottom"))
  expect_silent(plot_Histogram(df, summary.pos = "bottomright"))

  ## interactive
  expect_silent(plot_Histogram(df, interactive = TRUE,
                               normal_curve = TRUE, se = TRUE))

  ## missing values
  df.na <- df
  df.na[10, 1] <- NA
  expect_output(plot_Histogram(df.na),
                "1 NA value excluded")
  df.na[20, 1] <- NA
  expect_output(plot_Histogram(df.na),
                "2 NA values excluded")
})
