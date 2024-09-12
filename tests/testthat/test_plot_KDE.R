data(ExampleData.DeValues, envir = environment())
df <- Second2Gray(ExampleData.DeValues$BT998, c(0.0438,0.0019))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_KDE("error"),
               "Input data must be one of 'data.frame', 'RLum.Results' or")
  expect_error(plot_KDE(df[0, ]),
               "Input data 1 has 0 rows")
  expect_error(plot_KDE(list()),
               "'data' is an empty list")
  expect_error(expect_warning(plot_KDE(data.frame(a = Inf, b = 1)),
                              "Inf values removed in rows: 1 in data.frame 1"),
               "Your input is empty due to Inf removal")
  expect_error(plot_KDE(df, ylim = c(0, 1)),
               "'ylim' must be a vector of length 4")

  expect_warning(plot_KDE(df[1, ]),
                 "Single data point found, no density calculated")

  ## deprecated arguments
  expect_warning(plot_KDE(df, centrality = TRUE),
                 "Argument 'centrality' no longer supported")
  expect_warning(plot_KDE(df, dispersion = TRUE),
                 "Argument 'dispersion' no longer supported")
  expect_warning(plot_KDE(df, polygon.col = TRUE),
                 "Argument 'polygon.col' no longer supported")
  expect_warning(plot_KDE(df, weights = TRUE),
                 "Argument 'weights' no longer supported")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## create plot straightforward
  expect_silent(plot_KDE(data = df))

  ## more coverage
  expect_silent(plot_KDE(data = df, summary = "n", summary.pos = c(105, 0.07),
                         main = "Title", mtext = "Subtitle",
                         xlab = "x", ylab = "y", layout = "default", log = "x",
                         xlim = c(100, 180), ylim = c(0, 0.07, 0, 0.01),
                         col = 2, lty = 2, lwd = 2, cex = 1))
  expect_silent(plot_KDE(data = df, summary.pos = "topleft"))
  expect_silent(plot_KDE(data = df, summary.pos = "top"))
  expect_silent(plot_KDE(data = df, summary.pos = "topright"))
  expect_silent(plot_KDE(data = df, summary.pos = "left"))
  expect_silent(plot_KDE(data = df, summary.pos = "center"))
  expect_silent(plot_KDE(data = df, summary.pos = "right"))
  expect_silent(plot_KDE(data = df, summary.pos = "bottomleft"))
  expect_silent(plot_KDE(data = df, summary.pos = "bottom"))
  expect_silent(plot_KDE(data = df, summary.pos = "bottomright"))

  ## numeric vector
  expect_silent(plot_KDE(df[, 1]))

  ## single-column data.frame
  expect_silent(plot_KDE(df[, 1, drop = FALSE]))

  ## RLum.Results object
  expect_silent(plot_KDE(calc_CommonDose(df, plot = FALSE, verbose = FALSE)))

  ## infinite values
  df.inf <- df
  df.inf[9, 1] <- Inf
  expect_warning(plot_KDE(df.inf),
                 "Inf values removed in rows: 9 in data.frame 1")
  df.inf[2, 2] <- Inf
  expect_warning(plot_KDE(df.inf),
                 "Inf values removed in rows: 2, 9 in data.frame 1")

  ## missing values
  df.na <- df
  df.na[2, 1] <- NA
  expect_message(plot_KDE(df.na),
                 "1 NA value excluded from data set 1")
  df.na[3, 1] <- NA
  expect_message(plot_KDE(df.na),
                 "2 NA values excluded from data set 1")
})
