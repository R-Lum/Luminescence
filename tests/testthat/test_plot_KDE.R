## load data
data(ExampleData.DeValues, envir = environment())
df <- convert_Second2Gray(ExampleData.DeValues$BT998, c(0.0438,0.0019))

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_KDE("error"),
               "'data' should be of class 'RLum.Results', 'data.frame' or 'numeric'")
  expect_error(plot_KDE(df[0, ]),
               "Input data 1 has 0 rows")
  expect_error(plot_KDE(list()),
               "'data' is an empty list")
  expect_error(expect_warning(plot_KDE(data.frame(a = Inf, b = 1)),
                              "Inf values removed in rows: 1 in data.frame 1"),
               "Your input is empty due to Inf removal")
  expect_error(plot_KDE(df, ylim = c(0, 1)),
               "'ylim' should have length 4")

  expect_warning(plot_KDE(df[1, ]),
                 "Single data point found, no density calculated")
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
  expect_silent(plot_KDE(data = df, sub = "test"))

  ## specify layout
  layout <- get_Layout("default")
  layout$kde$dimension$figure.width <- 100
  layout$kde$dimension$figure.height <- 100
  plot_KDE(data = df, layout = layout)

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
