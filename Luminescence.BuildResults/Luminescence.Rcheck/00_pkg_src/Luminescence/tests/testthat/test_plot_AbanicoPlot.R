data(ExampleData.DeValues, envir = environment())
ExampleData.DeValues <- ExampleData.DeValues$CA1

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_AbanicoPlot(data = "error"),
               "Input data format must be 'data.frame' or 'RLum.Results'")
  expect_error(plot_AbanicoPlot(ExampleData.DeValues[, 1, drop = FALSE]),
               "Data set (1) has fewer than 2 columns: data without errors",
               fixed = TRUE)

  expect_message(expect_null(plot_AbanicoPlot(ExampleData.DeValues[0, ])),
                 "Error: Nothing plotted, your data set is empty")

  expect_warning(expect_message(
      expect_null(plot_AbanicoPlot(ExampleData.DeValues[1, ])),
      "Error: After removing invalid entries, nothing is plotted"),
      "Data sets 1 are found to be empty or consisting of only 1 row")

  expect_error(plot_AbanicoPlot(ExampleData.DeValues, plot = FALSE),
               "'plot.ratio' must be a positive scalar")
  expect_error(plot_AbanicoPlot(ExampleData.DeValues, xlab = "x"),
               "'xlab' must have length 2")
  expect_error(plot_AbanicoPlot(ExampleData.DeValues, z.0 = "error"),
               "Value for 'z.0' not supported")
  expect_error(plot_AbanicoPlot(ExampleData.DeValues, dispersion = "error"),
               "Measure of dispersion not supported")

  ## zero-error values
  data.zeros <- ExampleData.DeValues
  data.zeros[2, 2] <- 0
  expect_warning(plot_AbanicoPlot(data.zeros, grid.col = c(1, 2)),
                 "Values with zero errors cannot be displayed and were removed")
  data.zeros[, 2] <- 0
  expect_error(plot_AbanicoPlot(data.zeros),
               "Data set contains only values with zero errors")
})

test_that("Test examples from the example page", {
  testthat::skip_on_cran()

  ## plot the example data straightforward
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues))

  ## now with linear z-scale
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   log.z = FALSE))

  ## now with output of the plot parameters
  expect_type(plot_AbanicoPlot(data = ExampleData.DeValues,
                            output = TRUE), "list")

  ## now with adjusted z-scale limits
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   zlim = c(10, 200)))

  ## now with adjusted x-scale limits
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   xlim = c(0, 20)))

  ## now with rug to indicate individual values in KDE part
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   rug = TRUE))

  ## now with a smaller bandwidth for the KDE plot
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   bw = 0.04))

  ## now with a histogram instead of the KDE plot
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   hist = TRUE,
                   kde = FALSE))

  ## now with a KDE plot and histogram with manual number of bins
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   hist = TRUE,
                   breaks = 20))

  ## now with a KDE plot and a dot plot
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   dots = TRUE))

  ## now with user-defined plot ratio
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   plot.ratio = 0.5))

  ## now with user-defined central value
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   z.0 = 70))

  ## now with median as central value
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   z.0 = "median"))

  ## now with the 17-83 percentile range as definition of scatter
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   z.0 = "median",
                   dispersion = "p17"))

  ## now with user-defined green line for minimum age model
  CAM <- calc_CentralDose(ExampleData.DeValues,
                          plot = FALSE, verbose = FALSE)

  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   line = CAM,
                   line.col = "darkgreen",
                   line.label = "CAM"))

  ## now create plot with legend, colour, different points and smaller scale
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   legend = "Sample 1",
                   col = "tomato4",
                   bar.col = "peachpuff",
                   pch = "R",
                   cex = 0.8))

  ## now without 2-sigma bar, polygon, grid lines and central value line
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   bar.col = FALSE,
                   polygon.col = FALSE,
                   grid.col = FALSE,
                   y.axis = FALSE,
                   lwd = 0))

  ## now with direct display of De errors, without 2-sigma bar
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   bar.col = FALSE,
                   ylab = "",
                   y.axis = FALSE,
                   error.bars = TRUE))

  ## now with user-defined axes labels
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   xlab = c("Data error (%)",
                            "Data precision"),
                   ylab = "Scatter",
                   zlab = "Equivalent dose [Gy]"))

  ## now with minimum, maximum and median value indicated
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   stats = c("min", "max", "median")))

  ## now with a brief statistical summary as subheader
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   summary = c("n", "in.2s")))

  ## now with another statistical summary
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   summary = c("mean.weighted", "median"),
                   summary.pos = "topleft"))

  ## now a plot with two 2-sigma bars for one data set
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   bar = c(30, 100)))

  ## now the data set is split into sub-groups, one is manipulated
  data.1 <- ExampleData.DeValues[1:30,]
  data.2 <- ExampleData.DeValues[31:62,] * 1.3
  data.3 <- list(data.1, data.2)

  ## now the two data sets are plotted in one plot
  expect_silent(plot_AbanicoPlot(data = data.3))

  ## now with some graphical modification
  expect_silent(plot_AbanicoPlot(data = data.3,
                   z.0 = "median",
                   col = c("steelblue4", "orange4"),
                   bar.col = c("steelblue3", "orange3"),
                   polygon.col = c("steelblue1", "orange1"),
                   pch = c(2, 6),
                   angle = c(30, 50),
                   summary = c("n", "in.2s", "median")))

  ## create Abanico plot with predefined layout definition
  expect_silent(plot_AbanicoPlot(data = ExampleData.DeValues,
                   layout = "journal"))

  ## now with predefined layout definition and further modifications
  expect_silent(plot_AbanicoPlot(data = data.3,
                   z.0 = "median",
                   layout = "journal",
                   col = c("steelblue4", "orange4"),
                   bar.col = adjustcolor(c("steelblue3", "orange3"),
                                         alpha.f = 0.5),
                   polygon.col = c("steelblue3", "orange3")))

  ## for further information on layout definitions see documentation
  ## of function get_Layout()

  ## now with manually added plot content
  ## create empty plot with numeric output
  expect_type(plot_AbanicoPlot(data = ExampleData.DeValues,
                         pch = NA,
                         output = TRUE), "list")

  ## interactive mode
  expect_silent(plot_AbanicoPlot(ExampleData.DeValues, interactive = TRUE))
})

test_that("more coverage", {
  testthat::skip_on_cran()

  ## weights
  expect_silent(plot_AbanicoPlot(ExampleData.DeValues, weights = FALSE,
                                 boxplot = TRUE, frame = 2))
  suppressWarnings( # additional warning on weights not summing to 1
  expect_warning(plot_AbanicoPlot(ExampleData.DeValues, weights = TRUE,
                                  rotate = TRUE, line = 1,
                                  grid.col = c(1, 2)),
                 "Selecting bandwidth *not* using 'weights'",
                 fixed = TRUE)
  )

  ## negative values
  data.neg <- ExampleData.DeValues
  data.neg[1, 1] <- -1
  expect_silent(plot_AbanicoPlot(data.neg, z.0 = "mean", dispersion = "sd",
                                 boxplot = TRUE, frame = 3,
                                 main = "Title", sub = "Subtitle"))

  ## missing values
  data.na <- ExampleData.DeValues
  data.na[1, 2] <- NA
  expect_message(plot_AbanicoPlot(data.na, rotate = TRUE, boxplot = TRUE,
                                  hist = TRUE, error.bars = TRUE, dots = TRUE,
                                  rug = TRUE, y.axis = FALSE, stats = "min",
                                  legend = "legend", legend.pos = "bottomleft",
                                  summary.pos = "bottomright", log.z = FALSE,
                                  xlab = c("x1", "x2", "x3"), lty = 2,
                                  dispersion = "2sd",
                                  at = seq(20, 120, nrow(data.na) - 1)),
                 "Data set (1): 1 NA value excluded",
                 fixed = TRUE)
})
