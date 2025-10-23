## load example data (sample UNIL/NB123, see ?ExampleData.Fading)
data("ExampleData.Fading", envir = environment())
fading_data <- ExampleData.Fading$fading.data$IR50

## create artificial object
set.seed(0)
time <- as.POSIXct("2025-09-26 10:26:20 CEST")
ll <- list()
for (x in runif(3, 120, 130)) {
  irr <- set_RLum("RLum.Data.Curve",
                  data = matrix(c(1:x, rep(1, x)), ncol = 2),
                  originator = "read_XSYG2R",
                  recordType = "irradiation (NA)",
                  curveType = "simulated",
                  info = list(startDate = format(time, "%Y%m%d%H%M%S"),
                              position = 1))
  lum <- set_RLum("RLum.Data.Curve",
                  data = matrix(c(1:40, exp(-c(1:40) / x * 10)), ncol = 2),
                  originator = "read_XSYG2R",
                  recordType = "IRSL",
                  curveType = "measured",
                  info = list(startDate = format(time + x + 30, "%Y%m%d%H%M%S"),
                              position = 1))
  time <- time + x + 60
  ll <- c(ll, irr, lum, lum)
}
object <- set_RLum("RLum.Analysis", records = ll, originator = "read_XSYG2R")

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(analyse_FadingMeasurement(object = "test"),
               "'object' should be of class 'RLum.Analysis', 'data.frame' or")
  expect_error(analyse_FadingMeasurement(object = iris[0, ]),
               "'object' cannot be an empty data.frame")
  expect_error(analyse_FadingMeasurement(object = iris[, 0]),
               "'object' cannot be an empty data.frame")
  expect_warning(expect_error(
      analyse_FadingMeasurement(list(fading_data, "test")),
      "No valid records in 'object' left"),
      "2 unsupported records removed")
  expect_error(analyse_FadingMeasurement(cbind(fading_data, fading_data[, 1])),
               "When 'object' is a data.frame, the number of columns must be")
  expect_error(analyse_FadingMeasurement(data.frame(NA, 1:5, 1:5)),
               "After NA removal, nothing is left from the data set")

  expect_error(analyse_FadingMeasurement(object, signal.integral = 1:2,
                                         background.integral = 2),
               "'signal.integral' and 'background.integral' overlap")
  expect_error(analyse_FadingMeasurement(object[-3], signal.integral = 1:2,
                                         background.integral = 3:40,
                                         structure = c("Lx", "Tx")),
               "The number of Lx curves (3) differs from the number of Tx curves (2)",
               fixed = TRUE)

  ## check various for t_star
  ## stop t_star
  expect_error(analyse_FadingMeasurement(fading_data, t_star = "error"),
               "'t_star' should be one of 'half', 'half_complex', 'end' or a function")

  ## wrong originator
  psl.file <- system.file("extdata/DorNie_0016.psl", package = "Luminescence")
  SW({
  expect_message(expect_null(analyse_FadingMeasurement(read_PSL2R(psl.file))),
                 "Error: Unknown or unsupported originator, NULL returned")

  ## no irradiation steps
  xsyg.file <- system.file("extdata/XSYG_file.xsyg", package = "Luminescence")
  expect_error(analyse_FadingMeasurement(read_XSYG2R(xsyg.file, fastForward = TRUE)),
               "No irradiation times could be retrieved, check that 'object'")
  })
})

test_that("test functionality", {
  testthat::skip_on_cran()

  ## run routine analysis
  SW({
  expect_s4_class(analyse_FadingMeasurement(
    fading_data,
    plot = TRUE,
    verbose = TRUE,
    n.MC = 10), class = "RLum.Results")
  })

  ## test merging of objects if combined in a list
  ## this crashed before and was fixed
  expect_s4_class(merge_RLum(
    list(analyse_FadingMeasurement(
      fading_data[1,],
      plot = FALSE,
      verbose = FALSE,
      n.MC = 10),
    analyse_FadingMeasurement(
      fading_data[1:10,],
      plot = FALSE,
      verbose = FALSE,
      n.MC = 10))), "RLum.Results")

  expect_warning(analyse_FadingMeasurement(fading_data,
                                           plot = TRUE, plot.single = TRUE,
                                           verbose = FALSE,
                                           n.MC = 10),
                 "'plot.single' is deprecated, use 'plot_singlePanels'")

  ## more coverage
  data.inf <- fading_data
  data.inf$LxTx[24] <- Inf
  expect_s4_class(analyse_FadingMeasurement(
    data.inf,
    plot = FALSE,
    verbose = FALSE,
    n.MC = 10), class = "RLum.Results")
})

test_that("test XSYG file fading data", {
  testthat::skip_on_cran()

  SW({
  expect_s4_class(analyse_FadingMeasurement(
    object,
    signal.integral = 1:2,
    background.integral = 10:40,
    structure = "Lx"
  ), "RLum.Results")
  })

  SW({
  expect_s4_class(analyse_FadingMeasurement(
    object,
    signal.integral = 1:2,
    t_star = "end",
    background.integral = 10:40,
    structure = "Lx",
    plot = FALSE
  ), "RLum.Results")
  })

  obj.sub <- get_RLum(object, record.id = 1:2, drop = FALSE)
  expect_message(expect_null(analyse_FadingMeasurement(obj.sub)),
                 "After irradiation step removal not enough curves are left")

  SW({
  expect_warning(analyse_FadingMeasurement(object[-c(3,6,9)], signal.integral = 1:2,
                                           background.integral = 3,
                                           structure = "Lx"),
                 "Number of background channels for Lx < 25")

  object@records[[3]]@data <- object@records[[3]]@data[1:10, ]
  expect_warning(analyse_FadingMeasurement(object, signal.integral = 1:2,
                                           background.integral = 3:40),
                 "Skipped the following samples because Lx and Tx have different sizes: 1")
  })

  obj.mod <- object
  obj.mod@records[[6]]@data <- obj.mod@records[[6]]@data[1:10, ]
  obj.mod@records[[9]]@data <- obj.mod@records[[9]]@data[1:10, ]
  expect_error(analyse_FadingMeasurement(obj.mod, signal.integral = 1:2,
                                         background.integral = 3:40),
               "No curves left after removing those with different Lx and Tx sizes")
})

test_that("test BIN file while fading data", {
  testthat::skip_on_cran()

  data(ExampleData.BINfileData, envir = environment())
  d1 <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1)
  expect_error(analyse_FadingMeasurement(d1),
               "BIN-file has version 03, but only versions from 05 on are supported")

  SW({
  d2 <- read_BIN2R(test_path("_data/BINfile_V5.binx"), verbose = FALSE,
                   fastForward = TRUE)
  })
  expect_output(analyse_FadingMeasurement(d2, signal.integral = 1:2,
                                          background.integral = 10:30,
                                          plot = TRUE))
  expect_message(expect_null(
      analyse_FadingMeasurement(d2, signal.integral = 1:2,
                                background.integral = 10:30,
                                structure = c("Lx", "Tx", "Lx"))),
      "Error: 'structure' can only be 'Lx' or c('Lx', 'Tx'), NULL returned",
      fixed = TRUE)

  ## more coverage
  analyse_FadingMeasurement(d2, signal.integral = 1:2,
                            background.integral = 10:30,
                            signal.integral.Tx = 2,
                            background.integral.Tx = 5:30,
                            plot_singlePanels = 2:3, ylim = c(0.1, 1.1),
                            background.count.distribution = "poisson",
                            sig0 = 2, verbose = FALSE, plot = TRUE)

  d2[[2]]@records[[1]]@info$TIMESINCEIRR <- -1
  expect_message(expect_warning(expect_null(
      analyse_FadingMeasurement(d2, signal.integral = 1:2,
                                background.integral = 10:30,
                                verbose = TRUE)),
      "removed 2 records with negative 'time since irradiation'"),
      "After record removal nothing is left from the data set, NULL returned")
  suppressWarnings( # repeated warning about negative time since irradiation
  expect_warning(expect_s4_class(
      analyse_FadingMeasurement(d2, signal.integral = 1:2,
                                background.integral = 10:40,
                                structure = "Lx", verbose = FALSE),
      "RLum.Results"),
      "removed 1 records with negative 'time since irradiation'")
  )

  fd.mod <- fading_data
  fd.mod[6, 1] <- 0.712
  expect_output(analyse_FadingMeasurement(fd.mod, n.MC = 10))
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 558
  df <- data.frame(LxTx = c(1, 0.9879, 0.9865, 0.9872, 0.9759, 0.9978, 0.9879, 0.98372),
                   LxTxError = rep(0, 8),
                   timeSinceIrr = c(2516, 41353, 50357, 140342, 1040044, 2516, 41360, 50360))
  res <- analyse_FadingMeasurement(df, n.MC = 10, plot = FALSE, verbose = FALSE)
  expect_false(is.nan(res$rho_prime$MEAN))

  ## issue 616
  expect_output(analyse_FadingMeasurement(df[1:2, ]))
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  SW({
  set.seed(1)
  vdiffr::expect_doppelganger("default",
                              analyse_FadingMeasurement(fading_data,
                                                        n.MC = 10))
  vdiffr::expect_doppelganger("panel 3",
                              analyse_FadingMeasurement(fading_data,
                                                        plot_singlePanels = 3,
                                                        n.MC = 10))
  vdiffr::expect_doppelganger("panel 4",
                              analyse_FadingMeasurement(fading_data,
                                                        plot_singlePanels = 4,
                                                        n.MC = 10))
  vdiffr::expect_doppelganger("lx half_complex",
                              analyse_FadingMeasurement(object,
                                                        signal.integral = 1:2,
                                                        background.integral = 10:40,
                                                        structure = "Lx",
                                                        t_star = "half_complex",
                                                        n.MC = 10))
  vdiffr::expect_doppelganger("lx tx identity",
                              analyse_FadingMeasurement(object,
                                                        signal.integral = 1:2,
                                                        background.integral = 10:40,
                                                        structure = c("Lx", "Tx"),
                                                        t_star = identity,
                                                        n.MC = 10))
  })
})
