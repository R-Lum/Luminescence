## load example data (sample UNIL/NB123, see ?ExampleData.Fading)
data("ExampleData.Fading", envir = environment())
fading_data <- ExampleData.Fading$fading.data$IR50

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(analyse_FadingMeasurement(object = "test"),
               "'object' should be of class 'RLum.Analysis', 'data.frame' or")
  expect_warning(expect_error(
      analyse_FadingMeasurement(list(fading_data, "test")),
      "No valid records in 'object' left"),
      "2 unsupported records removed")
  expect_error(analyse_FadingMeasurement(cbind(fading_data, fading_data[, 1])),
               "if you provide a data.frame as input, the number of columns")

  ## check various for t_star
  ## stop t_star
  expect_error(analyse_FadingMeasurement(fading_data, t_star = "error"),
               "'t_star' should be one of 'half', 'half_complex', 'end' or a function")
})

test_that("general test", {
  testthat::skip_on_cran()

  ## run routine analysis
  SW({
  expect_s4_class(analyse_FadingMeasurement(
    fading_data,
    plot = TRUE,
    verbose = TRUE,
    n.MC = 10), class = "RLum.Results")
  })

  ##no plot not verbose
  expect_s4_class(analyse_FadingMeasurement(
    fading_data,
    plot = FALSE,
    verbose = FALSE,
    n.MC = 10), class = "RLum.Results")

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

})

test_that("test XSYG file fading data", {
  testthat::skip_on_cran()

  # Create artificial object ------------------------------------------------
  l <- list()
  time <- 0
  set.seed(0)
  for(x in runif(3, 120,130)) {
    ## set irr
    irr  <-
      set_RLum(
        "RLum.Data.Curve",
        data = matrix(c(1:x, rep(1,x)), ncol = 2),
        originator = "read_XSYG2R",
        recordType = "irradiation (NA)",
        curveType = "simulated",
        info = list(
          startDate = format(Sys.time() + time, "%Y%m%d%H%M%S"),
          position = 1)
      )

    ## set lum
    lum  <-
      set_RLum(
        "RLum.Data.Curve",
        data = matrix(c(1:40, exp(-c(1:40)/ x * 10)), ncol = 2),
        originator = "read_XSYG2R",
        recordType = "IRSL",
        curveType = "measured",
        info = list(
          startDate = format(Sys.time() + time + x + 30, "%Y%m%d%H%M%S"),
          position = 1)
      )

    time <- time + x + 60
    l <- c(l, irr, lum)

  }

  ## generate object
  object <- set_RLum("RLum.Analysis", records = l, originator = "read_XSYG2R")

  # Test --------------------------------------------------------------------
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
    t_star = "half_complex",
    background.integral = 10:40,
    structure = "Lx",
    plot = FALSE
  ), "RLum.Results")
  expect_s4_class(analyse_FadingMeasurement(
    object,
    signal.integral = 1:2,
    t_star = "end",
    background.integral = 10:40,
    structure = "Lx",
    plot = FALSE
  ), "RLum.Results")
  })

  expect_error(analyse_FadingMeasurement(object, signal.integral = 1:2,
                                         background.integral = 2),
               "Overlapping of 'signal.integral' and 'background.integral'")

  SW({
  expect_warning(analyse_FadingMeasurement(object, signal.integral = 1:2,
                                           background.integral = 3,
                                           structure = "Lx"),
                 "Number of background channels for Lx < 25")

  expect_warning(analyse_FadingMeasurement(object, signal.integral = 1:2,
                                           background.integral = 3),
                 "Lx and Tx have different sizes: skipped sample 2")
  })
})

test_that("test BIN file while fading data", {
  testthat::skip_on_cran()

  data(ExampleData.BINfileData, envir = environment())
  d1 <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1)
  expect_error(analyse_FadingMeasurement(d1),
               "BIN-file has version 03, but only versions from 05 on are supported")

  v5 <- read_BIN2R(test_path("_data/BINfile_V5.binx"), verbose = FALSE)
  SW({
  d2 <- Risoe.BINfileData2RLum.Analysis(v5)
  })
  expect_output(analyse_FadingMeasurement(d2, signal.integral = 1:2,
                                          background.integral = 10:30,
                                          plot = TRUE))
})
