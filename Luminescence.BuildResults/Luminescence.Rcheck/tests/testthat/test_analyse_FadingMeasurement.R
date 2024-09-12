## load example data (sample UNIL/NB123, see ?ExampleData.Fading)
data("ExampleData.Fading", envir = environment())
fading_data <- ExampleData.Fading$fading.data$IR50

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(analyse_FadingMeasurement(object = "test"),
               "'object' must be an 'RLum.Analysis' object or a 'list' of such objects")
  expect_error(expect_warning(
      analyse_FadingMeasurement(list(fading_data, "test")),
      "2 non-supported records removed"),
      "'object' must be an 'RLum.Analysis' object or a 'list' of such objects")
  expect_error(analyse_FadingMeasurement(cbind(fading_data, fading_data[, 1])),
               "if you provide a data.frame as input, the number of columns")
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

  ## check various for t_star
  ## stop t_star
  expect_error(analyse_FadingMeasurement(
    object,
    t_star = "error",
  ), "\\[analyse_FadingMeasurement\\(\\)\\] Invalid input for t_star.")

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

  expect_warning(analyse_FadingMeasurement(object, signal.integral = 1:2,
                                           background.integral = 3,
                                           structure = c("Lx", "error")),
                 "Nothing to combine, object contains a single curve")
  })
})
