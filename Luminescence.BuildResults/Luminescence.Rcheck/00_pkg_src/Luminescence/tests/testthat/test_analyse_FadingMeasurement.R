test_that("general test", {
  testthat::skip_on_cran()
  local_edition(3)

  ## load example data (sample UNIL/NB123, see ?ExampleData.Fading)
  data("ExampleData.Fading", envir = environment())

  ##(1) get fading measurement data (here a three column data.frame)
  fading_data <- ExampleData.Fading$fading.data$IR50

  ##break function
  expect_error(analyse_FadingMeasurement(object = "test"),
               regexp = "'object' needs to be of type 'RLum.Analysis' or a 'list' of such objects!")

  ## run routine analysis
  expect_s4_class(analyse_FadingMeasurement(
    fading_data,
    plot = TRUE,
    verbose = TRUE,
    n.MC = 10), class = "RLum.Results")

  ##no plot not verbose
  expect_s4_class(analyse_FadingMeasurement(
    fading_data,
    plot = FALSE,
    verbose = FALSE,
    n.MC = 10), class = "RLum.Results")

})

test_that("test XSYG file fading data", {
  testthat::skip_on_cran()
  local_edition(3)

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
  expect_s4_class(analyse_FadingMeasurement(
    object,
    signal.integral = 1:2,
    background.integral = 10:40,
    structure = "Lx"
  ), "RLum.Results")

  ## check various for t_star
  ## stop t_star
  expect_error(analyse_FadingMeasurement(
    object,
    signal.integral = 1:2,
    t_star = "error",
    background.integral = 10:40,
    structure = "Lx"
  ), "\\[analyse_FadingMeasurement\\(\\)\\] Invalid input for t_star.")

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

