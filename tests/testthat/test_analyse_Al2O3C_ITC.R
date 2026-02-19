##load data
data(ExampleData.Al2O3C, envir = environment())

test_that("input validation", {
  skip_on_cran()

  a <- set_RLum(class = "RLum.Data.Curve", recordType = "OSL",
                data = matrix(1:20, ncol = 2))
  b <- set_RLum(class = "RLum.Data.Curve", recordType = "TL")
  object <- set_RLum(class = "RLum.Analysis", records = list(a,b))

  expect_error(analyse_Al2O3C_ITC("test"),
               "'object' should be of class 'RLum.Analysis'")
  expect_error(analyse_Al2O3C_ITC(iris),
               "'object' should be of class 'RLum.Analysis' or a 'list' of")
  expect_error(analyse_Al2O3C_ITC(set_RLum("RLum.Analysis")),
               "'object' cannot be an empty RLum.Analysis")
  expect_error(analyse_Al2O3C_ITC(list()),
               "'object' cannot be an empty list")
  expect_error(analyse_Al2O3C_ITC(list(data_ITC, "test")),
               "All elements of 'object' should be of class 'RLum.Analysis'")
  expect_error(analyse_Al2O3C_ITC(data_ITC, recordType = NA),
               "'recordType' should be of class 'character' or NULL")
  expect_error(expect_warning(analyse_Al2O3C_ITC(data_ITC, recordType = "EXP"),
                              "This request produced an empty list of records"),
               "'recordType' produced an empty object")
  expect_error(analyse_Al2O3C_ITC(data_ITC, method_control = "EXP"),
               "'method_control' should be of class 'list'")
  expect_error(analyse_Al2O3C_ITC(data_ITC, dose_points = NA),
               "'dose_points' should be of class 'numeric' or 'list'")
  expect_error(analyse_Al2O3C_ITC(data_ITC, dose_points = list(NA)),
               "All elements of 'dose_points' should be of class 'numeric'")
  expect_error(analyse_Al2O3C_ITC(data_ITC, signal_integral = 0),
               "'signal_integral' is of length 0 after removing values smaller")
})

test_that("check functionality", {
  skip_on_cran()

  SW({
  ## fit_DoseResponseCurve() fails to fit
  expect_warning(expect_null(analyse_Al2O3C_ITC(list(data_ITC),
                                                dose_points = list(2))),
                 "Nothing was merged as the object list was found to be empty")
  })

  ## integral_input
  set.seed(1)
  res1 <- analyse_Al2O3C_ITC(data_ITC, signal_integral = c(42.1, 42.3),
                             integral_input = "measurement", verbose = FALSE)
  set.seed(1)
  res2 <- analyse_Al2O3C_ITC(data_ITC, signal_integral = 1:3,
                             integral_input = "channel", verbose = FALSE)
  res1@info <- res2@info <- list() # remove $call
  res1@data$fit <- res2@data$fit <- NULL
  res1@.uid <- res2@.uid <- NA_character_
  expect_equal(res1, res2)

  expect_warning(analyse_Al2O3C_ITC(data_ITC, signal_integral = 1:4,
                                    integral_input = "measurement", verbose = FALSE),
                 "from time to channels failed: expected values in 42.1:51.9")
})

test_that("snapshot tests", {
  skip_on_cran()

  set.seed(1)
  snapshot.tolerance <- 5.0e-6

  SW({
  expect_snapshot_RLum(analyse_Al2O3C_ITC(data_ITC),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(analyse_Al2O3C_ITC(list(data_ITC),
                                          signal_integral = 2,
                                          method_control = list(fit.method = "EXP")),
                       tolerance = snapshot.tolerance)
  })
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  set.seed(1)

  SW({
    vdiffr::expect_doppelganger("default",
                                analyse_Al2O3C_ITC(data_ITC))
  })
})
