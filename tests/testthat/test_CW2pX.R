##load data
data(ExampleData.CW_OSL_Curve, envir = environment())
values <- CW_Curve.BosWallinga2012

test_that("check functionality", {
  testthat::skip_on_cran()

  tol <- 1.5e-6
  expect_snapshot_plain(CW2pLM(values),
                        tolerance = tol)
  expect_snapshot_plain(CW2pLMi(values, P = 1/20),
                        tolerance = tol)
  expect_snapshot_plain(CW2pLMi(values),
                        tolerance = tol)
  expect_warning(
      expect_snapshot_plain(CW2pHMi(values),
                            tolerance = tol),
      "56 invalid values have been found")
  expect_warning(
      expect_snapshot_plain(CW2pHMi(values, delta = 40),
                            tolerance = tol),
      "56 invalid values have been found")
  SW({ # repeated warning about invalid values
  expect_warning(
      expect_snapshot_plain(CW2pHMi(values, delta = 2),
                            tolerance = tol),
      "t' is beyond the time resolution and more than two data points")
  })
  expect_warning(
      expect_snapshot_plain(CW2pPMi(values, P = 1/10),
                            tolerance = tol),
      "t' is beyond the time resolution: only two data points have been extrapolated")
})

test_that("Test RLum.Types", {
  testthat::skip_on_cran()

  ##load CW-OSL curve data
  data(ExampleData.CW_OSL_Curve, envir = environment())
  object <-
    set_RLum(
      class = "RLum.Data.Curve",
      data = as.matrix(ExampleData.CW_OSL_Curve),
      curveType = "measured",
      recordType = "OSL"
    )

  ##transform values
  expect_s4_class(CW2pLM(object), class = "RLum.Data.Curve")
  expect_s4_class(CW2pLMi(object), class = "RLum.Data.Curve")
  expect_s4_class(CW2pHMi(object), class = "RLum.Data.Curve")
  expect_s4_class(suppressWarnings(CW2pPMi(object)), class = "RLum.Data.Curve")

  ##test error handling
  expect_error(CW2pLMi(values, P = 0), regexp = "[CW2pLMi] P has to be > 0!", fixed = TRUE)
  expect_warning(CW2pLMi(values, P = 10))
  expect_error(CW2pHMi(values = matrix(0, 2)),
               "'values' should be of class 'data.frame' or 'RLum.Data.Curve'")
  expect_error(CW2pLMi(values = matrix(0, 2)),
               "'values' should be of class 'data.frame' or 'RLum.Data.Curve'")
  expect_error(CW2pLM(values = matrix(0, 2)),
               "'values' should be of class 'data.frame' or 'RLum.Data.Curve'")
  expect_error(CW2pPMi(values = matrix(0, 2)),
               "'values' should be of class 'data.frame' or 'RLum.Data.Curve'")

  object@recordType <- "RF"
  expect_error(CW2pLM(values = object),
               "recordType RF is not allowed for the transformation")
  expect_error(object = CW2pLMi(values = object),
               "[CW2pLMi()] recordType RF is not allowed for the transformation",
               fixed = TRUE)
  expect_error(object = CW2pHMi(values = object),
               "[CW2pHMi()] recordType RF is not allowed for the transformation",
               fixed = TRUE)
  expect_error(object = CW2pPMi(values = object),
               "[CW2pPMi()] recordType RF is not allowed for the transformation",
               fixed = TRUE)
})
