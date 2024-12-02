## load data
data(ExampleData.CW_OSL_Curve, envir = environment())
values <- CW_Curve.BosWallinga2012
object <- set_RLum(class = "RLum.Data.Curve",
                   data = as.matrix(ExampleData.CW_OSL_Curve),
                   curveType = "measured",
                   recordType = "OSL")

test_that("check functionality", {
  testthat::skip_on_cran()

  tol <- 1.5e-6
  expect_snapshot_plain(convert_CW2pLM(values),
                        tolerance = tol)
  expect_snapshot_plain(convert_CW2pLMi(values, P = 1/20),
                        tolerance = tol)
  expect_snapshot_plain(convert_CW2pLMi(values),
                        tolerance = tol)
  expect_warning(
      expect_snapshot_plain(convert_CW2pHMi(values),
                            tolerance = tol),
      "56 invalid values have been found")
  expect_warning(
      expect_snapshot_plain(convert_CW2pHMi(values, delta = 40),
                            tolerance = tol),
      "56 invalid values have been found")
  SW({ # repeated warning about invalid values
  expect_warning(
      expect_snapshot_plain(convert_CW2pHMi(values, delta = 2),
                            tolerance = tol),
      "t' is beyond the time resolution and more than two data points")
  })
  expect_warning(
      expect_snapshot_plain(convert_CW2pPMi(values, P = 1/10),
                            tolerance = tol),
      "t' is beyond the time resolution: only two data points have been extrapolated")
})

test_that("Test RLum.Types", {
  testthat::skip_on_cran()

  expect_s4_class(convert_CW2pLM(object),
                  class = "RLum.Data.Curve")
  expect_s4_class(convert_CW2pLMi(object),
                  class = "RLum.Data.Curve")
  expect_s4_class(convert_CW2pHMi(object),
                  class = "RLum.Data.Curve")
  expect_s4_class(suppressWarnings(convert_CW2pPMi(object)),
                  class = "RLum.Data.Curve")

  ## deprecated names
  expect_warning(CW2pHMi(object),
                 "'CW2pHMi' is deprecated")
  expect_warning(CW2pLM(object),
                 "'CW2pLM' is deprecated")
  expect_warning(CW2pLMi(object),
                 "'CW2pLMi' is deprecated")
  suppressWarnings(
  expect_warning(CW2pPMi(object),
                 "'CW2pPMi' is deprecated"))
})

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(convert_CW2pLMi(values, P = 0),
               "[convert_CW2pLMi()] P has to be > 0", fixed = TRUE)
  expect_warning(convert_CW2pLMi(values, P = 10),
                 "t' is beyond the time resolution and more than two data points")
  expect_error(convert_CW2pHMi(values = matrix(0, 2)),
               "'values' should be of class 'data.frame' or 'RLum.Data.Curve'")
  expect_error(convert_CW2pHMi(values = data.frame()),
               "'values' cannot be an empty data.frame")
  expect_error(convert_CW2pLMi(values = matrix(0, 2)),
               "'values' should be of class 'data.frame' or 'RLum.Data.Curve'")
  expect_error(convert_CW2pLMi(values = data.frame()),
               "'values' cannot be an empty data.frame")
  expect_error(convert_CW2pLM(values = matrix(0, 2)),
               "'values' should be of class 'data.frame' or 'RLum.Data.Curve'")
  expect_error(convert_CW2pLM(values = data.frame()),
               "'values' cannot be an empty data.frame")
  expect_error(convert_CW2pPMi(values = matrix(0, 2)),
               "'values' should be of class 'data.frame' or 'RLum.Data.Curve'")
  expect_error(convert_CW2pPMi(values = data.frame()),
               "'values' cannot be an empty data.frame")

  object@recordType <- "RF"
  expect_error(convert_CW2pLM(values = object),
               "recordType RF is not allowed for the transformation")
  expect_error(convert_CW2pLMi(values = object),
               "[convert_CW2pLMi()] recordType RF is not allowed for the transformation",
               fixed = TRUE)
  expect_error(convert_CW2pHMi(values = object),
               "[convert_CW2pHMi()] recordType RF is not allowed for the transformation",
               fixed = TRUE)
  expect_error(convert_CW2pPMi(values = object),
               "[convert_CW2pPMi()] recordType RF is not allowed for the transformation",
               fixed = TRUE)
})
