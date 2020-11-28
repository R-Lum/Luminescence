##load data
data(ExampleData.CW_OSL_Curve, envir = environment())
values <- CW_Curve.BosWallinga2012

test_that("Check the example and the numerical values", {
  testthat::skip_on_cran()
  local_edition(3)

  values_pLM <- CW2pLM(values)
  values_pLMi <- CW2pLMi(values, P = 1/20)
  values_pLMi_alt <- CW2pLMi(values)
  values_pHMi <- suppressWarnings(CW2pHMi(values, delta = 40))
  values_pHMi_alt <- suppressWarnings(CW2pHMi(values))
  values_pHMi_alt1 <- suppressWarnings(CW2pHMi(values, delta = 2))
  values_pPMi <- suppressWarnings(CW2pPMi(values, P = 1/10))

    ##check conversion sum values
    expect_equal(round(sum(values_pLM), digits = 0),90089)
    expect_equal(round(sum(values_pLMi[,1:2]), digits = 0),197522)
    expect_equal(round(sum(values_pLMi_alt[,1:2]), digits = 0),197522)
    expect_equal(round(sum(values_pHMi[,1:2]), digits = 0),217431)
    expect_equal(round(sum(values_pHMi_alt[,1:2]), digits = 0),217519)
    expect_equal(round(sum(values_pHMi_alt1[,1:2]), digits = 0), 221083)
    expect_equal(round(sum(values_pPMi[,1:2]), digits = 0),196150)


})

test_that("Test RLum.Types", {
  testthat::skip_on_cran()
  local_edition(3)

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
  expect_error(object = CW2pLM(values = matrix(0, 2)))
  expect_error(object = CW2pLMi(values = matrix(0, 2)))
  expect_error(object = CW2pHMi(values = matrix(0, 2)))
  expect_error(object = CW2pPMi(values = matrix(0, 2)))

  object@recordType <- "RF"
  expect_error(CW2pLM(values = object),
               "recordType RF is not allowed for the transformation")
  expect_error(object = CW2pLMi(values = object),
               regexp = "[CW2pLMi()] recordType RF is not allowed for the transformation!",
               fixed = TRUE)
  expect_error(object = CW2pHMi(values = object),
               regexp = "[CW2pHMi()] recordType RF is not allowed for the transformation!",
               fixed = TRUE)
  expect_error(object = CW2pPMi(values = object),
               regexp = "[CW2pPMi()] recordType RF is not allowed for the transformation!",
               fixed = TRUE)


})
