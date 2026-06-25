## load data
data(ExampleData.LxTxData, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(set_RLum(1),
               "'class' should be of class 'character' and have length 1")
  expect_error(set_RLum(character()),
               "'class' should be of class 'character' and have length 1")
  expect_error(set_RLum(letters),
               "'class' should be of class 'character' and have length 1")
  expect_error(set_RLum(""),
               "'class' cannot be an empty character")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_equal(set_RLum("RLum.Data.Curve")@originator,
               "eval_bare") # TODO(mcol): ideally it should be NA_character_
  expect_equal(set_RLum("RLum.Data.Curve", originator = "test")@originator,
               "test")
  expect_equal(fit_DoseResponseCurve(LxTxData, verbose = FALSE)@originator,
               "fit_DoseResponseCurve")

  ## qualified calls
  expect_equal(Luminescence::fit_DoseResponseCurve(LxTxData, verbose = FALSE)@originator,
               "fit_DoseResponseCurve")
  expect_equal(Luminescence:::fit_DoseResponseCurve(LxTxData, verbose = FALSE)@originator,
               "fit_DoseResponseCurve")
})
