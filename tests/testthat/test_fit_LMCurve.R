data(ExampleData.FittingLM, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(fit_LMCurve("error"),
               "'values' has to be of type 'data.frame' or 'RLum.Data.Curve'")
  expect_error(fit_LMCurve(set_RLum("RLum.Data.Curve", recordType = "OSL")),
               "recordType should be 'RBR' or 'LM-OSL'")
  expect_error(fit_LMCurve(values.curve, values.bg = "error"),
               "'values.bg' must be of type 'data.frame' or 'RLum.Data.Curve'")
  expect_error(fit_LMCurve(set_RLum("RLum.Data.Curve", recordType = "LM-OSL"),
                           values.bg = values.curveBG),
               "Lengths of 'values' and 'values.bg' differ")
  expect_error(fit_LMCurve(values.curve, values.bg = values.curveBG,
                           bg.subtraction = "error"),
               "Invalid method for background subtraction")
  expect_error(fit_LMCurve(values.curve, n.components = "error"),
               "'n.components' must be a positive integer scalar")
  expect_error(fit_LMCurve(values.curve, fit.method = "error"),
               "Unknown method for 'fit.method'")

  ## warning for failed confint ...skip on windows because with R >= 4.2 is does not fail anymore
  if (!grepl(pattern = "mingw", sessionInfo()$platform) && !grepl(pattern = "linux", sessionInfo()$platform))
    expect_warning(fit_LMCurve(values = values.curve, fit.calcError = TRUE))
})

test_that("check class and length of output", {
  testthat::skip_on_cran()

  SW({
  fit <- fit_LMCurve(values.curve, values.bg = values.curveBG,
                     n.components = 3, log = "x",
                     start_values = data.frame(Im = c(170,25,400),
                                               xm = c(56,200,1500)))
  })

  expect_s4_class(fit, "RLum.Results")
  expect_equal(length(fit), 4)
  expect_type(fit$component_matrix, "double")
  expect_equal(nrow(fit$component_matrix), 4000)

  expect_equal(fit$data$n.components, 3)
  expect_equal(round(fit$data$Im1, digits = 0), 169)
  expect_equal(round(fit$data$xm1, digits = 0), 49)
  expect_equal(round(fit$data$b1, digits = 0), 2)
  expect_equal(round(fit$data$`pseudo-R^2`, digits = 0), 1)

})

## Test 2 with LM
SW({
fit <- fit_LMCurve(values = values.curve,
                   values.bg = values.curveBG,
                   n.components = 3,
                   log = "x",
                   fit.method = "LM",
                   plot = FALSE)
})

test_that("check class and length of output", {
  testthat::skip_on_cran()

  expect_s4_class(fit, "RLum.Results")
  expect_equal(length(fit), 4)

  expect_equal(fit$data$n.components, 3)
  expect_equal(round(fit$data$Im1, digits = 0), 169)
  expect_equal(round(fit$data$xm1, digits = 0), 49)
  expect_equal(round(fit$data$b1, digits = 0), 2)
  expect_equal(round(fit$data$`pseudo-R^2`, digits = 0), 1)

  SW({
  expect_message(fit <- fit_LMCurve(values.curve, values.bg = values.curveBG,
                                    start_values = data.frame(Im = c(70,25,400),
                                                              xm = c(56,200,10))),
                 "Fitting Error: Plot without fit produced")
  expect_equal(fit@data$component_matrix, NA)

  fit_LMCurve(values.curve, values.bg = values.curveBG, plot.BG = TRUE,
              fit.advanced = TRUE)
  fit_LMCurve(values.curve, values.bg = values.curveBG, plot.BG = TRUE,
              bg.subtraction = "linear")
  fit_LMCurve(values.curve, values.bg = values.curveBG, plot.BG = TRUE,
              bg.subtraction = "channel")
  fit_LMCurve(values.curve, values.bg = values.curveBG,
              fit.calcError = TRUE)
  suppressWarnings(
      expect_warning(fit_LMCurve(values.curve, values.bg = values.curveBG,
                                 fit.advanced = TRUE, fit.calcError = TRUE),
                 "The computation of the parameter confidence intervals failed")
  )
  })
})
