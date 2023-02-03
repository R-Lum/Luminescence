## Test 1 with NLS
data(ExampleData.FittingLM, envir = environment())
fit <- fit_LMCurve(values = values.curve,
            values.bg = values.curveBG,
            n.components = 3,
            log = "x",
            start_values = data.frame(Im = c(170,25,400), xm = c(56,200,1500)),
            plot = FALSE)


test_that("crashs and warnings function", {
  testthat::skip_on_cran()
  local_edition(3)

  ## wrong input type
  expect_error(object = fit_LMCurve(values = "error"),
               regexp = "\\[fit\\_LMCurve\\(\\)\\] 'values' has to be of type 'data.frame' or 'RLum.Data.Curve'!")

  ## not RBR
  expect_error(fit_LMCurve(values = set_RLum("RLum.Data.Curve", recordType = "OSL")),
               regexp = "\\[fit\\_LMCurve\\(\\)\\] recordType should be .+")

  ## warning for failed confint ...skip on windows because with R >= 4.2 is does not fail anymore
  if (!grepl(pattern = "mingw", sessionInfo()$platform) && !grepl(pattern = "linux", sessionInfo()$platform))
    expect_warning(fit_LMCurve(values = values.curve, fit.calcError = TRUE))

})

test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_s4_class(fit, "RLum.Results")
  expect_equal(length(fit), 4)
  expect_type(fit$component_matrix, "double")
  expect_equal(nrow(fit$component_matrix), 4000)


})

test_that("check values from output example", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_equal(fit$data$n.components, 3)
  expect_equal(round(fit$data$Im1, digits = 0), 169)
  expect_equal(round(fit$data$xm1, digits = 0), 49)
  expect_equal(round(fit$data$b1, digits = 0), 2)
  expect_equal(round(fit$data$`pseudo-R^2`, digits = 0), 1)

})

## Test 2 with LM
data(ExampleData.FittingLM, envir = environment())
fit <- fit_LMCurve(values = values.curve,
                   values.bg = values.curveBG,
                   n.components = 3,
                   log = "x",
                   fit.method = "LM",
                   plot = FALSE)

test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_s4_class(fit, "RLum.Results")
  expect_equal(length(fit), 4)

})

test_that("check values from output example", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_equal(fit$data$n.components, 3)
  expect_equal(round(fit$data$Im1, digits = 0), 169)
  expect_equal(round(fit$data$xm1, digits = 0), 49)
  expect_equal(round(fit$data$b1, digits = 0), 2)
  expect_equal(round(fit$data$`pseudo-R^2`, digits = 0), 1)


})

