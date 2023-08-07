test_that("check class", {
  testthat::skip_on_cran()
  local_edition(3)

  object <- set_RLum(class = "RLum.Data.Curve")
  expect_length(rep(object, 10), 10)

  ## <RLum.Results>
  data(ExampleData.DeValues, envir = environment())
  temp <- calc_FuchsLang2001(ExampleData.DeValues$BT998, cvThreshold = 5, plot = FALSE)
  expect_output(show(temp))

  ## test coercion
  expect_s4_class(as(as(object = temp, Class = "list"), "RLum.Results"), "RLum.Results")
})
