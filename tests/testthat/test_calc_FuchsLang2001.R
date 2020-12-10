test_that("check class and length of output", {
  testthat::skip_on_cran()
  local_edition(3)

  ##load example data
  data(ExampleData.DeValues, envir = environment())

  ##break function
  expect_error(calc_FuchsLang2001(
    data = "ExampleData.DeValues$BT998",
    cvThreshold = 5,
    plot = FALSE,
    verbose = FALSE),
    "\\[calc_FuchsLang2001\\(\\)\\] 'data' has to be of type 'data.frame' or 'RLum.Results'\\!")

  ##the simple and silent run
  temp <- expect_s4_class(
    calc_FuchsLang2001(
      data = ExampleData.DeValues$BT998,
      cvThreshold = 5,
      plot = FALSE,
      verbose = FALSE),
    "RLum.Results")

  ##regression tests
  expect_equal(length(temp), 4)
  expect_equal(get_RLum(temp)$de, 2866.11)
  expect_equal(get_RLum(temp)$de_err, 157.35)
  expect_equal(get_RLum(temp)$de_weighted, 2846.66)
  expect_equal(get_RLum(temp)$de_weighted_err, 20.58)
  expect_equal(get_RLum(temp)$n.usedDeValues, 22)

  ##the check output
  output <- expect_s4_class(
    calc_FuchsLang2001(
      data = ExampleData.DeValues$BT998,
      cvThreshold = 5,
      plot = TRUE,
      verbose = TRUE

    ), "RLum.Results")

})

