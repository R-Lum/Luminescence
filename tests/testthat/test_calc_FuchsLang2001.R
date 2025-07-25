## load data
data(ExampleData.DeValues, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_FuchsLang2001("error"),
               "[calc_FuchsLang2001()] 'data' should be of class 'data.frame'",
               fixed = TRUE)
  expect_error(calc_FuchsLang2001(data.frame()),
               "'data' cannot be an empty data.frame")
  expect_error(calc_FuchsLang2001(iris[, 1, drop = FALSE]),
               "'data' should have 2 columns")
})

test_that("check class and length of output", {
  testthat::skip_on_cran()

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

  ## using an RLum.Results object as input
  SW({
  expect_s4_class(calc_FuchsLang2001(data = temp, startDeValue = 24,
                                     plot = FALSE),
                  "RLum.Results")
  })

  ##the check output
  SW({
  output <- expect_s4_class(
    calc_FuchsLang2001(
      data = ExampleData.DeValues$BT998,
      cvThreshold = 5,
      plot = TRUE,
      verbose = TRUE

    ), "RLum.Results")
  })

  ## issue 818
  expect_silent(calc_FuchsLang2001(data.frame(ED = c(rep(0, 4), 10),
                                              ED_Error = rnorm(5) + 1),
                                   verbose = FALSE))
})
