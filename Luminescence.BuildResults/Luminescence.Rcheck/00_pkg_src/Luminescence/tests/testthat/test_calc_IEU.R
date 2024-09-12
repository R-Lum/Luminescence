data(ExampleData.DeValues, envir = environment())
temp <- calc_IEU(ExampleData.DeValues$CA1,
                 a = 0.2,
                 b = 1.9,
                 interval = 1, verbose = FALSE, plot = FALSE)

test_that("Test general behaviour", {
  testthat::skip_on_cran()

  data(ExampleData.DeValues, envir = environment())

  ##standard
  expect_silent(calc_IEU(
    ExampleData.DeValues$CA1,
    a = 0.2,
    b = 1.9,
    interval = 1,
    verbose = FALSE, plot =FALSE
  ))

  ##enable plot
  SW({
  expect_message(calc_IEU(
    ExampleData.DeValues$CA1,
    a = 0.2,
    b = 1.9,
    interval = 1,
    trace = TRUE,
    verbose = TRUE, plot = TRUE
  ))
  })

  ##verbose without setting
  expect_message(calc_IEU(
    ExampleData.DeValues$CA1,
    a = 0.2,
    b = 1.9,
    interval = 1,
    plot = FALSE
  ))

  ##cause stop
  expect_error(calc_IEU(
    "ExampleData.DeValues$CA1",
    a = 0.2,
    b = 1.9,
    interval = 1,
    plot = FALSE
  ))

  ##provide RLum.Results
  ##cause stop
  expect_silent(calc_IEU(
    set_RLum(class = "RLum.Results", data = list(test = ExampleData.DeValues$CA1)),
    a = 0.2,
    b = 1.9,
    interval = 1,
    verbose = FALSE, plot = FALSE
  ))
})

test_that("check class and length of output", {
  testthat::skip_on_cran()

  expect_s4_class(temp, "RLum.Results")
  expect_equal(length(temp), 5)

  results <- get_RLum(temp)

  expect_equal(results$de, 46.67)
  expect_equal(results$de_err, 2.55)
  expect_equal(results$n, 24)

})
