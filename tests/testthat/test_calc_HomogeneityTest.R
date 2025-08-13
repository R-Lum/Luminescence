##use the data example given by Galbraith (2003)
df <-
  data.frame(
    x = c(30.1, 53.8, 54.3, 29.0, 47.6, 44.2, 43.1),
    y = c(4.8, 7.1, 6.8, 4.3, 5.2, 5.9, 3.0))

temp <- calc_HomogeneityTest(df, verbose = FALSE)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_HomogeneityTest(TRUE),
               "'data' should be of class 'data.frame' or 'RLum.Results'")
  expect_error(calc_HomogeneityTest(data.frame()),
               "'data' cannot be an empty data.frame")
  expect_error(calc_HomogeneityTest(iris[, 1, drop = FALSE]),
               "'data' should have 2 columns")
})

test_that("check values from output example", {
  testthat::skip_on_cran()

  expect_s4_class(temp, "RLum.Results")
  expect_equal(length(temp), 3)

  ## using an RLum.Results object as input
  SW({
  expect_s4_class(calc_HomogeneityTest(temp),
                  "RLum.Results")
  })

  results <- get_RLum(temp)

  ##test the normal
  expect_equal(results$n, 7)
  expect_equal(round(results$g.value, 4), 19.2505)
  expect_equal(results$df, 6)
  expect_equal(round(results$P.value,3), 0.004)

  ##test the unlogged version
  SW({
  temp <- calc_HomogeneityTest(df, log = FALSE)$summary
  })
  expect_equal(round(temp$P.value,3),0.001)

  ## negative values in data
  expect_warning(expect_warning(
      calc_HomogeneityTest(data.frame(1:5, -1:3), verbose = FALSE),
      "'data' contains negative values and 'log = TRUE'"),
      "NaNs produced")
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 924
  expect_s4_class(
      calc_HomogeneityTest(data.frame(1:4, NA), verbose = FALSE),
      "RLum.Results")
})
