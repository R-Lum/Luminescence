context("calc_HomogeneityTest")

##use the data example given by Galbraith (2003)
df <-
  data.frame(
    x = c(30.1, 53.8, 54.3, 29.0, 47.6, 44.2, 43.1),
    y = c(4.8, 7.1, 6.8, 4.3, 5.2, 5.9, 3.0))

temp <- calc_HomogeneityTest(df)


test_that("check class and length of output", {
  testthat::skip_on_cran()
  expect_equal(is(temp), c("RLum.Results", "RLum"))
  expect_equal(length(temp), 3)

})

test_that("check values from output example", {
  testthat::skip_on_cran()

  results <- get_RLum(temp)

  ##test the normal
  expect_equal(results$n, 7)
  expect_equal(round(results$g.value, 4), 19.2505)
  expect_equal(results$df, 6)
  expect_equal(round(results$P.value,3), 0.004)

  ##test the unlogged version
  temp <- calc_HomogeneityTest(df, log = FALSE)$summary
  expect_equal(round(temp$P.value,3),0.001)
})
