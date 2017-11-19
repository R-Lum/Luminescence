context("zzz")

test_that("Test zzz functions ... they should still work", {
  testthat::skip_on_cran()

  ##get right answer
  expect_equal(get_rightAnswer(), 46)
  expect_equal(get_rightAnswer("test"), 46)

  ##get quote
  expect_silent(get_Quote())
  expect_silent(get_Quote(ID = 1))
  expect_silent(get_Quote(ID = 10, separated = TRUE))
  expect_silent(get_Quote(ID = 1e06))

  ##tune data
  expect_warning(tune_Data(1:10))

})
