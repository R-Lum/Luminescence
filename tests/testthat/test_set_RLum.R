test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(set_RLum(1),
               "'class' should be of class 'character'")
  expect_error(set_RLum(""),
               "'class' cannot be an empty character")
})
