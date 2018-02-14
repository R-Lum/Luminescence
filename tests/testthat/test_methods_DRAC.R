context("methods_DRAC")

##Full check
test_that("methods_DRAC", {
  skip_on_cran()

  input <- template_DRAC()

  ##test print methods
  expect_output(print(input, blueprint = TRUE))
  expect_output(print(input, blueprint = FALSE))

})

