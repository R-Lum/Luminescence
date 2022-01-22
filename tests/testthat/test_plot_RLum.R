test_that("test_plot_RLum", {
  testthat::skip_on_cran()
  local_edition(3)

  ## create dataset to test
  image <- as(array(rnorm(1000), dim = c(10,10,10)), "RLum.Data.Image")
  image_short <- as(array(rnorm(1000), dim = c(10,10,1)), "RLum.Data.Image")

  ## image plot
  expect_silent(plot_RLum(image))

  ## a list
  ## check list
  expect_silent(plot_RLum(list(image_short, image_short), main = "test", mtext = "test"))

  ## trigger error
  expect_error(plot_RLum("error"), "\\[plot_RLum\\(\\)\\] Sorry, I don't know what to do for object of type 'character'.")


})

