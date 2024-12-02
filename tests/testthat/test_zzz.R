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
  expect_warning(tune_Data(data.frame(runif(n = 10, 8,12),runif(n = 10, 0.1,0.3) ), decrease.error = TRUE))
  expect_warning(tune_Data(data.frame(runif(n = 10, 8,12),runif(n = 10, 0.1,0.3) ), increase.data = TRUE))

  ##sTeve
  ## read example data set
  data(ExampleData.DeValues, envir = environment())
  ExampleData.DeValues <-
    convert_Second2Gray(ExampleData.DeValues$BT998, c(0.0438,0.0019))

  ## create plot straightforward
  expect_silent(plot_KDE(data = ExampleData.DeValues))
  expect_silent(sTeve(type = 1))
  expect_silent(sTeve(type = 2, t_animation = 1))
  expect_silent(sTeve(type = 3, t_animation = 1, n.tree = 2))
  expect_error(sTeve("error"),
               "'n_frames' should be of class 'integer' or 'numeric'")
})
