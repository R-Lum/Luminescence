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
  expect_error(tune_Data(1:10),
               "'data' should be of class 'data.frame'")
  expect_error(tune_Data(iris[1, , drop = FALSE]),
               "'data' should have at least two rows")
  expect_error(tune_Data(iris[, 1, drop = FALSE]),
               "'data' should have at least two columns")
  expect_error(tune_Data(iris, decrease.error = -1),
               "'decrease.error' should be a single non-negative value")
  expect_error(tune_Data(iris, decrease.error = 10),
               "'decrease.error' should be at most 1")
  expect_error(tune_Data(iris, increase.data = -1),
               "'increase.data' should be a single non-negative value")
  expect_error(tune_Data(iris, increase.data = 2000),
               "'increase.data' should be at most 1000")
  expect_warning(
      expect_equal(ncol(tune_Data(iris, decrease.error = 0.5, increase.data = 1)),
                   2),
      "these activities on your")

  ##sTeve
  ## read example data set
  data(ExampleData.DeValues, envir = environment())
  ExampleData.DeValues <-
    convert_Second2Gray(ExampleData.DeValues$BT998, c(0.0438,0.0019))

  ## create plot straightforward
  expect_error(sTeve("error"),
               "'n_frames' should be a single positive integer value")
  expect_error(sTeve(c(1, NaN, 0)),
               "'n_frames' should be a single positive integer value")
  expect_error(sTeve(t_animation = 0),
               "'t_animation' should be a single positive integer value")
  expect_error(sTeve(n.tree = 0.12),
               "'n.tree' should be a single positive integer value")
  expect_silent(plot_KDE(data = ExampleData.DeValues))
  expect_silent(sTeve(type = 1))
  expect_silent(sTeve(type = 2, t_animation = 1))
  expect_silent(sTeve(type = 3, t_animation = 1, n.tree = 2))
  expect_silent(sTeve(n_frames = 1, n.tree = 1))
})
