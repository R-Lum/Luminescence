## load data
data(ExampleData.TR_OSL, envir = environment())
temp_mat <- get_RLum(ExampleData.TR_OSL)[1:200, ]

test_that("input validation", {
  testthat::skip_on_cran()

  expect_warning(expect_null(fit_OSLLifeTimes("error")),
                 "'object' should be of class 'RLum.Data.Curve', 'data.frame'")
  expect_error(fit_OSLLifeTimes(data.frame()),
               "'object' cannot be an empty data.frame")
  expect_error(fit_OSLLifeTimes(set_RLum("RLum.Analysis")),
               "'object' cannot be an empty RLum.Analysis")
  expect_error(fit_OSLLifeTimes(matrix(NA, 0, 3)),
               "'object' cannot be an empty matrix")
  expect_error(fit_OSLLifeTimes(matrix()),
               "'object' should have at least two columns")
  expect_error(fit_OSLLifeTimes(ExampleData.TR_OSL, n.components = -1),
               "'n.components' should be a positive integer scalar")
  expect_error(fit_OSLLifeTimes(ExampleData.TR_OSL, signal_range = FALSE),
               "'signal_range' should be of class 'numeric'")

  empty <- set_RLum(class = "RLum.Data.Curve")
  expect_error(fit_OSLLifeTimes(empty),
               "recordType 'NA' not supported for input object")
  expect_warning(expect_output(fit_OSLLifeTimes(list(empty)),
                               "recordType 'NA' not supported for input object"),
                 "Nothing was merged as the object list was found to be empty")

  expect_message(expect_null(fit_OSLLifeTimes(temp_mat[1:3, ])),
                 "For 1 components the dataset must have at least 5 signal points")
  expect_message(fit_OSLLifeTimes(temp_mat, n.components = 1,
                                  signal_range = c(1, 3), verbose = FALSE),
                 "For 1 components the dataset must have at least 5 signal points")
  expect_message(fit_OSLLifeTimes(temp_mat, n.components = 2,
                                  signal_range = c(1, 6), verbose = FALSE),
                 "For 2 components the dataset must have at least 7 signal points")

  expect_warning(fit_OSLLifeTimes(temp_mat, n.components = 1,
                                  signal_range = c(1, 150:200), verbose = FALSE),
                 "'signal_range' has more than 2 elements")
  expect_warning(fit_OSLLifeTimes(temp_mat, n.components = 1,
                                  signal_range = c(1, 300), verbose = FALSE),
                 "'signal_range' > number of channels, reset to maximum")
  expect_warning(fit_OSLLifeTimes(temp_mat, n.components = 1,
                                  signal_range = 300, verbose = FALSE),
                 "'signal_range' first element > last element, reset to default"
                )

  temp <- temp_mat
  temp[100, 2] <- NA
  expect_warning(fit_OSLLifeTimes(temp, n.components = 1, verbose = FALSE),
                 "NA values detected and removed from dataset")

  temp[100:110, 2] <- 0
  expect_warning(fit_OSLLifeTimes(temp, n.components = 1, verbose = FALSE),
                 "The dataset contains 0, a value of 0.1 has been added")

  expect_warning(fit_OSLLifeTimes(ExampleData.TR_OSL,
                                  method_control = list(seed = 1,
                                                        DEoptim.itermax = 15,
                                                        nlsLM.lower = FALSE),
                                  plot = FALSE,
                                  verbose = FALSE,
                                  n.components = 1),
                 "At least one parameter is negative")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6

  ## Test different inputs
  ##simple run
  set.seed(1)
  SW({
  expect_snapshot_RLum(fit_OSLLifeTimes(
    object = ExampleData.TR_OSL,
    plot = FALSE,
    method_control = list(DEoptim.itermax = 15),
    n.components = 1),
    tolerance = snapshot.tolerance)

  ##simple list
  temp_list <- list(ExampleData.TR_OSL, ExampleData.TR_OSL)
  expect_snapshot_RLum(fit_OSLLifeTimes(
    object = temp_list,
    log = "x",
    method_control = list(DEoptim.itermax = 25),
    n.components = 1),
    tolerance = snapshot.tolerance)
  })

  ## RLum.Analysis
  temp_analysis <- set_RLum("RLum.Analysis", records = temp_list)
  expect_s4_class(object = fit_OSLLifeTimes(
    object = temp_analysis,
    verbose = FALSE,
    plot = FALSE,
    n.components = 1), class = "RLum.Results")
  expect_s4_class(fit_OSLLifeTimes(
    object = list(temp_analysis),
    verbose = FALSE,
    plot = FALSE,
    n.components = 1), class = "RLum.Results")

  ## simple data.frame
  SW({
  expect_snapshot_RLum(fit_OSLLifeTimes(
    object = as.data.frame(temp_mat),
    method_control = list(seed = 1, weights = FALSE,
                          DEoptim.itermax = 50),
    signal.range = 3,
    verbose = TRUE,
    plot = FALSE,
    n.components = NULL),
    tolerance = snapshot.tolerance)
  })

  ##test arguments
  ##simple run
  expect_s4_class(object = fit_OSLLifeTimes(
    object = ExampleData.TR_OSL,
    method_control = list(seed = 1, weights = FALSE,
                          DEoptim.itermax = 25,
                          nlsLM.upper = FALSE, nlsLM.lower = FALSE),
    plot = FALSE,
    verbose = FALSE,
    n.components = 1), class = "RLum.Results")

  ##warning for log
  expect_warning(expect_warning(
      fit_OSLLifeTimes(
          object = temp_mat,
          verbose = FALSE,
          plot = TRUE,
          plot_simple = TRUE,
          log = list("xy"),
          lty = 1,
          col = 1,
          n.components = 1),
      "log-scale requires x-values > 0, set min xlim to 0.01"),
      "log-scale requires y-values > 0, set min ylim to 1.69e+10",
      fixed = TRUE)

  SW({
  expect_message(fit_OSLLifeTimes(temp_mat[1:10, ]),
                 "The fitting was not successful, consider trying again")
  })
})
