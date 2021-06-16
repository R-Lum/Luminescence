test_that("general test", {
  testthat::skip_on_cran()
  local_edition(3)

  ## load example data (sample UNIL/NB123, see ?ExampleData.Fading)
  data("ExampleData.Fading", envir = environment())

  ##(1) get fading measurement data (here a three column data.frame)
  fading_data <- ExampleData.Fading$fading.data$IR50

  ##break function
  expect_error(analyse_FadingMeasurement(object = "test"),
               regexp = "'object' needs to be of type 'RLum.Analysis' or a 'list' of such objects!")

  ## run routine analysis
  expect_s4_class(analyse_FadingMeasurement(
    fading_data,
    plot = TRUE,
    verbose = TRUE,
    n.MC = 10), class = "RLum.Results")

  ##not plot not verbose
  expect_s4_class(analyse_FadingMeasurement(
    fading_data,
    plot = FALSE,
    verbose = FALSE,
    n.MC = 10), class = "RLum.Results")

})
