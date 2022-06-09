test_that("Test certain input scenarios", {
  testthat::skip_on_cran()
  local_edition(3)

  ##function stop
  expect_error(plot_DRCSummary("test"), regexp = "The input is not of class 'RLum.Results'")
  expect_error(plot_DRCSummary(set_RLum("RLum.Results")),
               regexp = "'object' was created by none supported function, cf. manual for allowed originators")

})

test_that("Test plotting", {
  testthat::skip_on_cran()
  local_edition(3)

  #load data example data
  data(ExampleData.BINfileData, envir = environment())

  #transform the values from the first position in a RLum.Analysis object
  object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

  results <- analyse_SAR.CWOSL(
    object = object,
    signal.integral.min = 1,
    signal.integral.max = 2,
    background.integral.min = 900,
    background.integral.max = 1000,
    plot = FALSE,
    verbose = FALSE
  )

  ## create LambertW DRC
  results_LamW <- analyse_SAR.CWOSL(
    object = object,
    fit.method = "LambertW",
    signal.integral.min = 1,
    signal.integral.max = 2,
    background.integral.min = 900,
    background.integral.max = 1000,
    NumberIterations.MC = 2,
    plot = FALSE,
    verbose = FALSE
  )


  ##simple
  expect_silent(plot_DRCSummary(results))

  ##simple with graphical arguments
  expect_silent(plot_DRCSummary(results, col.lty = "red"))

  ##simple with LambertW
  expect_silent(plot_DRCSummary(results_LamW))

  ##plus points
  expect_silent(plot_DRCSummary(results, show_dose_points = TRUE, show_natural = TRUE))

  ##expect warning
  expect_warning(plot_DRCSummary(results, show_dose_points = TRUE, show_natural = TRUE, sel_curves = 1000))

  ## different fit
  ##error
  err <- merge_RLum(list(results, results))
  err@data$data$Fit[2] <- "err"
  expect_error(plot_DRCSummary(err), regexp = "\\[plot\\_DRCSummary\\(\\)\\] I can only.*")

})
