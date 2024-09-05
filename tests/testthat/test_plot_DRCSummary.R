data(ExampleData.BINfileData, envir = environment())
object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1)
results <- analyse_SAR.CWOSL(
    object = object,
    signal.integral.min = 1,
    signal.integral.max = 2,
    background.integral.min = 900,
    background.integral.max = 1000,
    plot = FALSE,
    verbose = FALSE
)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(plot_DRCSummary("test"),
               "'object' is not of class 'RLum.Results'")
  expect_error(plot_DRCSummary(set_RLum("RLum.Results")),
               "'object' was not created by a supported function")

  ## different fit
  err <- merge_RLum(list(results, results))
  err@data$data$Fit[2] <- "err"
  expect_error(plot_DRCSummary(err),
               "I can only visualise dose-response curves based on the same")
})

test_that("Test plotting", {
  testthat::skip_on_cran()

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

  ## list
  expect_silent(plot_DRCSummary(list(results, results_LamW),
                                main = "Title"))
  expect_silent(plot_DRCSummary(list(results, results_LamW),
                                source_dose_rate = 1))

  ##plus points
  expect_silent(plot_DRCSummary(results, show_dose_points = TRUE, show_natural = TRUE))

  ##expect warning
  expect_warning(plot_DRCSummary(results, show_dose_points = TRUE, show_natural = TRUE, sel_curves = 1000))
})
