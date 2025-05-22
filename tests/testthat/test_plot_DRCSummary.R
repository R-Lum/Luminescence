## load data
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
               "'object' should be of class 'RLum.Results'")
  expect_error(plot_DRCSummary(set_RLum("RLum.Results")),
               "Object originator should be one of 'analyse_SAR.CWOSL' or")

  ## different fit
  err <- merge_RLum(list(results, results))
  err@data$data$Fit[2] <- "err"
  expect_error(plot_DRCSummary(err),
               "I can only visualise dose-response curves based on the same")
})

test_that("Test plotting", {
  testthat::skip_on_cran()

  ## create OTOR DRC
  results_OTOR <- analyse_SAR.CWOSL(
    object = object,
    fit.method = "OTOR",
    signal.integral.min = 1,
    signal.integral.max = 2,
    background.integral.min = 900,
    background.integral.max = 1000,
    n.MC = 2,
    plot = FALSE,
    verbose = FALSE
  )

  ## create OTOR DRC
  results_OTORX <- analyse_SAR.CWOSL(
    object = object,
    fit.method = "OTORX",
    dose.points.test = 10,
    signal.integral.min = 1,
    signal.integral.max = 2,
    background.integral.min = 900,
    background.integral.max = 1000,
    n.MC = 2,
    plot = FALSE,
    verbose = FALSE
  )

  ##simple
  expect_silent(plot_DRCSummary(results))

  ##simple with graphical arguments
  expect_silent(plot_DRCSummary(results, col.lty = "red"))

  ##simple with OTOR
  expect_silent(plot_DRCSummary(results_OTOR))

  ##simple with OTORX
  expect_silent(plot_DRCSummary(results_OTORX))

  ## list
  expect_silent(plot_DRCSummary(list(results, results_OTOR),
                                main = "Title"))
  expect_silent(plot_DRCSummary(list(results, results_OTOR),
                                source_dose_rate = 1))
  l <- expect_silent(plot_DRCSummary(list()))
  expect_length(l, 0)

  ##plus points
  expect_silent(plot_DRCSummary(results, show_dose_points = TRUE, show_natural = TRUE))

  ##expect warning
  expect_warning(plot_DRCSummary(results, show_dose_points = TRUE,
                                 show_natural = TRUE, sel_curves = 1000),
                 "'sel_curves' out of bounds, reset to full dataset")
  expect_warning(plot_DRCSummary(results_OTOR, xlim = c(-1e12, 1e12)),
                 "Dose response curve 1 contains NA/NaN values, curve removed")
})
