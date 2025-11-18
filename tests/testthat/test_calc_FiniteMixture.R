## load data
data(ExampleData.DeValues, envir = environment())

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(calc_FiniteMixture(),
               "'data' should be of class 'data.frame' or 'RLum.Results'")
  expect_error(calc_FiniteMixture("test"),
               "'data' should be of class 'data.frame' or 'RLum.Results'")
  expect_error(calc_FiniteMixture(data.frame(col = 1:10)),
               "'data' object must have two columns")
  expect_error(calc_FiniteMixture(data.frame(col = 0:10, 1:11)),
               "'data' must have only positive values in its De column")
  expect_error(calc_FiniteMixture(data.frame(col = c(1:10, NA), 1:11)),
               "'data' must have only positive values in its De column")
  expect_error(calc_FiniteMixture(ExampleData.DeValues$CA1),
               "'sigmab' should be a single positive value")
  expect_error(calc_FiniteMixture(ExampleData.DeValues$CA1, sigmab = 2),
               "'sigmab' must be a value between 0 and 1")
  expect_error(calc_FiniteMixture(ExampleData.DeValues$CA1, sigmab = 0.2),
               "'n.components' should be of class 'integer' or 'numeric'")
  expect_error(calc_FiniteMixture(ExampleData.DeValues$CA1, sigmab = 0.2,
                                  n.components = 1),
               "'n.components' should be at least 2")
  expect_error(calc_FiniteMixture(ExampleData.DeValues$CA1, sigmab = 0.2,
                                  n.components = 2, pdf.sigma = "error"),
               "'pdf.sigma' should be one of 'sigmab' or 'se'")
  expect_error(calc_FiniteMixture(ExampleData.DeValues$CA1, sigmab = 0.2,
                                  n.components = 2, pdf.colors = "error"),
               "'pdf.colors' should be one of 'gray', 'colors' or 'none'")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  snapshot.tolerance <- 1.5e-6

  ## simple run
  SW({
  expect_snapshot_RLum(calc_FiniteMixture(
    ExampleData.DeValues$CA1,
    sigmab = 0.2,
    n.components = 2,
    grain.probability = TRUE,
    verbose = TRUE),
    tolerance = snapshot.tolerance)
  })

  expect_snapshot_RLum(calc_FiniteMixture(
    ExampleData.DeValues$CA1,
    sigmab = 0.2,
    n.components = 3:5,
    grain.probability = TRUE,
    verbose = FALSE),
    tolerance = snapshot.tolerance)

  ## more coverage
  SW({
  expect_warning(expect_message(calc_FiniteMixture(
    ExampleData.DeValues$CA1[2:9, ],
    sigmab = 0.1,
    n.components = 3,
    verbose = TRUE),
    "'n.components' specified only one component, nothing plotted"),
    "The model produced NA values: either the input data are inapplicable")
  })

  expect_output(calc_FiniteMixture(
    set_RLum("RLum.Results",
             data = list(data = ExampleData.DeValues$CA1)),
    sigmab = 0.62,
    n.components = 2:3,
    pdf.colors = "colors",
    verbose = TRUE),
    "No significant increase in maximum log-likelihood estimates")
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  SW({
  vdiffr::expect_doppelganger("default",
                              calc_FiniteMixture(
                                  ExampleData.DeValues$CA1,
                                  sigmab = 0.2,
                                  n.components = 2:3,
                                  grain.probability = TRUE,
                                  trace = TRUE,
                                  main = "Plot title",
                                  verbose = TRUE))

  vdiffr::expect_doppelganger("cex pdf.weights",
                              calc_FiniteMixture(
                                  ExampleData.DeValues$CA1,
                                  sigmab = 0.2,
                                  n.components = 2:3,
                                  grain.probability = TRUE,
                                  pdf.weights = FALSE,
                                  pdf.colors = "none",
                                  cex = 2))

  ## plot with plot_RLum.Results
  res <- calc_FiniteMixture(ExampleData.DeValues$CA1,
                            sigmab = 0.2,
                            n.components = 3:4,
                            grain.probability = FALSE,
                            trace = FALSE,
                            plot = FALSE,
                            main = "From RLum.Results",
                            verbose = FALSE)
  vdiffr::expect_doppelganger("from RLum.Results",
                              calc_FiniteMixture(res,
                                                 pdf.colors = "colors"))
  vdiffr::expect_doppelganger("from RLum.Results none",
                              calc_FiniteMixture(res,
                                                 pdf.colors = "none"))
  })
})

test_that("regression tests", {
  testthat::skip_on_cran()

  SW({
  ## issue 691
  expect_s4_class(calc_FiniteMixture(ExampleData.DeValues$CA1, sigmab = 0.2,
                                     n.components = c(2, 4), plot = FALSE),
                  "RLum.Results")
  expect_s4_class(calc_FiniteMixture(ExampleData.DeValues$CA1, sigmab = 0.2,
                                     n.components = 3:2, plot = FALSE),
                  "RLum.Results")

  ## issue 704
  expect_silent(calc_FiniteMixture(ExampleData.DeValues$CA1, sigmab = 0.2,
                                   n.components = 2:9, verbose = FALSE))

  ## issue 708
  expect_silent(calc_FiniteMixture(ExampleData.DeValues$CA1, sigmab = 0.57,
                                   n.components = 2:6, verbose = FALSE))

  ## issue 710
  expect_silent(calc_FiniteMixture(ExampleData.DeValues$CA1, sigmab = 0.54,
                                   n.components = 2:5, pdf.weight = FALSE,
                                   verbose = FALSE))
  })
})
