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
  expect_error(calc_FiniteMixture(ExampleData.DeValues$CA1),
               "argument .* is missing, with no default")
  expect_error(calc_FiniteMixture(ExampleData.DeValues$CA1, sigmab = -1),
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

test_that("check class and length of output", {
  testthat::skip_on_cran()

  ## simple run
  SW({
  temp <- expect_s4_class(calc_FiniteMixture(
    ExampleData.DeValues$CA1,
    sigmab = 0.2,
    n.components = 2,
    grain.probability = TRUE,
    verbose = TRUE), "RLum.Results")
  })

  ## check length of output
  expect_equal(length(temp), 10)

  ## check for numerical regression
  results <- get_RLum(temp)
  expect_equal(results$de[1], 31.5299)
  expect_equal(results$de[2], 72.0333)
  expect_equal(results$de_err[1], 3.6387)
  expect_equal(results$de_err[2], 2.4082)
  expect_equal(results$proportion[1], 0.1096)
  expect_equal(results$proportion[2], 0.8904)

  ## test plot
  SW({
  expect_s4_class(calc_FiniteMixture(
    ExampleData.DeValues$CA1,
    sigmab = 0.2,
    n.components = 2:3,
    grain.probability = TRUE,
    trace = TRUE,
    main = "Plot title",
    verbose = TRUE), "RLum.Results")

  ## more coverage
  expect_warning(calc_FiniteMixture(
    ExampleData.DeValues$CA1[2:9, ],
    sigmab = 0.1,
    n.components = 3,
    verbose = TRUE),
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

  ## plot with plot_RLum.Results
  t <- expect_s4_class(calc_FiniteMixture(
    ExampleData.DeValues$CA1,
    sigmab = 0.2,
    n.components = 2:3,
    grain.probability = FALSE,
    trace = FALSE,
    plot = FALSE,
    main = "Plot title",
    verbose = FALSE), "RLum.Results")
  plot_RLum.Results(t, pdf.colors = "none")
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
