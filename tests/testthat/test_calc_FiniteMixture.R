test_that("check class and length of output", {
  testthat::skip_on_cran()

  ## load example data
  data(ExampleData.DeValues, envir = environment())

  ## input validation
  expect_error(calc_FiniteMixture("test"),
               "object has to be of type 'data.frame' or 'RLum.Results'")
  expect_error(calc_FiniteMixture(data.frame(col = 1:10)),
               "'data' object must have two columns")
  expect_error(calc_FiniteMixture(),
               "argument .* is missing, with no default")
  expect_error(calc_FiniteMixture(ExampleData.DeValues$CA1),
               "argument .* is missing, with no default")
  expect_error(calc_FiniteMixture(ExampleData.DeValues$CA1, sigmab = 0.2),
               "argument .* is missing, with no default")
  expect_error(calc_FiniteMixture(ExampleData.DeValues$CA1, sigmab = -1),
               "'sigmab' must be a value between 0 and 1")
  expect_error(calc_FiniteMixture(ExampleData.DeValues$CA1, sigmab = 0.2,
                                  n.components = 1),
               "At least two components need to be fitted")
  expect_error(calc_FiniteMixture(ExampleData.DeValues$CA1, sigmab = 0.2,
                                  n.components = 2, pdf.sigma = "test"),
               "Only 'se' or 'sigmab' allowed for the pdf.sigma argument")

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
  })
})
