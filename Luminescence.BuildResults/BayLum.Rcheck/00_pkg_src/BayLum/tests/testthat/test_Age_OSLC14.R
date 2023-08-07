test_that("Full function test", {
  testthat::skip_on_cran()
  local_edition(3)

  ## Load data
  # OSL data
  data(DATA1,envir = environment())
  data(DATA2,envir = environment())
  Data <- combine_DataFiles(DATA2,DATA1)

  # 14C data
  C14Cal <- DATA_C14$C14[1,1]
  SigmaC14Cal <- DATA_C14$C14[1,2]
  Names <- DATA_C14$Names[1]

  # Prior Age
  prior <- rep(c(1, 60), 3)
  samplenature = matrix(
    data = c(1, 0, 1, 0, 1, 0),
    ncol = 3,
    nrow = 2,
    byrow = TRUE
  )
  SC <-
    matrix(
      data = c(1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 0),
      ncol = 3,
      nrow = 4,
      byrow = TRUE
    )

  ## Age computation of samples
  expect_s3_class(suppressWarnings(
    Age_OSLC14(
      DATA = Data,
      Data_C14Cal = C14Cal,
      Data_SigmaC14Cal = SigmaC14Cal,
      SampleNames = c("GDB5", Names, "GDB3"),
      Nb_sample = 3,
      SampleNature = samplenature,
      PriorAge = prior,
      StratiConstraints = SC,
      Iter = 50,
      adapt = 50,
      burnin = 50,
      n.chains = 2
    )
  ), class = "BayLum.list")

  ## check  mode via creating an error (everything else takes too long)
  results <- expect_s3_class(suppressWarnings(Age_OSLC14(
      DATA = Data,
      jags_method = 'rjparallel',
      Data_C14Cal = C14Cal,
      Data_SigmaC14Cal = SigmaC14Cal,
      SampleNames = c("GDB5", Names, "GDB3"),
      Nb_sample = 3,
      SampleNature = samplenature,
      PriorAge = prior,
      StratiConstraints = SC,
      Iter = 50,
      burnin = 20,
      adapt = 20,
      n.chains = 2,
      startburnin = 50,
      startsample = 400)), "BayLum.list")

  ##crash function for wrong ordering
  samplenature <-
    matrix(
      data = c(0, 1, 1, 1, 0, 0),
      ncol = 3,
      nrow = 2,
      byrow = TRUE
    )
  expect_error(
    Age_OSLC14(
      DATA = Data,
      Data_C14Cal = C14Cal,
      Data_SigmaC14Cal = SigmaC14Cal,
      SampleNames = c("GDB5", Names, "GDB3"),
      Nb_sample = 3,
      SampleNature = samplenature,
      PriorAge = prior,
      StratiConstraints = SC,
      Iter = 50,
      n.chains = 2
    ),
    "If you see this message, you are probably trying to run the model with a small number of samples."
  )

  ## test output for regression
  expect_silent(plot_Ages(results))
  expect_silent(suppressWarnings(plot_MCMC(results)))
  plot_Scatterplots(results)

})

