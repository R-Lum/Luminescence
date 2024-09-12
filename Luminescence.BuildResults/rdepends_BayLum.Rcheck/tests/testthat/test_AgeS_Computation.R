test_that("Full function test", {
  testthat::skip_on_cran()
  local_edition(3)

  ## load data
  data(DATA1,envir = environment())
  data(DATA2,envir = environment())
  Data <- combine_DataFiles(DATA2,DATA1)

  ## without common error and without stratigraphic constraints
  expect_s3_class(suppressWarnings(AgeS_Computation(
    DATA = Data,
    Nb_sample = 2,
    SampleNames = c("GDB5","GDB3"),
    PriorAge = c(1,10,20,60),
    Iter = 50,
    adapt = 50,
    burnin = 50,
    n.chains = 2,
    quiet = TRUE
  )), class = "BayLum.list")

  ## test parallel computation
  expect_s3_class(suppressWarnings(AgeS_Computation(
    DATA = Data,
    Nb_sample = 2,
    SampleNames = c("GDB5","GDB3"),
    PriorAge = c(1,10,20,60),
    Iter = 50,
    quiet = FALSE,
    jags_method = "rjparallel",
    n.chains = 2,
    adapt = 20,
    burnin = 20
  )), class = "BayLum.list")

  ## check additional functionality
  ## THETA matrix (this was former bug)
  df <- suppressMessages(create_ThetaMatrix())
  df <- rbind(df,df)
  df[] <- 1

  sigma_s <-  c(
    s_betaK = 0.010,
    s_betaU = 0.007,
    s_betaTh = 0.006,
    s_gammaK = 0.010,
    s_gammaU = 0.007,
    s_gammaTh = 0.006,
    s_gammaDR = 0.05,
    s_CAL = 0.020,
    s_intDR = 0.030)

  THETA_file <- tempfile(fileext = ".csv")
  THETA <- suppressWarnings(create_ThetaMatrix(input = df, output_file = THETA_file, sigma_s = sigma_s))

  ## without common error and without stratigraphic constraints
  ##sigma_s
  expect_s3_class(suppressWarnings(AgeS_Computation(
    DATA = Data,
    Nb_sample = 2,
    THETA = THETA,
    SampleNames = c("GDB5","GDB3"),
    PriorAge = c(1,10,20,60),
    Iter = 50,
    adapt = 50,
    burnin = 50,
    n.chains = 2,
    quiet = TRUE
  )), class = "BayLum.list")

  expect_s3_class(suppressWarnings(AgeS_Computation(
    DATA = Data,
    Nb_sample = 2,
    THETA = THETA_file,
    SampleNames = c("GDB5","GDB3"),
    PriorAge = c(1,10,20,60),
    Iter = 50,
    adapt = 50,
    burnin = 50,
    n.chains = 2,
    quiet = TRUE
  )), class = "BayLum.list")

  ## let THETA be a data.frame
  results <- expect_s3_class(suppressWarnings(AgeS_Computation(
    DATA = Data,
    Nb_sample = 2,
    THETA = as.data.frame(THETA),
    SampleNames = c("GDB5","GDB3"),
    PriorAge = c(1,10,20,60),
    Iter = 50,
    adapt = 50,
    burnin = 50,
    n.chains = 2,
    quiet = TRUE
  )), class = "BayLum.list")

  ## test output for regression
  expect_silent(plot_Ages(results))
  expect_silent(suppressWarnings(plot_MCMC(results)))
  plot_Scatterplots(results)

})

