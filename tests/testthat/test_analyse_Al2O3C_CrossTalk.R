## load data
data(ExampleData.Al2O3C, envir = environment())

test_that("input validation", {
  skip_on_cran()

  expect_error(analyse_Al2O3C_CrossTalk("error"),
               "'object' should be of class 'RLum.Analysis' or 'list")
  expect_error(analyse_Al2O3C_CrossTalk(list("error")),
               "All elements of 'object' should be of class 'RLum.Analysis'")
  expect_error(analyse_Al2O3C_CrossTalk(list()),
               "'object' cannot be an empty list")
  expect_error(analyse_Al2O3C_CrossTalk(set_RLum("RLum.Analysis")),
               "'object' cannot be an empty RLum.Analysis")
  expect_error(analyse_Al2O3C_CrossTalk(data_CrossTalk, signal_integral = NA),
               "'signal_integral' should be of class 'integer', 'numeric' or NULL")
  expect_error(analyse_Al2O3C_CrossTalk(data_CrossTalk, dose_points = NA),
               "'dose_points' should be of class 'numeric' or 'integer'")
  expect_error(analyse_Al2O3C_CrossTalk(data_CrossTalk, dose_points = numeric(0)),
               "'dose_points' cannot be an empty numeric")
  expect_error(analyse_Al2O3C_CrossTalk(data_CrossTalk, dose_points = 1:3),
               "'dose_points' should have length 1 or divisible by 2")
  expect_error(analyse_Al2O3C_CrossTalk(data_CrossTalk,
                                        recordType = list(NA)),
               "'recordType' should be of class 'character'")
  expect_error(analyse_Al2O3C_CrossTalk(data_CrossTalk,
                                        recordType = "error"),
               "'object' contains no records with recordType = 'error'")
  expect_error(analyse_Al2O3C_CrossTalk(data_CrossTalk,
                                        method_control = "EXP"),
               "'method_control' should be of class 'list'")
  expect_error(analyse_Al2O3C_CrossTalk(data_CrossTalk,
                                        irradiation_time_correction = FALSE),
               "'irradiation_time_correction' should be of class 'numeric',")
  corr <- set_RLum("RLum.Results")
  expect_error(analyse_Al2O3C_CrossTalk(data_CrossTalk,
                                        irradiation_time_correction = corr),
               "'irradiation_time_correction' has an unsupported originator")
  expect_error(analyse_Al2O3C_CrossTalk(data_CrossTalk,
                                        signal_integral = 0),
               "is of length 0 after removing values smaller than 1 and greater than 99")
})

test_that("check functionality", {
  skip_on_cran()

  ## integral_input
  res1 <- analyse_Al2O3C_CrossTalk(data_CrossTalk[[1]],
                                   signal_integral = c(42.1, 42.4),
                                   integral_input = "measurement")
  res2 <- analyse_Al2O3C_CrossTalk(data_CrossTalk[[1]],
                                   signal_integral = 1:4,
                                   integral_input = "channel")
  res1@info <- res2@info <- list() # remove $call
  res1@data$fit <- res2@data$fit <- NULL
  res1@.uid <- res2@.uid <- NA_character_
  expect_equal(res1, res2)

  expect_warning(analyse_Al2O3C_CrossTalk(data_CrossTalk[[1]],
                                          signal_integral = 1:4,
                                          integral_input = "measurement"),
                 "from time to channels failed: expected values in 42.1:51.9")
})

test_that("snapshot tests", {
  skip_on_cran()

  set.seed(1)
  snapshot.tolerance <- 1.7e-5

  ## run analysis
  expect_snapshot_RLum(
      analyse_Al2O3C_CrossTalk(data_CrossTalk),
      tolerance = snapshot.tolerance)
  expect_snapshot_RLum(
      analyse_Al2O3C_CrossTalk(data_CrossTalk,
                               method_control = list(fit.method = "LIN")),
      tolerance = snapshot.tolerance)

  ## irradiation_time_correction
  SW({
  corr <- analyse_Al2O3C_ITC(data_ITC)
  })
  expect_snapshot_RLum(
      analyse_Al2O3C_CrossTalk(data_CrossTalk,
                               irradiation_time_correction = corr),
      tolerance = snapshot.tolerance)

  corr@data$data <- rbind(corr@data$data, corr@data$data)
  expect_snapshot_RLum(
      analyse_Al2O3C_CrossTalk(data_CrossTalk,
                               irradiation_time_correction = corr),
      tolerance = snapshot.tolerance)
})

test_that("regression tests", {
  skip_on_cran()

  ## issue 1182
  expect_silent(analyse_Al2O3C_CrossTalk(data_ITC))
})
