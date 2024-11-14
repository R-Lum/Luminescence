## load data
data(ExampleData.Al2O3C, envir = environment())

test_that("input validation", {
  skip_on_cran()

  expect_error(analyse_Al2O3C_CrossTalk("test"),
               "All elements of 'object' should be of class 'RLum.Analysis'")
  expect_error(analyse_Al2O3C_CrossTalk(data_CrossTalk,
                                        method_control = "EXP"),
               "'method_control' should be of class 'list'")
  expect_error(analyse_Al2O3C_CrossTalk(data_CrossTalk,
                                        irradiation_time_correction = FALSE),
               "'irradiation_time_correction' should be of class 'numeric' or")
  corr <- set_RLum("RLum.Results")
  expect_error(analyse_Al2O3C_CrossTalk(data_CrossTalk,
                                        irradiation_time_correction = corr),
               "was created by an unsupported function")
  expect_warning(analyse_Al2O3C_CrossTalk(data_CrossTalk,
                                          signal_integral = 0),
                 "'signal_integral' corrected to 1:99")
})

test_that("check functionality", {
  skip_on_cran()

  set.seed(1)
  snapshot.tolerance <- 1.5e-5

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
