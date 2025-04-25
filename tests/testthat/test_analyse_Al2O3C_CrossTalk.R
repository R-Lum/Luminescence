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
