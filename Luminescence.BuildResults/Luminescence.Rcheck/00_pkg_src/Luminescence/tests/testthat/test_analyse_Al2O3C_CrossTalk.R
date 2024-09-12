test_that("Full check", {
  skip_on_cran()

   ##load data
   data(ExampleData.Al2O3C, envir = environment())

  ## run analysis
  expect_s4_class(analyse_Al2O3C_CrossTalk(data_CrossTalk), "RLum.Results")
  res <- expect_s4_class(
      analyse_Al2O3C_CrossTalk(data_CrossTalk,
                               method_control = list(fit.method = "LIN")),
      "RLum.Results")

  ## input validation
  expect_error(analyse_Al2O3C_CrossTalk("test"),
               "The elements in 'object' are not all of type 'RLum.Analysis'")
  expect_error(analyse_Al2O3C_CrossTalk(data_CrossTalk,
                                        method_control = "EXP"),
               "'method_control' is expected to be a list")
  expect_error(analyse_Al2O3C_CrossTalk(data_CrossTalk,
                                        irradiation_time_correction = FALSE),
               "'irradiation_time_correction' is expected to be")
  expect_error(analyse_Al2O3C_CrossTalk(data_CrossTalk,
                                        irradiation_time_correction = res),
               "was created by an unsupported function")
  expect_warning(analyse_Al2O3C_CrossTalk(data_CrossTalk,
                                          signal_integral = 0),
                 "Input for 'signal_integral' corrected to")

  ## irradiation_time_correction
  SW({
  corr <- analyse_Al2O3C_ITC(data_ITC)
  })
  expect_s4_class(
      analyse_Al2O3C_CrossTalk(data_CrossTalk,
                               irradiation_time_correction = corr),
      "RLum.Results")
  corr@data$data <- rbind(corr@data$data, corr@data$data)
  expect_s4_class(
      analyse_Al2O3C_CrossTalk(data_CrossTalk,
                               irradiation_time_correction = corr),
      "RLum.Results")
})
