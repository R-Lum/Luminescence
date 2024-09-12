## load data
data(ExampleData.Al2O3C, envir = environment())

test_that("input validation", {
  skip_on_cran()

  SW({
  expect_error(analyse_Al2O3C_Measurement(),
               "is missing, with no default")
  expect_error(analyse_Al2O3C_Measurement("error"),
               "'object' must be an 'RLum.Analysis' object or a list of such objects")
  expect_error(analyse_Al2O3C_Measurement(list(data_CrossTalk, "error")),
               "Elements in 'object' are not all of type 'RLum.Analysis'")
  suppressWarnings(
  expect_error(analyse_Al2O3C_Measurement(data_CrossTalk,
                                          travel_dosimeter = "error"),
               "Input for 'travel_dosimeter' is not numeric")
  )
  expect_error(analyse_Al2O3C_Measurement(data_CrossTalk,
                                          irradiation_time_correction = 7),
               "must have length 2")
  expect_error(analyse_Al2O3C_Measurement(data_CrossTalk,
                                          irradiation = set_RLum("RLum.Results")),
               "was created by an unsupported function")
  expect_error(analyse_Al2O3C_Measurement(data_CrossTalk,
                                          irradiation_time_correction = "a"),
               "must be a numeric vector or an 'RLum.Results' object")
  expect_error(analyse_Al2O3C_Measurement(data_CrossTalk,
                                          cross_talk_correction = "a"),
               "'cross_talk_correction' was created by an unsupported function")

  expect_warning(Luminescence:::.warningCatcher(
                                     analyse_Al2O3C_Measurement(object = data_CrossTalk, signal_integral = 1000)))
  })
})

test_that("analyse_Al2O3C_Measurements", {
  skip_on_cran()

  ## run analysis
  SW({
   expect_s4_class(suppressWarnings(analyse_Al2O3C_Measurement(data_CrossTalk)), "RLum.Results")
   expect_s4_class(suppressWarnings(analyse_Al2O3C_Measurement(data_CrossTalk, calculate_TL_dose = TRUE)),
                                    "RLum.Results")
   })
  expect_output(analyse_Al2O3C_Measurement(data_CrossTalk[[2]],
                        test_parameter = list(stimulation_power = 0.01)))
  expect_output(analyse_Al2O3C_Measurement(data_CrossTalk[[2]],
                                           dose_points = list(3)))

  ## tests without TL curves
  temp <- get_RLum(data_CrossTalk, recordType = "OSL", drop = FALSE)
  SW({
  expect_s4_class(analyse_Al2O3C_Measurement(temp),
                  "RLum.Results")
  expect_output(analyse_Al2O3C_Measurement(temp, travel_dosimeter = 2),
                "travel dosimeter correction applied")
  expect_message(analyse_Al2O3C_Measurement(temp, travel_dosimeter = 1:2),
                 "'travel_dosimeter' specifies every position")
  expect_message(analyse_Al2O3C_Measurement(temp, travel_dosimeter = 2000),
                 "Invalid position in 'travel_dosimeter', nothing corrected")
  })

  ## irradiation_time_correction
  it.corr <- analyse_Al2O3C_ITC(data_ITC, verbose = FALSE)
  analyse_Al2O3C_Measurement(temp, irradiation_time_correction = list(it.corr),
                             plot = 1, verbose = FALSE)

  ## cross_talk_correction
  ct.corr <- analyse_Al2O3C_CrossTalk(data_CrossTalk)
  suppressWarnings( # FIXME(mcol): warnings come from a poorly fitted ct.corr
  analyse_Al2O3C_Measurement(temp, cross_talk_correction = list(ct.corr),
                             plot = FALSE, verbose = FALSE)
  )
})
