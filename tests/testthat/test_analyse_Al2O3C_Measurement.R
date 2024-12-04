## load data
data(ExampleData.Al2O3C, envir = environment())

test_that("input validation", {
  skip_on_cran()

  SW({
  expect_error(analyse_Al2O3C_Measurement(),
               "is missing, with no default")
  expect_error(analyse_Al2O3C_Measurement("error"),
               "'object' should be of class 'RLum.Analysis' or a 'list' of such objects")
  expect_error(analyse_Al2O3C_Measurement(list()),
               "'object' cannot be an empty list")
  expect_error(analyse_Al2O3C_Measurement(list(data_CrossTalk, "error")),
               "All elements of 'object' should be of class 'RLum.Analysis'")
  suppressWarnings(
  expect_error(analyse_Al2O3C_Measurement(data_CrossTalk,
                                          travel_dosimeter = "error"),
               "'travel_dosimeter' should be of class 'numeric' or 'integer'")
  )
  expect_error(analyse_Al2O3C_Measurement(data_CrossTalk,
                                          irradiation_time_correction = 7),
               "'irradiation_time_correction' should have length 2")
  expect_error(analyse_Al2O3C_Measurement(data_CrossTalk,
                                          irradiation = set_RLum("RLum.Results")),
               "was created by an unsupported function")
  expect_error(analyse_Al2O3C_Measurement(data_CrossTalk,
                                          irradiation_time_correction = "a"),
               "should be of class 'RLum.Results' or 'numeric'")
  expect_error(analyse_Al2O3C_Measurement(data_CrossTalk,
                                          cross_talk_correction = "a"),
               "'cross_talk_correction' was created by an unsupported function")

  expect_warning(Luminescence:::.warningCatcher(
                                     analyse_Al2O3C_Measurement(object = data_CrossTalk, signal_integral = 1000)))
  })
})

test_that("check functionality", {
  skip_on_cran()

  ## run analysis
  SW({ # warning: TL peak shift detected for aliquot position 1
  expect_s4_class(analyse_Al2O3C_Measurement(data_CrossTalk),
                  "RLum.Results")
  expect_s4_class(analyse_Al2O3C_Measurement(data_CrossTalk,
                                             calculate_TL_dose = TRUE),
                  "RLum.Results")
  expect_output(analyse_Al2O3C_Measurement(data_CrossTalk,
                        test_parameter = list(stimulation_power = 0.03)))
  expect_output(analyse_Al2O3C_Measurement(data_CrossTalk,
                        test_parameter = list(list(stimulation_power = 0.1),
                                              list(stimulation_power = 0.2))))
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
  analyse_Al2O3C_Measurement(temp, cross_talk_correction = list(ct.corr),
                             plot = FALSE, verbose = FALSE)
})

test_that("more coverage", {
  skip_on_cran()

  ## failed integration
  CT.mod <- data_CrossTalk
  CT.mod[[1]]@records[[2]]@data <- CT.mod[[1]]@records[[2]]@data[1:5, ]
  CT.mod[[1]]@records[[4]]@data <- CT.mod[[1]]@records[[4]]@data[1:5, ]
  warnings <- capture_warnings(
      analyse_Al2O3C_Measurement(CT.mod, verbose = FALSE,
                                 calculate_TL_dose = TRUE))
  expect_match(warnings, all = FALSE,
               "Natural TL signal out of bounds, NA returned")
  expect_match(warnings, all = FALSE,
               "Regenerated TL signal out of bounds, NA returned")

  ## missing position
  CT.mod <- data_CrossTalk
  CT.mod[[1]]@records[[1]]@info["position"] <- list(NULL)
  expect_message(expect_warning(
      analyse_Al2O3C_Measurement(CT.mod, verbose = FALSE),
      "TL peak shift detected for aliquot position NA"),
      "Error: Aliquot position not found, no cross-talk correction applied")

  CT.mod <- data_CrossTalk
  for (i in seq_along(CT.mod[[1]]@records)) {
    CT.mod[[1]]@records[[i]]@info$stimulator <- "LED"
  }
  suppressWarnings( # TL peak shift detected for aliquot position 1
  expect_warning(
      analyse_Al2O3C_Measurement(CT.mod, verbose = FALSE,
                                 test_parameter = list(stimulation_power = 0.03)),
      "Stimulation power was not stable for ALQ 1, results are likely to be wrong"))
})
