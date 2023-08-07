##Full check
test_that("analyse_Al2O3C_Measurements", {
  skip_on_cran()
  local_edition(3)

   ##load data
   data(ExampleData.Al2O3C, envir = environment())

   ##00 - cause function breaks
   expect_error(analyse_Al2O3C_Measurement())
   expect_error(analyse_Al2O3C_Measurement(object = "test"))
   expect_warning(Luminescence:::.warningCatcher(
      analyse_Al2O3C_Measurement(object = data_CrossTalk, signal_integral = 1000)))

   ##run analysis
   expect_s4_class(suppressWarnings(analyse_Al2O3C_Measurement(data_CrossTalk)), "RLum.Results")
   expect_s4_class(suppressWarnings(analyse_Al2O3C_Measurement(data_CrossTalk, calculate_TL_dose = TRUE)),
                                    "RLum.Results")

   ##run test without TL curves
   temp <- get_RLum(data_CrossTalk, recordType = "OSL", drop = FALSE)
   expect_s4_class(suppressWarnings(analyse_Al2O3C_Measurement(temp)), "RLum.Results")

})

