context("analyse_Al2O3C_Measurement")

##Full check
test_that("Full check", {
  skip_on_cran()

   ##load data
   data(ExampleData.Al2O3C, envir = environment())

   ##00 - cause function breaks
   expect_error(analyse_Al2O3C_Measurement())
   expect_error(analyse_Al2O3C_Measurement(object = "test"))
   expect_warning(analyse_Al2O3C_Measurement(object = data_CrossTalk, signal_integral = 1000))

   ##run analysis
   expect_is(analyse_Al2O3C_Measurement(data_CrossTalk), "RLum.Results")
   expect_is(analyse_Al2O3C_Measurement(data_CrossTalk, calculate_TL_dose = TRUE), "RLum.Results")


})

