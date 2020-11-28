##Full check
test_that("Full check", {
  skip_on_cran()
   local_edition(3)

   ##load data
   data(ExampleData.Al2O3C, envir = environment())

   ##run analysis
   expect_s4_class(analyse_Al2O3C_CrossTalk(data_CrossTalk), "RLum.Results")

})

