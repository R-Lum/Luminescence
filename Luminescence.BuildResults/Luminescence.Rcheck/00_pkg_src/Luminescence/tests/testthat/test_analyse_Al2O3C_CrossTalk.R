context("analyse_Al2O3_CrossTalk")

##Full check
test_that("Full check", {
  skip_on_cran()

   ##load data
   data(ExampleData.Al2O3C, envir = environment())

   ##run analysis
   expect_is(analyse_Al2O3C_CrossTalk(data_CrossTalk), "RLum.Results")


})

