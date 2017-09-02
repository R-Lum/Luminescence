context("analyse_Al2O3_ITC")

##Full check
test_that("Full check", {
  skip_on_cran()

   ##check stops
   ##RLum-object
   expect_error(object = analyse_Al2O3_ITC(object = "test"))

   ##input curve type
   a <- set_RLum(class = "RLum.Data.Curve", recordType = "OSL", data = matrix(1:20, ncol = 2))
   b <- set_RLum(class = "RLum.Data.Curve", recordType = "TL")
   object <- set_RLum(class = "RLum.Analysis", records = list(a,b))
   expect_error(object = analyse_Al2O3_ITC(object))

   ##signal_integral
   object <- set_RLum(class = "RLum.Analysis", records = list(a, a))
   #expect_warning(object = analyse_Al2O3_ITC(object, signal_integral = 1:100))



})

