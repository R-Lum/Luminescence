##load data
data(ExampleData.Al2O3C, envir = environment())

test_that("input validation", {
  skip_on_cran()

  a <- set_RLum(class = "RLum.Data.Curve", recordType = "OSL",
                data = matrix(1:20, ncol = 2))
  b <- set_RLum(class = "RLum.Data.Curve", recordType = "TL")
  object <- set_RLum(class = "RLum.Analysis", records = list(a,b))

  expect_error(analyse_Al2O3C_ITC("test"),
               "'object' should be of class 'RLum.Analysis'")
  expect_error(analyse_Al2O3C_ITC(list(data_ITC, "test")),
               "All elements of 'object' should be of class 'RLum.Analysis'")
  expect_error(analyse_Al2O3C_ITC(data_ITC, method_control = "EXP"),
               "'method_control' should be of class 'list'")
#  expect_error(analyse_Al2O3C_ITC(data_ITC, dose_points = list(NA)),
#               "At least three regeneration points are required") XXX!

  SW({
  expect_warning(analyse_Al2O3C_ITC(data_ITC, signal_integral = 0),
                 "Input for 'signal_integral' corrected to 1:99")
  })
})

test_that("Full check", {
  skip_on_cran()

  ##run analysis
  SW({
  expect_s4_class(analyse_Al2O3C_ITC(data_ITC), "RLum.Results")
  expect_s4_class(analyse_Al2O3C_ITC(list(data_ITC), signal_integral = 2,
                                     method_control = list(fit.method = "EXP")),
                  "RLum.Results")
  expect_warning(expect_null(analyse_Al2O3C_ITC(list(data_ITC),
                                                dose_points = list(2))),
                 "Nothing was merged as the object list was found to be empty")
  })
})
