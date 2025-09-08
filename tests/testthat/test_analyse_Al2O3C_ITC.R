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
  expect_error(analyse_Al2O3C_ITC(set_RLum("RLum.Analysis")),
               "'object' cannot be an empty RLum.Analysis")
  expect_error(analyse_Al2O3C_ITC(list(data_ITC, "test")),
               "All elements of 'object' should be of class 'RLum.Analysis'")
  expect_error(analyse_Al2O3C_ITC(data_ITC, method_control = "EXP"),
               "'method_control' should be of class 'list'")
  expect_error(analyse_Al2O3C_ITC(data_ITC, dose_points = NA),
               "'dose_points' should be of class 'numeric' or 'list'")
  expect_error(analyse_Al2O3C_ITC(data_ITC, dose_points = list(NA)),
               "All elements of 'dose_points' should be of class 'numeric'")

  SW({
  expect_warning(analyse_Al2O3C_ITC(data_ITC, signal_integral = 0),
                 "Input for 'signal_integral' corrected to 1:99")
  })
})

test_that("check functionality", {
  skip_on_cran()

  ##run analysis
  SW({
  expect_warning(expect_null(analyse_Al2O3C_ITC(list(data_ITC),
                                                dose_points = list(2))),
                 "Nothing was merged as the object list was found to be empty")
  })
})

test_that("snapshot tests", {
  skip_on_cran()

  set.seed(1)
  snapshot.tolerance <- 5.0e-6

  SW({
  expect_snapshot_RLum(analyse_Al2O3C_ITC(data_ITC),
                       tolerance = snapshot.tolerance)
  expect_snapshot_RLum(analyse_Al2O3C_ITC(list(data_ITC),
                                          signal_integral = 2,
                                          method_control = list(fit.method = "EXP")),
                       tolerance = snapshot.tolerance)
  })
})
