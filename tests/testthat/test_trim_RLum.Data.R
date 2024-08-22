test_that("RLum.Data.Curve", {
  testthat::skip_on_cran()

  data(ExampleData.BINfileData, envir = environment())
  temp <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1)

  ## trim with range
  t <- testthat::expect_type(
    object = trim_RLum.Data(temp$TL, trim_range = c(20,50)),
    type = "list")

  ## check output length
  testthat::expect_length(
    object = t[[1]]@data[,1], n = 31)

  ## trim maximum only
  t <- testthat::expect_type(
    object = trim_RLum.Data(temp$TL, trim_range = c(50)),
    type = "list")

  ## check output length
  testthat::expect_length(
    object = t[[1]]@data[,1], n = 50)

  ## trim with nothing
  t <- testthat::expect_type(
    object = trim_RLum.Data(temp$TL, trim_range = NULL),
    type = "list")

  ## check output length
  testthat::expect_length(
    object = t[[1]]@data[,1], n = 250)

  ## trim with wrong curve type ... this should do nothing
  ## because the object is just parsed through
  ## on top we use a single object instead of list only
  t <- testthat::expect_s4_class(
    object = trim_RLum.Data(temp@records[[1]], recordType = "OSL", trim_range = NULL),
    class = "RLum.Data.Curve")

  ## check output length
  testthat::expect_length(
    object = t@data[,1], n = 250)

  ## test some edge cases from trim
  ## 1
  testthat::expect_s4_class(
    object = trim_RLum.Data(temp@records[[1]], trim_range = c(1)),
    class = "RLum.Data.Curve")
  ## 0
  testthat::expect_s4_class(
    object = trim_RLum.Data(temp@records[[1]], trim_range = c(0)),
    class = "RLum.Data.Curve")
  ## -1
  testthat::expect_s4_class(
    object = trim_RLum.Data(temp@records[[1]], trim_range = c(-1)),
    class = "RLum.Data.Curve")
  ## c(1,2,3)
  testthat::expect_s4_class(
    object = trim_RLum.Data(temp@records[[1]], trim_range = c(1:3)),
    class = "RLum.Data.Curve")


})

test_that("RLum.Data.Spectrum", {
  testthat::skip_on_cran()

  ## simple test for RLum.Data.Spectrum ... this can be kept
  ## simple because everything else was tested already
  data(ExampleData.XSYG, envir = environment())

  t <- testthat::expect_s4_class(
    object = trim_RLum.Data(TL.Spectrum, trim_range = c(2, 4)),
    class = "RLum.Data.Spectrum")

  testthat::expect_length(object = t@data[1,], n = 3)

})

test_that("RLum.Data.Image", {
  testthat::skip_on_cran()

  ## simple test for RLum.Data.Spectrum ... this can be kept
  ## simple because everything else was tested already
  data(ExampleData.RLum.Data.Image, envir = environment())

  testthat::expect_s4_class(
    object = trim_RLum.Data(ExampleData.RLum.Data.Image, recordType = c(10,100)),
    class = "RLum.Data.Image")

})

test_that("RLum.Analysis", {
  testthat::skip_on_cran()

  ##load example data
  data(ExampleData.BINfileData, envir = environment())
  temp <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1)

  ## generate case where one OSL curve has one channel less
  temp@records[[2]]@data <- temp@records[[2]]@data[-nrow(temp[[2]]@data),]

  ## now all OSL curves should be shortened to 999
  t <- testthat::expect_s4_class(
    object = trim_RLum.Data(temp),
    class = "RLum.Analysis")

 ## check for OSL, the TL must remain untouched
 testthat::expect_length(
   object = t@records[[4]]@data[,1], n = 999)
 testthat::expect_length(
   object = t@records[[1]]@data[,1], n = 250)

 ## apply a trimming to all curves
 t <- testthat::expect_s4_class(
   object = trim_RLum.Data(temp, trim_range = c(10,20)),
   class = "RLum.Analysis")

 ## check for two curves
 testthat::expect_length(
   object = t@records[[4]]@data[,1], n = 11)
 testthat::expect_length(
   object = t@records[[1]]@data[,1], n = 11)

 ## apply a trimming to TL curves only
 t <- testthat::expect_s4_class(
   object = trim_RLum.Data(temp, recordType = "OSL", trim_range = c(10,20)),
   class = "RLum.Analysis")
 ## check for two curves
 testthat::expect_length(
   object = t@records[[4]]@data[,1], n = 11)
 testthat::expect_length(
   object = t@records[[1]]@data[,1], n = 250)

})


test_that("Crash function", {
  testthat::skip_on_cran()

  ## trigger stop
  testthat::expect_error(
    object = trim_RLum.Data("error"),
    regexp = "\\[trim\\_RLum.Data\\(\\)\\] Unsupported input class\\!")
})
