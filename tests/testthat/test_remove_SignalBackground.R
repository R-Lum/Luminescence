## load data
xsyg <- read_XSYG2R(
  system.file("extdata/XSYG_file.xsyg", package = "Luminescence"),
  fastForward = TRUE,
  verbose = FALSE
)

xsyg_v1 <- set_RLum("RLum.Analysis", records = list(
  xsyg[[1]]@records[[1]],
  xsyg[[1]]@records[[4]],
  xsyg[[1]]@records[[4]],
  xsyg[[1]]@records[[10]],
  xsyg[[1]]@records[[4]],
  xsyg[[1]]@records[[4]]
  ))

test_that("test paramters", {
  testthat::skip_on_cran()

  ## test input object failure for object
  expect_error(
    object = remove_SignalBackground(object = "error"),
    regexp = "'object' should be of class 'RLum.Analysis'")

  ## test input object failure for object_bg
  expect_error(
    object = remove_SignalBackground(object = xsyg[[1]], object_bg = "error"),
    regexp = "'object_bg' should be of class 'RLum.Data.Curve', 'list', 'matrix', 'numeric' or 'integer'")

  ## test input object failure for recordType
  expect_error(
    object = remove_SignalBackground(object = xsyg[[1]], recordType = 1),
    regexp = "'recordType' should be of class 'character'")

  ## test input object failure for clean_up
  expect_error(
    object = remove_SignalBackground(object = xsyg[[1]], clean_up = "error"),
    regexp = "'clean_up' should be of class 'logical'")

})

test_that("simple run", {
  testthat::skip_on_cran()

  ## with clean_up
  t <- expect_s4_class(remove_SignalBackground(xsyg_v1), "RLum.Analysis")
  expect_equal(length(t@records), expected = 4)

  ## without clean_up
  t <- expect_s4_class(remove_SignalBackground(xsyg_v1, clean_up = FALSE), "RLum.Analysis")
  expect_equal(length(t@records), expected = 6)

  ## with clean_up
  t <- expect_s4_class(remove_SignalBackground(xsyg_v1), "RLum.Analysis")
  expect_equal(length(t@records), expected = 4)

  ## with invalid recordType set
  expect_warning(remove_SignalBackground(xsyg_v1, recordType = "error"),
                 regexp = "'recordType' setting invalid, nothing removed.")

  ## test the list treatment
  t <- expect_type(remove_SignalBackground(list(xsyg_v1, xsyg_v1)), "list")
  expect_equal(length(t), expected = 2)

  ## pass non wanted argument
  t <- expect_type(remove_SignalBackground(list(xsyg_v1, "error")), "list")
  expect_equal(length(t), expected = 1)

  ## test with different inputs for object_bg
  ## RLum.Data.Curve()
  expect_s4_class(remove_SignalBackground(
    object = xsyg_v1, recordType = "OSL (UVVIS)",
    object_bg = xsyg_v1[[2]]), "RLum.Analysis")
  ## list
  expect_s4_class(remove_SignalBackground(
    object = xsyg_v1, recordType = "OSL (UVVIS)",
    object_bg = c(xsyg_v1[[2]], xsyg_v1[[2]])), "RLum.Analysis")
  ## matrix
  expect_s4_class(remove_SignalBackground(
    object = xsyg_v1, recordType = "OSL (UVVIS)",
    object_bg = matrix(runif(100,1,100), ncol = 2)), "RLum.Analysis")
  ## numeric
  expect_s4_class(remove_SignalBackground(
    object = xsyg_v1, recordType = "OSL (UVVIS)",
    object_bg = c(runif(100,1,100))), "RLum.Analysis")

})
