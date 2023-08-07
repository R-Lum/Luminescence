test_that("Full function test", {
  testthat::skip_on_cran()
  local_edition(3)


  ## load data
  data(DATA1,envir = environment())
  data(DATA2,envir = environment())

  ##cause function stop
  ##not all are lists
  expect_error(
    combine_DataFiles(DATA2, "DATA1"),
    regexp = "\\[combine_DataFiles\\(\\)\\] This function only accepts 'list' objects as input!")

  ##wrong list elements
  expect_error(combine_DataFiles(DATA2, list(a = "1")),
               regexp = "\\[combine_DataFiles\\(\\)\\] The input objects are not compatible and cannot be combined!")


  ##NULL output
  expect_null(combine_DataFiles(), NULL)


  ##success
  expect_type(combine_DataFiles(DATA2,DATA1), "list")

  ##TODO - trigger deprecated warning
  expect_warning(Concat_DataFile())

})

