test_that("Check .as.latex.table()", {
  testthat::skip_on_cran()

  ## data.frame
  df <- data.frame(x = "test", y = 1:10, z = as.factor(letters[1:10]))
  expect_error(.as.latex.table(df, select = 2),
               "Undefined columns selected")

  expect_output(.as.latex.table(df))
  expect_output(.as.latex.table(df, select = "x"))

  ## RLum.Results
  expect_null(.as.latex.table(as(object = list(1:10),
                                 Class = "RLum.Results")))

  ## DRAC highlights
  dh <- use_DRAC(template_DRAC(preset = "DRAC-example_quartz",
                               notification = FALSE), verbose = FALSE)
  expect_output(.as.latex.table(dh))

})

test_that("Check .as.latex.table.data.frame()", {
  testthat::skip_on_cran()

  expect_error(.as.latex.table.data.frame("error"),
               "'x' must be a data frame")

  df <- data.frame(x = "test", y = 1:10)
  expect_error(.as.latex.table.data.frame(df, col.names = "col1"),
               "Length of 'col.names' does not match the number of columns")
  expect_error(.as.latex.table.data.frame(df, row.names = "row1"),
               "Length of 'row.names' does not match the number of rows")
  expect_error(.as.latex.table.data.frame(df, pos = c("pos1", "pos2")),
               "Length of 'pos' does not match the number of columns")

  expect_output(.as.latex.table.data.frame(df, tabular_only = TRUE))
  expect_output(.as.latex.table.data.frame(df[, 1, drop = FALSE]))
})
