test_that("Check .as.latex.table()", {
  testthat::skip_on_cran()

  ## data.frame
  df <- data.frame(x = "test", y = 1:10, z = as.factor(letters[1:10]))
  expect_error(.as.latex.table(data.frame()),
               "'x' cannot be an empty data.frame")
  expect_error(.as.latex.table(df, comments = iris),
               "'comments' should be a single logical value")
  expect_error(.as.latex.table(df, rm.zero = NA),
               "'rm.zero' should be a single logical value")
  expect_error(.as.latex.table(df, select = 2),
               "'select' should be of class 'character' or NULL")
  expect_error(.as.latex.table(df, select = character()),
               "'select' cannot be an empty character")
  expect_error(.as.latex.table(df, select = "error"),
               "Invalid 'select' column name, valid column names are: 'x', 'y', 'z'")
  expect_error(.as.latex.table(df, tabular_only = 2:3),
               "'tabular_only' should be a single logical value")

  expect_output(.as.latex.table(df))
  expect_output(.as.latex.table(df, select = "x"))

  ## RLum.Results
  res <- as(object = list(1:10), Class = "RLum.Results")
  expect_error(.as.latex.table(res, comments = iris),
               "'comments' should be a single logical value")
  expect_error(.as.latex.table(res, rm.zero = NA),
               "'rm.zero' should be a single logical value")
  expect_null(.as.latex.table(set_RLum("RLum.Results")))
  expect_null(.as.latex.table(res))

  ## DRAC highlights
  dh <- use_DRAC(template_DRAC(preset = "DRAC-example_quartz",
                               notification = FALSE), verbose = FALSE)
  expect_output(.as.latex.table(dh))
})

test_that("Check .as.latex.table.data.frame()", {
  testthat::skip_on_cran()

  expect_error(.as.latex.table.data.frame("error"),
               "'x' should be of class 'data.frame'")

  df <- data.frame(x = "test", y = 1:10)
  expect_error(.as.latex.table(df, col.names = "col1"),
               "Length of 'col.names' does not match the number of columns")
  expect_error(.as.latex.table(df, row.names = "row1"),
               "Length of 'row.names' does not match the number of rows")
  expect_error(.as.latex.table(df, pos = c("pos1", "pos2")),
               "Length of 'pos' does not match the number of columns")
  expect_error(.as.latex.table(df, comments = iris),
               "'comments' should be a single logical value")
  expect_error(.as.latex.table(df, verbose = NA),
               "'verbose' should be a single logical value")

  expect_output(.as.latex.table(df, tabular_only = TRUE))
  expect_output(.as.latex.table(df[, 1, drop = FALSE], pos = "error"))
})
