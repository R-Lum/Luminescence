test_that("input validation", {
  skip_on_cran()

  expect_type(get_Layout("empty"),
              "list")
  expect_type(get_Layout("journal"),
              "list")
  expect_warning(res <- get_Layout("error"),
                 "Layout definition not supported, default layout is used")
  expect_equal(res,
               get_Layout("default"))

  input <- list(element = "value")
  expect_equal(get_Layout(input),
               input)
})
