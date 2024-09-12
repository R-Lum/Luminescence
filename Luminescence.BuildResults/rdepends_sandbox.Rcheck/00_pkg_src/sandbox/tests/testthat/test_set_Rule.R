test_that("full example test", {
  testthat::skip_on_cran()
  local_edition(3)
  
  ## prepare something to set a rule for
  book_01 <- get_RuleBook(book = "empty")
  
  ## assign rule definitions to lists
  depth <- list(c(0, 10))
  age <- list(c(0, 1000))
  
  ## just let it run
  book_01 <- expect_type(set_Rule(
    book = book_01,
    parameter = "age",
    value = age,
    depth = depth
  ),
  "list")
  
  ## check length
  expect_length(book_01, 6)
  
  ## the book should maintain the attributes, check
  expect_true(all(c("sandbox", "book") %in% attributes(book_01)[c("package", "medium")]))
  
  ## trigger warning
  expect_warning(set_Rule(
    book = book_01,
    parameter = "age",
    value = age,
    depth = depth, 
    type = "warning"
  ), "Interpolation method unavailable. Spline is used!")
  
  ## crash function -- not a legal medium
  expect_error(set_Rule("error"), "\\[set_Rule\\(\\)\\] 'book' is not an object created by sandbox!")
  
  ## parameter not recognised
  expect_error(set_Rule(
    book = book_01,
    parameter = "nono",
    value = age,
    depth = depth
  ), "\\[set_Rule\\(\\)\\] Parameter name not present in rule book\\!\\n.+")
  
})
  