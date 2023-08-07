test_that("full example test", {
  testthat::skip_on_cran()
  local_edition(3)
  
  ## crash function
  expect_error(make_Sample(
      book = "error",
      depth = 1,
      geometry = "cuboid",
      height = 0.001,
      n_cores = 1,
      width = 0.001,
      length = 0.001), 
      "\\[make_Sample\\(\\)\\] 'book' is not an object created by sandbox!")
  
  expect_error(make_Sample(
    book = get_RuleBook(), 
    depth = 1, 
    geometry = "cuboid",
    height = 0.001,
    n_cores = 1,
    width = 0.0001, 
    length = 0.0001),"\\[make_Sample\\(\\)\\] Sample volume is smaller than grain diameter!")
  
  ## simple run
  set.seed(12234)
  sample_01 <- expect_s3_class(make_Sample(
    book = get_RuleBook(),
    depth = 1,
    geometry = "cuboid",
    height = 0.001,
    n_cores = 1,
    width = 0.001,
    length = 0.001), "data.frame")
  
  ## check argument
  expect_true(attributes(sample_01)$package == "sandbox")
  
})
  