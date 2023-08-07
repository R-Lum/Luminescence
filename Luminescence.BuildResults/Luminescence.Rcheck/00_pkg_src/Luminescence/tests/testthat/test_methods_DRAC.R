##Full check
test_that("methods_DRAC", {
  testthat::skip_on_cran()
  local_edition(3)

  input <- template_DRAC()

  ## print
  expect_output(print(input, blueprint = TRUE))
  expect_output(print(input, blueprint = FALSE))

  ## as.data.frame
  expect_s3_class(as.data.frame(input), "data.frame")
  expect_s3_class(as.data.frame(input), "DRAC.data.frame")

  ## [[<-
  expect_warning({
    input <- template_DRAC()
    input[[1]] <- 1i
  }, regexp = "cannot use objects of class")
  expect_warning({
    input <- template_DRAC()
    input[[1]] <- c(1, 2)
  }, regexp = "Input must be of length")
  expect_warning({
    input <- template_DRAC()
    input[[5]] <- "1"
  }, regexp = "Input must be of class numeric")

   expect_warning({
    input <- template_DRAC()
    input[[5]] <- "X"
    Luminescence:::.warningCatcher(input[[5]] <- "abc")
  }, regexp = "Cannot coerce < abc > to a numeric value")

  expect_warning({
    input <- template_DRAC(nrow = 2)
    input[[5]] <- c("X", 1)
    Luminescence:::.warningCatcher(input[[5]] <- c("X", "abc"))
  }, regexp = "Cannot coerce < abc > to a numeric value")

  expect_warning({
    input <- template_DRAC()
    input[[5]] <- 1L
    input[[5]] <- "abc"
  }, regexp = "Input must be of class integer")
  expect_warning({
    input <- template_DRAC()
    input[[13]] <- "abc"
  }, regexp = "Invalid option")
  expect_warning({
    input <- template_DRAC()
    input[[13]] <- 1
  }, regexp = "Input must be of class character")

  ## [<-
  expect_identical(
    object = template_DRAC(),
    expected = {
      input <- template_DRAC()
      input[1] <- NA_character_
      input
    })

  ## $<-
  expect_identical(
    object = template_DRAC(),
    expected = {
      input <- template_DRAC()
      input$`Project ID` <- NA_character_
      input
    })


})

