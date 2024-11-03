##Full check
test_that("methods_DRAC", {
  testthat::skip_on_cran()

  input <- template_DRAC(notification = FALSE)

  ## print
  expect_message(
      expect_output(print(input, blueprint = TRUE)),
      "You can copy all lines above to your script and fill in the data")
  expect_output(print(input, blueprint = FALSE))

  ## as.data.frame
  expect_s3_class(as.data.frame(input), "data.frame")
  expect_s3_class(as.data.frame(input), "DRAC.data.frame")

  SW({

  ## [[<-
  expect_warning({
    input <- template_DRAC()
    input[[1]] <- 1i
  }, regexp = "'value' should be of class 'integer', 'character', 'numeric' or")
  expect_warning({
    input <- template_DRAC()
    input[[1]] <- c(1, 2)
  }, regexp = "Input must be of length")
  expect_warning({
    input <- template_DRAC()
    input[[5]] <- "1"
  }, regexp = "found character, expected numeric")

   expect_warning({
    input <- template_DRAC()
    input[[5]] <- "X"
    Luminescence:::.warningCatcher(input[[5]] <- "abc")
  }, regexp = "Cannot coerce 'abc' to a numeric value")

  expect_warning({
    input <- template_DRAC(nrow = 2)
    input[[5]] <- c("X", 1)
    Luminescence:::.warningCatcher(input[[5]] <- c("X", "abc"))
  }, regexp = "Cannot coerce 'abc' to a numeric value")

  input <- template_DRAC(notification = FALSE)
  expect_warning({
    input[[5]] <- 1L
  }, regexp = "found integer, expected numeric")

  expect_warning({
    input[[5]] <- "abc"
  })

  expect_warning({
    input <- template_DRAC()
    input[[13]] <- 1
  }, regexp = "Input must be of class 'character'")

  ## [<-
  expect_identical(
    object = template_DRAC(notification = FALSE),
    expected = {
      input <- template_DRAC(notification = FALSE)
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
})
