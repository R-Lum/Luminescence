data("ExampleData.portableOSL", envir = environment())
merged <- merge_RLum(ExampleData.portableOSL)
results <-
  analyse_portableOSL(
    merged,
    signal.integral = 1:5,
    invert = FALSE,
    normalise = TRUE,
    plot = FALSE
  )

test_that("check class and length of output", {
    testthat::skip_on_cran()
    local_edition(3)

    expect_s4_class(results, "RLum.Results")
    expect_equal(length(results), 3)
    expect_s3_class(results$summary, "data.frame")
    expect_s4_class(results$data, "RLum.Analysis")

    ## check list input
    expect_s4_class(
      suppressWarnings(analyse_portableOSL(ExampleData.portableOSL)),
      "RLum.Results")

    ## check additional argument sample
    expect_s4_class(analyse_portableOSL(
      merged,
      signal.integral = 1:5,
      invert = FALSE,
      normalise = TRUE,
      plot = TRUE,
      sample = "test"
    ),
    "RLum.Results")


})

test_that("check output", {
  testthat::skip_on_cran()
  local_edition(3)

  expect_equal(round(sum(results$summary[,-1]), digits = 2), 70.44)

})
