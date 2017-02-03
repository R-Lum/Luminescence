context("analyse_portableOSL")

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
    expect_is(results, "RLum.Results")
    expect_equal(length(results), 3)
    expect_is(results$summary, "data.frame")
    expect_is(results$data, "RLum.Analysis")

})

test_that("check output", {
  expect_equal(round(sum(results$summary), digits = 2), 70.44)

})
