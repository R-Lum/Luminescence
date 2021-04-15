test_that("Test combine_Dr_De", {
  testthat::skip_on_cran()
  local_edition(3)

  ## simple test using the example
  ## set parameters
  Dr <- stats::rlnorm (1000, 0, 0.3)
  De <-  50*sample(Dr, 50, replace = TRUE)
  s <- stats::rnorm(50, 10, 2)

  ## set seed
  set.seed(1276)

  ## simple run
  results <- expect_s4_class(combine_Dr_De(
    Dr = Dr,
    int_OD = 0.1,
    De,
    s,
    outlier_analysis_plot = TRUE,
    Age_range = c(0, 100),
    method_control = list(n.iter = 100,
                          n.chains = 1)), "RLum.Results")


  ## check the length of the output
  expect_length(results, 3)

  ## try to plot the results again
  plot_OSLAgeSummary(results)




})
