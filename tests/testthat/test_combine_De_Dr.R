## load data
set.seed(1276)
Dr <- stats::rlnorm(1000, 0, 0.3)
De <- 50*sample(Dr, 50, replace = TRUE)
s <- stats::rnorm(50, 10, 2)

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(combine_De_Dr("error"),
               "'De' should be of class 'numeric'")
  expect_error(combine_De_Dr(De, "error"),
               "'s' should be of class 'numeric'")
  expect_error(combine_De_Dr(De, s, "error"),
               "'Dr' should be of class 'numeric'")
  expect_error(combine_De_Dr(De, s[-1], Dr, int_OD = 0.1),
               "'De' and 's' should have the same length")
  expect_error(combine_De_Dr(De, s, Dr, int_OD = -1),
               "'int_OD' should be a single non-negative value")
  expect_error(combine_De_Dr(De, s, Dr, int_OD = 0.1, Age_range = 1:4),
               "'Age_range' should be of class 'numeric' and have length 2")
  expect_error(combine_De_Dr(De, s, Dr, int_OD = 0.1, Age_range = c(1, NA)),
               "'Age_range' cannot contain missing values")
  expect_error(combine_De_Dr(De, s, Dr, int_OD = 0.1, outlier_threshold = NA),
               "'outlier_threshold' should be a single positive value")
  expect_error(combine_De_Dr(De, s, Dr, int_OD = 0.1, outlier_method = NA),
               "'outlier_method' should be of class 'character' and have length 1")
  SW({
  expect_error(combine_De_Dr(De, s, Dr, int_OD = 0.1,
                             method_control = list(n.chains = 1, diag = TRUE,
                                                   n.iter = 20)),
               "You need at least two chains")
  expect_error(combine_De_Dr(De, s, Dr, int_OD = 0.1,
                             method_control = list(n.chains = 2,
                                                   inits = list(
                                                       list(
                                                           .RNG.name = "base::Wichmann-Hill",
                                                           .RNG.seed = 1)
                                                   ))),
               "Length mismatch between inits and n.chains")
  })
})

test_that("check functionality", {
  testthat::skip_on_cran()

  SW({
  ## simple run with standard settings
  results <- expect_s4_class(combine_De_Dr(
    Dr = Dr,
    int_OD = 0.1,
    De,
    s,
    outlier_analysis_plot = TRUE,
    Age_range = c(0, 100),
    cdf_ADr_quantiles = FALSE,
    legend.pos = "topright",
    legend = TRUE,
    method_control = list(n.iter = 100,
                          n.chains = 1)), "RLum.Results")
  })

  ## check whether mcmc is NULL
  expect_null(results$mcmc_IAM)
  expect_null(results$mcmc_BCAM)

  ## run the same with different par settings
  oldpar <- par(mfrow = c(2,2))
  SW({
  results <- expect_s4_class(combine_De_Dr(
    Dr = Dr,
    int_OD = 0.1,
    De,
    s,
    outlier_analysis_plot = TRUE,
    par_local = FALSE,
    Age_range = c(0, 100),
    method_control = list(
      n.iter = 100,
      n.chains = 1,
      return_mcmc = TRUE
      )), "RLum.Results")
  })

  ## check the length of the output
  expect_length(results, 9)

  ## check whether we have the MCMC plots
  expect_s3_class(results$mcmc_IAM, "mcmc.list")
  expect_s3_class(results$mcmc_BCAM, "mcmc.list")

  ## try to plot the results again
  SW({
  plot_OSLAgeSummary(results)
  })

  ## diag = TRUE
  SW({
  expect_s4_class(combine_De_Dr(
    Dr = Dr, int_OD = 0.1, De, s, Age_range = c(0, 100),
    method_control = list(n.iter = 100, n.chains = 2, diag = TRUE)),
    "RLum.Results")
  })

  ## cdf_ADr_quantiles = TRUE and outlier_method = "RousseeuwCroux1993"
  SW({
  expect_s4_class(combine_De_Dr(
    Dr = Dr, int_OD = 0.1, De, s, Age_range = c(0, 100),
    cdf_ADr_quantiles = TRUE,
    outlier_method = "RousseeuwCroux1993",
    method_control = list(n.iter = 100, n.chains = 1)),
    "RLum.Results")
  })

  ## meaningless data for coverage
  set.seed(1)
  Dr.short <- stats::rlnorm(10, 0, 0.3)
  De.short <- 50 * sample(Dr, 2, replace = TRUE)
  s.short <- stats::rnorm(2, 10, 2)

  SW({
  combine_De_Dr(
    Dr = Dr.short,
    int_OD = 0.1,
    De.short,
    s.short,
    outlier_analysis_plot = TRUE,
    Age_range = c(0, 100),
    cdf_ADr_quantiles = FALSE)
  })

  ## reset the graphical parameters to the original values
  par(oldpar)
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")

  set.seed(1)

  SW({
  vdiffr::expect_doppelganger("defaults",
                              combine_De_Dr(De, s, Dr, int_OD = 0.1,
                                            method_control =
                                              list(n.iter = 100,
                                                   n.chains = 1,
                                                   inits = list(
                                                       list(.RNG.name = "base::Wichmann-Hill",
                                                            .RNG.seed = 1)))))
  })
})
