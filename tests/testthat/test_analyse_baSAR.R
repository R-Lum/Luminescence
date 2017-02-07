context("analyse_baSAR")

##Full check
test_that("Full check of analyse_baSAR function", {
  skip_on_cran()

    set.seed(1)
    ##(1) load package test data set
    data(ExampleData.BINfileData, envir = environment())

    ##(2) selecting relevant curves, and limit dataset
    CWOSL.SAR.Data <- subset(CWOSL.SAR.Data,
                             subset = POSITION %in% c(1:3) & LTYPE == "OSL")


    ##(3) run analysis
    ##please not that the here selected parameters are
    ##choosen for performance, not for reliability
    results <- analyse_baSAR(
      object = CWOSL.SAR.Data,
      source_doserate = c(0.04, 0.001),
      signal.integral = c(1:2),
      background.integral = c(80:100),
      fit.method = "EXP",
      method_control = list(inits = list(
        list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1),
        list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 2),
        list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 3)
      )),
      plot = FALSE,
      verbose = FALSE,
      n.MCMC = 1000,
      txtProgressBar = FALSE

    )


    expect_is(
      results,
      class = "RLum.Results",
      info = NULL,
      label = NULL
    )
    expect_is(results$summary, "data.frame")
    expect_is(results$mcmc, "mcmc.list")
    expect_is(results$models, "list")

    expect_equal(round(sum(results$summary[, c(6:9)]), 2), 504.69)

})

