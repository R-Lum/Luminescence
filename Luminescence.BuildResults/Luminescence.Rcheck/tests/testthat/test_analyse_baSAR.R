##Full check
test_that("Full check of analyse_baSAR function", {
  skip_on_cran()
  local_edition(3)

    set.seed(1)
    ##(1) load package test data set
    data(ExampleData.BINfileData, envir = environment())

    ##(2) selecting relevant curves, and limit dataset
    CWOSL.SAR.Data <- subset(CWOSL.SAR.Data,
                             subset = POSITION %in% c(1:3) & LTYPE == "OSL")

    ##(3) run analysis
    ##please not that the here selected parameters are
    ##chosen for performance, not for reliability
    results <- suppressWarnings(analyse_baSAR(
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
      plot = TRUE,
      verbose = TRUE,
      n.MCMC = 100,
      txtProgressBar = TRUE

    ))

    expect_s4_class(results, class = "RLum.Results")
    expect_s3_class(results$summary, "data.frame")
    expect_s3_class(results$mcmc, "mcmc.list")
    expect_type(results$models, "list")
    expect_type(round(sum(results$summary[, c(6:9)]), 2),type = "double")

})

