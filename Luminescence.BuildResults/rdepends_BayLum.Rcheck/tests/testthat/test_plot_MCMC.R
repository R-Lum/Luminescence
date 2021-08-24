test_that("Full function test", {
  testthat::skip_on_cran()
  local_edition(3)

  ##test function stop
  expect_error(plot_MCMC("character"), label = "[plot_MCMC()] 'sample' has to be of type 'mcmc.list'!", )

  data(MCMCsample,envir = environment())
  object <- coda::as.mcmc(MCMCsample)

  ## load data
  data(DATA1,envir = environment())
  data(DATA2,envir = environment())
  Data <- combine_DataFiles(DATA2,DATA1)

  ## Age computation of samples GDB5 and GDB3,
  priorage <- c(1,10,20,60) # these samples are not young
  ## without common error and without stratigraphic constraints
  object2 <- suppressWarnings(AgeS_Computation(
    DATA = Data,
    Nb_sample = 2,
    SampleNames = c("GDB5","GDB3"),
    PriorAge = priorage,
    Iter = 50,
    n.chains = 2))

  ##test function itself
  expect_silent(plot_MCMC(object))
  expect_warning(plot_MCMC(object2), regexp = "'n.iter' out of range, reset to number of observations")
  expect_warning(plot_MCMC(object2$Sampling), regexp = "'n.iter' out of range, reset to number of observations")



  ##test arguments

    ##plot.single
    expect_silent(plot_MCMC(object, plot_single = TRUE))

    ##sample_names
    expect_silent(plot_MCMC(object, sample_names = "Test"))
    expect_silent(plot_MCMC(object, mtext = "Test"))
    expect_silent(plot_MCMC(object, sample_names = c("Test", "Test2")))
    expect_warning(Luminescence:::.warningCatcher(plot_MCMC(
      object2, sample_names = c("Test", "Test2", "Test3")
    )))

    #variables
    expect_silent(plot_MCMC(object, variables = c("A")))
    expect_warning(plot_MCMC(object, variables = c("U")))

    ##n.chains
    expect_silent(plot_MCMC(object, n.chains = 1))

    ##n.iter
    expect_silent(plot_MCMC(object, n.iter = 10))
    expect_warning(Luminescence:::.warningCatcher(plot_MCMC(object, n.iter = 10:20)))
    expect_warning(plot_MCMC(object, n.iter = c(20000)))
    expect_warning(plot_MCMC(object, n.iter = -1))
    expect_warning(Luminescence:::.warningCatcher(plot_MCMC(object, n.iter = c(1:20000))))

    #smooth
    expect_silent(plot_MCMC(object, smooth = TRUE))


})

