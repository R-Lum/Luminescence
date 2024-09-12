test_that("Full function test", {
  testthat::skip_on_cran()
  local_edition(3)

  ##create needed dataset
  ## load data
  data(DATA_C14,envir = environment())
  C14Cal <- DATA_C14$C14[,1]
  SigmaC14Cal <- DATA_C14$C14[,2]
  Names <- DATA_C14$Names
  nb_sample <- length(Names)

  ## Age computation
  Age <- suppressWarnings(AgeC14_Computation(
    Data_C14Cal = C14Cal,
    Data_SigmaC14Cal = SigmaC14Cal,
    SampleNames = Names,
    Nb_sample = nb_sample,
    PriorAge = rep(c(20,60),nb_sample),
    Iter = 500,
    quiet = TRUE))

  ##test function input
  expect_error(plot_Ages(object = "list"),
               regexp = "Wrong input, only objects of type 'BayLum.list' are allowed. Please check the manual!")

  ##test regular output
  expect_s3_class(plot_Ages(Age), class = "data.frame")

  ##test some features
  expect_silent(plot_Ages(object = Age,
            sample_names = c("test1", "test2")))

  ##test AT == NULL
  expect_silent(plot_Ages(object = Age, sample_order = c(1,2)))

  ##check legend option
  expect_silent(plot_Ages(object = Age, legend.pos = "top", legend.cex = 0.2))

  ## check density plot mode
  expect_silent(plot_Ages(object = Age, plot_mode = "density"))

  ## check density plot mode arguments
  expect_silent(plot_Ages(object = Age, plot_mode = "density", d_scale = 0.2, show_ages = TRUE))

})

