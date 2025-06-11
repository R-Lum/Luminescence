## load data
set.seed(1)
data(ExampleData.BINfileData, envir = environment())
object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1)
object <- get_RLum(object, record.id = c(-29, -30))
sequence.structure  <- c(1, 2, 2, 3, 4, 4)
sequence.structure <-
  as.vector(sapply(seq(0, length(object) - 1, by = 4),
                   function(x) {
                     sequence.structure + x
                   }))

object <-  sapply(1:length(sequence.structure), function(x) {
  object[[sequence.structure[x]]]
})

object <-
  set_RLum(class = "RLum.Analysis",
           records = object,
           protocol = "pIRIR")

SW({
results <- analyse_pIRIRSequence(
  object,
  signal.integral.min = 1,
  signal.integral.max = 2,
  background.integral.min = 900,
  background.integral.max = 1000,
  fit.method = "EXP",
  sequence.structure = c("TL", "pseudoIRSL1", "pseudoIRSL2"),
  main = "Pseudo pIRIR data set based on quartz OSL",
  plot = TRUE,
  plot_singlePanels = TRUE,
  verbose = FALSE
)

## check plot_RLum.Results
expect_silent(plot_RLum.Results(results))

## plot_singlePanels = FALSE && plot == TRUE
suppressWarnings( # warnings thrown by analyse_SAR.CWOSL and fit_DoseResponseCurve
  analyse_pIRIRSequence(
    object,
    signal.integral.min = c(1, 2),
    signal.integral.max = c(2, 3),
    background.integral.min = 900,
    background.integral.max = 1000,
    fit.method = "EXP",
    sequence.structure = c("TL", "pseudoIRSL1", "pseudoIRSL2"),
    main = "Pseudo pIRIR data set based on quartz OSL",
    plot = TRUE,
    plot_singlePanels = FALSE,
    verbose = FALSE
  )
)
})

test_that("check plot stuff", {
  ## it should throw a warning about the plot size
  pdf.out <- tempfile(fileext = ".pdf")
  pdf(pdf.out, width = 6, height = 8)
  expect_warning(analyse_pIRIRSequence(
    object,
    signal.integral.min = 1,
    signal.integral.max = 2,
    background.integral.min = 900,
    background.integral.max = 1000,
    fit.method = "EXP",
    sequence.structure = c("TL", "pseudoIRSL1", "pseudoIRSL2"),
    main = "Pseudo pIRIR data set based on quartz OSL",
    plot = TRUE,
    plot_singlePanels = FALSE,
    verbose = FALSE),
    "[analyse_pIRIRSequence()] Argument 'plot' reset to 'FALSE'",
    fixed = TRUE)
  dev.off()

  ## this should not throw any warning because we print to a large enough pdf
  pdf(pdf.out, width = 16, height = 16)
  expect_silent(analyse_pIRIRSequence(
    object,
    signal.integral.min = 1,
    signal.integral.max = 2,
    background.integral.min = 900,
    background.integral.max = 1000,
    fit.method = "EXP",
    sequence.structure = c("TL", "pseudoIRSL1", "pseudoIRSL2"),
    main = "Pseudo pIRIR data set based on quartz OSL",
    plot = TRUE,
    plot_singlePanels = FALSE,
    verbose = FALSE))
  dev.off()

  ## more coverage: get n.loops > 2
  pdf(pdf.out, width = 25, height = 25)
  suppressWarnings(analyse_pIRIRSequence(
      object,
      signal.integral.min = 1,
      signal.integral.max = 2,
      background.integral.min = 900,
      background.integral.max = 1000,
      fit.method = "EXP",
      sequence.structure = c("TL", paste0("pseudoIRSL", 1:6)),
      main = "Pseudo pIRIR data set based on quartz OSL",
      cex = 0.5,
      plot = TRUE,
      verbose = FALSE))
  dev.off()
  unlink(pdf.out)

  ## this should not throw any warning with plot_singlePanels = TRUE
  expect_silent(analyse_pIRIRSequence(
    object,
    signal.integral.min = 1,
    signal.integral.max = 2,
    background.integral.min = 900,
    background.integral.max = 1000,
    fit.method = "EXP",
    sequence.structure = c("TL", "pseudoIRSL1", "pseudoIRSL2"),
    main = "Pseudo pIRIR data set based on quartz OSL",
    plot = TRUE,
    plot_singlePanels = TRUE,
    verbose = FALSE))

  suppressWarnings( # duplicated plot.single warnings from sanalyse_SAR.CWOSL()
  expect_warning(analyse_pIRIRSequence(
    object,
    signal.integral.min = 1,
    signal.integral.max = 2,
    background.integral.min = 900,
    background.integral.max = 1000,
    fit.method = "EXP",
    plot = TRUE,
    plot.single = TRUE,
    verbose = FALSE),
    "'plot.single' is deprecated, use 'plot_singlePanels' instead")
  )
})

test_that("input validation", {
  expect_error(analyse_pIRIRSequence("test"),
               "'object' should be of class 'RLum.Analysis' or 'list'")
  expect_error(analyse_pIRIRSequence(list(data.frame())),
               "All elements of 'object' should be of class 'RLum.Analysis'")
  expect_error(analyse_pIRIRSequence(list("test"),
                                     signal.integral.min = 1,
                                     signal.integral.max = 2,
                                     background.integral.min = 900,
                                     background.integral.max = 1000),
               "'object' should be of class 'RLum.Analysis'")
  expect_error(analyse_pIRIRSequence(object),
               "'signal.integral.min' should be of class 'integer' or 'numeric'")
  expect_error(analyse_pIRIRSequence(object,
                                     signal.integral.min = 1,
                                     signal.integral.max = 2,
                                     background.integral.min = 900,
                                     background.integral.max = 1000,
                                     sequence.structure = "TL"),
               "'sequence.structure' should contain at least one IR step")
  expect_error(analyse_pIRIRSequence(object,
                                     signal.integral.min = 1,
                                     signal.integral.max = 2,
                                     background.integral.min = 900,
                                     background.integral.max = 1000,
                                     sequence.structure = c("IR50", "error")),
               "'error' not allowed in 'sequence.structure'")

  SW({
  expect_warning(expect_error(analyse_pIRIRSequence(list(object)),
                              "'background.integral.min' should be of class"),
                 "'signal.integral.min' missing, set to 1")
  expect_warning(analyse_pIRIRSequence(list(object),
                                       signal.integral.max = 2,
                                       background.integral.min = 900,
                                       background.integral.max = 1000,
                                       plot = FALSE),
                 "'signal.integral.min' missing, set to 1")
  expect_warning(analyse_pIRIRSequence(list(object),
                                       signal.integral.min = 1,
                                       background.integral.min = 900,
                                       background.integral.max = 1000,
                                       plot = FALSE),
                 "'signal.integral.max' missing, set to 2")
  expect_warning(analyse_pIRIRSequence(list(object),
                                       signal.integral.min = 1,
                                       signal.integral.max = 2,
                                       background.integral.min = 900,
                                       background.integral.max = 1000,
                                       sequence.structure = c("TL", "IRSL",
                                                              "EXCLUDE"),
                                       plot = FALSE),
                 "14 records have been removed due to EXCLUDE")

  expect_message(expect_null(
      analyse_pIRIRSequence(list(object),
                            signal.integral.min = 1,
                            signal.integral.max = 2,
                            background.integral.min = 900,
                            background.integral.max = 1000,
                            sequence.structure = c("TL", "pseudoIRSL1"),
                            plot = FALSE)),
      "The number of records is not a multiple of the defined sequence structure")

  object.warn <- object
  for (i in 1:6) object.warn@records[[i]]@recordType <- "error"
  expect_warning(analyse_pIRIRSequence(object.warn,
                                       main = "Title",
                                       signal.integral.min = 1,
                                       signal.integral.max = 2,
                                       background.integral.min = 900,
                                       background.integral.max = 1000,
                                       plot = FALSE),
                 "The following unrecognised record types have been removed:")

  object.noTL <- subset(object, recordType != "TL")
  expect_warning(analyse_pIRIRSequence(list(object.noTL),
                                       main = "Title",
                                       signal.integral.min = 1,
                                       signal.integral.max = 2,
                                       background.integral.min = 900,
                                       background.integral.max = 1000,
                                       plot = FALSE),
                 "Your sequence does not contain 'TL' curves")
  })
})

test_that("check class and length of output", {
    testthat::skip_on_cran()

    expect_s4_class(results, "RLum.Results")
    expect_equal(length(results), 4)
    expect_s3_class(results$LnLxTnTx.table, "data.frame")
    expect_s3_class(results$rejection.criteria, "data.frame")

   expect_equal(round(sum(results$data[1:2, 1:4]), 0),7584)
   expect_equal(round(sum(results$rejection.criteria$Value), 2),3338.69)
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 838
  object.mod <- object
  object.mod@records <- lapply(object.mod@records, function(x) {
    x@recordType <- paste(x@recordType, "(UVVIS)")
    x
  })
  expect_s4_class(analyse_pIRIRSequence(object.mod,
                                        signal.integral.min = 1,
                                        signal.integral.max = 10,
                                        background.integral.min = 900,
                                        background.integral.max = 1000,
                                        plot = FALSE,
                                        verbose = FALSE),
               "RLum.Results")
})

test_that("graphical snapshot tests", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not(getRversion() >= "4.4.0")

  set.seed(1)
  SW({
    vdiffr::expect_doppelganger("default",
                                analyse_pIRIRSequence(
                                  object,
                                  signal.integral.min = 1,
                                  signal.integral.max = 2,
                                  background.integral.min = 900,
                                  background.integral.max = 1000,
                                  fit.method = "EXP",
                                  sequence.structure = c("TL", "pseudoIRSL1", "pseudoIRSL2"),
                                  main = "Pseudo pIRIR data set based on quartz OSL",
                                  plot = TRUE,
                                  plot_singlePanels = FALSE,
                                  verbose = FALSE))

  })
})
