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
results <- analyse_pIRIRSequence(
  object,
  signal.integral.min = 1,
  signal.integral.max = 2,
  background.integral.min = 900,
  background.integral.max = 1000,
  fit.method = "EXP",
  sequence.structure = c("TL", "pseudoIRSL1", "pseudoIRSL2"),
  main = "Pseudo pIRIR data set based on quartz OSL",
  plot = FALSE,
  plot.single = TRUE,
  verbose = FALSE
)

test_that("check class and length of output", {
    testthat::skip_on_cran()
    local_edition(3)

    expect_s4_class(results, "RLum.Results")
    expect_equal(length(results), 4)
    expect_s3_class(results$LnLxTnTx.table, "data.frame")
    expect_s3_class(results$rejection.criteria, "data.frame")


})

test_that("check output", {
   testthat::skip_on_cran()
   local_edition(3)

   ##fix for different R versions
   if(R.version$major == "3" && as.numeric(R.version$minor) < 6){
     expect_equal(round(sum(results$data[1:2, 1:4]), 0),7583)

   }else{
     expect_equal(round(sum(results$data[1:2, 1:4]), 0),7584)

   }

   expect_equal(round(sum(results$rejection.criteria$Value), 2),3338.69)

})
