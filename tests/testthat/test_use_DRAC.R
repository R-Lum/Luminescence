test_that("input validation", {
  testthat::skip_on_cran()

  ## wrong file name
  expect_error(use_DRAC("error"),
               "[use_DRAC()] Input file does not exist",
               fixed = TRUE)
  expect_error(use_DRAC(NA),
               "'file' should be of class 'character', 'DRAC.list' or")
  expect_error(use_DRAC(character(0)),
               "'file' cannot be an empty character")
  expect_error(use_DRAC(letters),
               "'file' should have length 1")

  ## CSV file with the wrong header
  fake <- data.table::fread(test_path("_data/DRAC_Input_Template.csv"))
  fake[1, 1] <- "error"
  fake.csv <- tempfile(fileext = ".csv")
  data.table::fwrite(fake, file = fake.csv)
  expect_error(use_DRAC(fake.csv),
               "you are not using the original DRAC v1.2 CSV template")

  ## exceed allowed limit
  SW({
  expect_warning(input <- template_DRAC(preset = "DRAC-example_quartz",
                                        nrow = 5001),
                 "More than 5000 datasets might not be supported")
  })
  expect_error(use_DRAC(input), "The limit of allowed datasets is 5000")

  ## citation style
  expect_error(use_DRAC(template_DRAC(preset = "DRAC-example_quartz",
                                      notification = FALSE),
                        citation_style = "error"),
               "[use_DRAC()] 'citation_style' should be one of",
               fixed = TRUE)
})

##Full check
test_that("Test DRAC", {
  testthat::skip_on_cran()

 ##use manual example
 ##create template
 SW({
 input <- template_DRAC(preset = "DRAC-example_quartz")
 })

 ##test
 expect_s3_class(input, "DRAC.list")

 ##fill (which also tests the methods)
 input$`Project ID` <- "DRAC-Example"
 input$`Sample ID` <- "Quartz"
 input$`Conversion factors` <- "AdamiecAitken1998"
 input$`External U (ppm)` <- 3.4
 input$`errExternal U (ppm)` <- 0.51
 input$`External Th (ppm)` <- 14.47
 input$`errExternal Th (ppm)` <- 1.69
 input$`External K (%)` <- 1.2
 input$`errExternal K (%)` <- 0.14
 input$`Calculate external Rb from K conc?` <- "Y"
 input$`Calculate internal Rb from K conc?` <- "Y"
 input$`Scale gammadoserate at shallow depths?` <- "Y"
 input$`Grain size min (microns)` <- 90L
 input$`Grain size max (microns)` <- 125L
 input$`Water content ((wet weight - dry weight)/dry weight) %` <- 5
 input$`errWater content %` <- 2
 input$`Depth (m)` <- 2.2
 input$`errDepth (m)` <- 0.22
 input$`Overburden density (g cm-3)` <- 1.8
 input$`errOverburden density (g cm-3)` <- 0.1
 input$`Latitude (decimal degrees)` <- 30.0000
 input$`Longitude (decimal degrees)` <- 70.0000
 input$`Altitude (m)` <- 150
 input$`De (Gy)` <- 20
 input$`errDe (Gy)` <- 0.2

 ##run DRAC
 SW({
 output <- expect_s4_class(use_DRAC(input), "RLum.Results")
 })

 ## print method for DRAC.highlights
 expect_output(print(output$DRAC$highlights), regexp = "TO:GP = errAge")

 ## DRAC.data.frame
 input.df <- as.data.frame(input)
 class(input.df) <- c("data.frame", "DRAC.data.frame")
 expect_s4_class(use_DRAC(input.df, verbose = FALSE),
                 "RLum.Results")

 ## CSV input
 expect_s4_class(use_DRAC(test_path("_data/DRAC_Input_Template.csv"),
                          verbose = FALSE),
                 "RLum.Results")

 ## XLS input
 file.create(fake.xls <- tempfile(fileext = ".xls"))
 expect_error(use_DRAC(fake.xls),
              "XLS/XLSX format no longer supported, use CSV instead")

 ## communicate with non sufficient dataset
 SW({
 t <- template_DRAC(notification = FALSE)
 expect_error(use_DRAC(t),
              "We got a response from the server")
 print("N")
 })

 SW({
 ## communicate with fake url
 expect_error(use_DRAC(t, url = "iamnotvali8793270942hd.valid"),
              "Transmission failed with HTTP status code: URL invalid")
 })
})
