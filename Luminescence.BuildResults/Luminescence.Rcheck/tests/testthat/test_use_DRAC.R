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
 input$`Grain size min (microns)` <- 90
 input$`Grain size max (microns)` <- 125
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
 fake.xls <- system.file("extdata/clippy.xls", package = "readxl")
 expect_error(use_DRAC(fake.xls),
              "you are not using the original DRAC v1.1 XLSX template")

 ## crash function
 ## wrong file name
 expect_error(use_DRAC("error"), "\\[use_DRAC\\(\\)\\] It seems that the file doesn't exist!")
 expect_error(use_DRAC(NA),
              "The provided data object is not a valid DRAC template")

 ## exceed allowed limit
 SW({
 expect_warning(input <- template_DRAC(preset = "DRAC-example_quartz",
                                       nrow = 5001),
                "More than 5000 datasets might not be supported")
 })
 expect_error(use_DRAC(input), "The limit of allowed datasets is 5000!")
})
