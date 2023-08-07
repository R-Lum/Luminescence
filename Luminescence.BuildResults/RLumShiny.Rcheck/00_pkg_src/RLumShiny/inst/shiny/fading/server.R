## Server.R
## MAIN FUNCTION
function(input, output, session) {
  
  # input data (with default)
  values <- reactiveValues(data_primary = if ("startData" %in% names(.GlobalEnv)) startData else ExampleData.Fading$fading.data$IR50,
                           data = NULL,
                           args = NULL,
                           args_corr = NULL,
                           results = NULL,
                           results_corr = NULL)
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # check and read in file (DATA SET 1)
  observeEvent(input$file, {
    inFile<- input$file
    
    if(is.null(inFile)) 
      return(NULL) # if no file was uploaded return NULL
    
    values$data_primary <- fread(file = inFile$datapath, data.table = FALSE) # inFile[1] contains filepath 
  })
  
  output$table_in_primary <- renderRHandsontable({
    rhandsontable(values$data_primary, 
                  height = 300, 
                  colHeaders = c("LxTx", "LxTx error", "Time since irradiation"), 
                  rowHeaders = NULL)
  })
  
  observeEvent(input$table_in_primary, {
    
    # Workaround for rhandsontable issue #138 
    # https://github.com/jrowen/rhandsontable/issues/138
    # See detailed explanation in abanico application
    df_tmp <- input$table_in_primary
    row_names <-  as.list(as.character(seq_len(length(df_tmp$data))))
    df_tmp$params$rRowHeaders <- row_names
    df_tmp$params$rowHeaders <- row_names
    df_tmp$params$rDataDim <- as.list(c(length(row_names),
                                        length(df_tmp$params$columns)))
    if (df_tmp$changes$event == "afterRemoveRow")
      df_tmp$changes$event <- "afterChange"
    
    if (!is.null(hot_to_r(df_tmp)))
      values$data_primary <- hot_to_r(df_tmp)
  })
  
  # Arguments
  observe({
    
    values$data <- values$data_primary
    
    values$args <- list(
      object = values$data,
      structure = c("Lx", "Tx"),
      t_star = "half",
      n.MC = 100,
      verbose = FALSE,
      plot = TRUE,
      plot.single = 3
    )
  })
  
  # MAIN (analyse_FadingMeasurement) ---- 
  output$main_plot <- renderPlot({
    values$results <- try(do.call(analyse_FadingMeasurement, values$args))
  })
  
  # MAIN (calc_FadingCorr) ----
  observe({
    if (!input$override_gval)
      if (is.null(values$results))
        return(NULL)
    
    if (inherits(values$results, "try-error"))
      return(NULL)
    
    values$results@originator <- "analyse_FadingMeasurement"
    
    values$args_corr <- list(
      age.faded = c(input$age_faded, input$age_error_faded),
      g_value = if (input$override_gval) c(input$g_value, input$g_value_error) else values$results,
      tc = input$tc,
      tc.g_value = input$tc_gval,
      verbose = FALSE,
      txtProgressBar = FALSE,
      n.MC = 1000
    )
    
    values$results_corr <- try(do.call(calc_FadingCorr, values$args_corr))
  })
  
  
  
  observe({
    # nested renderText({}) for code output on "R plot code" tab
    code.output <- callModule(RLumShiny:::printCode, "printCode", n_input = 1, 
                              fun = "analyse_FadingMeasurement(data,", args = values$args)
    
    output$plotCode<- renderText({
      code.output
    })##EndOf::renderText({})
    
    callModule(RLumShiny:::exportCodeHandler, "export", code = code.output)
    callModule(RLumShiny:::exportPlotHandler, "export", fun = "analyse_FadingMeasurement", args = values$args)
  })
  
  output$corrCode <- renderText({
    
    if (input$override_gval) {
      gval <- values$args_corr$g_value
      tc <- input$tc
    } else {
      gval <- c(values$results@data$fading_results$FIT, values$results@data$fading_results$SD)
      tc <- values$results@data$fading_results$TC
    }
    
    
    paste(
      "# To reproduce the plot in your local R environment",
      "# copy and run the following code to your R console.",
      "library(Luminescence)", "\n",
      "calc_FadingCorr(",
      paste0("age.faded = c(", values$args_corr$age.faded[1], ", ",  values$args_corr$age.faded[2], "),"),
      paste0("g_value = c(", gval[1], ", ", gval[2], "),"),
      paste0("tc = ", tc, ", "),
      paste0("tc.g_value = ", input$tc_gval, ","),
      paste0("n.MC = 1000)"),
      
      sep = "\n")
  })
  
  output$results <- renderText({
    if (is.null(values$results))
      return(NULL)
    if (inherits(values$results, "try-error"))
      return(NULL)
    
    gval <- get_RLum(values$results)
    rho <- get_RLum(values$results, "rho_prime")
    
    HTML(paste0(
        tags$hr(), 
        tags$b("g-value: "), signif(gval$FIT, 3), " &plusmn; ", signif(gval$SD, 3), " %/decade", tags$br(),
        tags$b("g-value"), tags$sub("2days"), ": ", signif(gval$G_VALUE_2DAYS, 3), " &plusmn; ", signif(gval$G_VALUE_2DAYS.ERROR, 3), " %/decade", tags$br(),
        tags$b("t"), tags$sub("c"), ": ", gval$TC, tags$br(),
        " &rho;': ", signif(rho$MEAN, 3), " &plusmn; ", signif(rho$SD, 3), tags$br(),
        " &raquo; log10(&rho;'): ", signif(log10(rho$MEAN), 3), " &plusmn; ", signif(rho$SD / (rho$MEAN * log(10, base = exp(1))), 3)
      ))
    
  })
  
  output$results_corr <- renderText({
    
    if (is.null(values$results_corr) || inherits(values$results_corr, "try-error"))
      res <- data.frame(AGE = NA, AGE.ERROR = NA)
    else
      res <- get_RLum(values$results_corr)
    
    HTML(paste0(
      tags$hr(),
      tags$b("Age "), tags$em("(faded): "), input$age_faded, " &plusmn; ", input$age_error_faded, " ka", tags$br(),
      tags$b("Age "), tags$em("(corrected): "), signif(res$AGE, 3), " &plusmn; ", signif(res$AGE.ERROR, 3), " ka"
    ))
  })
  
}##EndOf::function(input, output)