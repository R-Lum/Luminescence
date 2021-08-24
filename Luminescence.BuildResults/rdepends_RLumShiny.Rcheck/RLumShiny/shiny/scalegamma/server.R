## Server.R
## MAIN FUNCTION
function(input, output, session) {
  
  # input data (with default)
  values <- reactiveValues(data = example_data, data_used = NULL, args = NULL, results = NULL, error = NULL)
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  observe({
    # make sure that input panels are registered on non-active tabs.
    # by default tabs are suspended and input variables are hence
    # not available
    outputOptions(x = output, name = "df_inf", suspendWhenHidden = FALSE)
    outputOptions(x = output, name = "df_scaled", suspendWhenHidden = FALSE)
    outputOptions(x = output, name = "main_plot", suspendWhenHidden = FALSE)
  })
  
  ## FILE INPUT ----
  observeEvent(input$file, {
    inFile<- input$file
    
    if(is.null(inFile)) 
      return(NULL) # if no file was uploaded return NULL
    
    data <- fread(file = inFile$datapath, data.table = FALSE) # inFile[1] contains filepath
    
    ## add or crop columns
    if (ncol(data) > 12)
      data <- data[ ,1:12]
    else if (ncol(data) < 12) {
      ncol <- 12 - ncol(data)
      data <- cbind(data, matrix(NA, ncol = ncol))
    }
    
    values$data <- data
  })
  
  
  ## R_HANDSONTABLE ----
  output$table_in_primary <- renderRHandsontable({
    
    rh <- rhandsontable(values$data,
                        height = 300,
                        colHeaders = c("Layer ID", "Thickness (cm)", "Sample offset (cm)", "K (%)", "error", "Th (ppm)", "error", "U (ppm)", "error", "Water content (%)", "error", "Density (g/cm3)"), 
                        rowHeaders = NULL)
    
    rh <- hot_cols(rh, renderer = "
function(instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.NumericRenderer.apply(this, arguments);
  if (col != 2 && !value) {
    td.style.background = 'crimson';
    td.style.textDecoration = 'line-through';
  } else if (col == 2 && !value) {
    td.style.background = 'darkgrey';
 } 
}")
    invalid_rows <- which(!complete.cases(values$data[ ,-3]))
    if (length(invalid_rows) > 0) {
      for (i in 1:length(invalid_rows))
        rh <- hot_cell(rh, row = invalid_rows[i], col = 1, 
                       comment = paste("Layer removed due to incomplete layer information.",
                                       "Please check all values and fill in missing information",
                                       "if applicable."))
    }
    
    rh
  })
  
  observeEvent(input$table_in_primary, {
    if (!is.null(hot_to_r(input$table_in_primary)))
      values$data <- hot_to_r(input$table_in_primary)
  })
  
  ## INPUT DATA CHECK ----
  observe({
    ## remove incomplete rows
    # note that we have to remove the third column (sample_offset), which
    # explicitly requires NA values for all non-target layers
    tmp <- values$data[complete.cases(values$data[ ,-3]), ]
    values$data_used <- tmp
  })
  
  
  ## ARGUMENTS ----
  observe({
    
    # compile args
    args <- list(
      data = values$data_used,
      conversion_factors = input$conv_fac,
      fractional_gamma_dose = input$frac_dose,
      plot = TRUE,
      plot_single = TRUE,
      verbose = FALSE
    )
    
    # sanitise final list by removing all NULL elements
    args[sapply(args, is.null)] <- NULL
    
    # return
    values$args <- args
  })
  
  
  ## SHINY MODULES ----
  observe({
    # nested renderText({}) for code output on "R plot code" tab
    code.output <- callModule(RLumShiny:::printCode, "printCode", n_input = 1, 
                              fun = paste0("scale_GammaDose(data,"), args = values$args)
    
    output$plotCode<- renderText({
      code.output
    })##EndOf::renderText({})
    
    callModule(RLumShiny:::exportCodeHandler, "export", code = code.output)
    callModule(RLumShiny:::exportPlotHandler, "export", fun = "scale_GammaDose", args = values$args)
  })
  
  ## MAIN ----
  
  ## Calculate results
  # observe({
  #   tryCatch({
  #     values$results <- do.call(scale_GammaDose, values$args)
  #   }, error = function(e) { 
  #     values$error <- e
  #     values$results <- NULL
  #   })
  # })
  
  ## PLOT ----
  output$main_plot <- renderPlot({
    tryCatch({
      values$results <- do.call(scale_GammaDose, values$args)
    }, error = function(e) { 
      values$error <- e
      values$results <- NULL
    })
  })
  
  ## ERROR HANDLING ----
  output$error <- renderText({
    # invalidate all reactive values
    if (!is.null(values$error)) {
      values$results <- NULL
      HTML(paste0(
        tags$br(),
        tags$p("ERROR!", style = "color:red; font-size:20px;"),
        values$error$message
      ))
    }
  })
  
  ## NUMERIC RESULTS ----
  output$console <- renderText({
    if (is.null(values$results))
      return(NULL)
    
    values$error <- NULL
    res <- as.data.frame(get_RLum(values$results))
    inf_table <- get_RLum(values$results, "dose_rates")$infinite_matrix
    
    HTML(paste0(
      tags$br(),
      tags$p("RESULTS", style = "color:#008000; font-size:20px;"),
      tags$p(
        tags$b("Target layer: "), res$id, tags$br(), 
        tags$b("Scaled gamma dose rate (Gy/ka): "), f(res$dose_rate_total), "\u00b1", f(res$dose_rate_total_err), tags$br(),
        style = "font-size:15px"
      )
    ))
  })
  
  ## TABLE 1: Infinite matrix dose rate ----
  output$df_inf <- renderDataTable({
    if (is.null(values$results))
      return(NULL)
    
    df <- get_RLum(values$results, "dose_rates")$`infinite_matrix`
    for (i in 2:ncol(df))
      df[,i] <- f(df[,i])
    df
  }, options = list(ordering = FALSE, searching = FALSE, paging = FALSE))
  
  ## TABLE 2: Scaled gamma dose rate ----
  output$df_scaled <- renderDataTable({
    if (is.null(values$results))
      return(NULL)
    
    df <- get_RLum(values$results, "dose_rates")$scaled_dose_rate
    for (i in 2:ncol(df))
      df[,i] <- f(df[,i])
    df
  }, options = list(ordering = FALSE, searching = FALSE, paging = FALSE))
  
}##EndOf::function(input, output)