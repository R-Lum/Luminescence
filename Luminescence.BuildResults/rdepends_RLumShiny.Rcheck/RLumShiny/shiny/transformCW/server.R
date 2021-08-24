## Server.R
## MAIN FUNCTION
function(input, output, session) {
  
  
  # input data (with default)
  values <- reactiveValues(data_primary = if ("startData" %in% names(.GlobalEnv)) startData else ExampleData.CW_OSL_Curve,
                           tdata = NULL,
                           args = NULL,
                           pargs = NULL)
  
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
                  colHeaders = c("Time", "Signal"), 
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
  
  # TRANSFORM DATA
  observe({

    P <- input$p
    delta <- input$delta
    
    # validate method parameters
    if (is.na(input$delta)) {
      updateNumericInput(session, "delta", value = 1)
      delta <- 1
    }
    else if (input$delta < 1) {
      updateNumericInput(session, "delta", value = 1)
      delta <- 1
    }
    
    # validate method parameters
    if (is.na(input$p)) {
      updateNumericInput(session, "p", value = 1)
      P <- 1
    }
    else if (input$p < 1) {
      updateNumericInput(session, "p", value = 1)
      P <- 1
    }

      
    args <- list(values$data_primary)
    if (input$method == "CW2pHMi")
      if (delta >= 1)
        args <- append(args, delta)
    if (input$method == "CW2pLMi" || input$method == "CW2pPMi")
      if (P >= 1)
        args <- append(args, P)
    
    values$args <- args
    
    # values$export_args <- args
    values$tdata <- try(do.call(input$method, args))
  })
  
  output$main_plot <- renderPlot({
    
    # be reactive on method changes
    input$method
    input$delta
    input$p
    
    if (inherits(values$tdata, "try-error")) {
      plot(1, type="n", axes=F, xlab="", ylab="")
      text(1, labels = paste(values$tdata, collapse = "\n"))
      return()
    }
    
    values$pargs <- list(values$tdata[,1], values$tdata[ ,2], 
                  log = paste0(ifelse(input$logx, "x", ""), ifelse(input$logy, "y", "")),
                  main = input$main,
                  xlab = input$xlab,
                  ylab = input$ylab1,
                  type = input$type,
                  pch = ifelse(input$pch != "custom", as.integer(input$pch) - 1, input$custompch),
                  col = ifelse(input$color != "custom", input$color, input$jscol1),
                  bty = "n")
    
    par(mar=c(5,4,4,5)+.1, cex = input$cex)
    do.call(plot, values$pargs)
    
    if (input$showCW) {
      par(new = TRUE)
      plot(values$data_primary, 
           axes = FALSE, 
           xlab = NA, 
           ylab = NA, 
           col = "red", 
           type = input$type,
           log = paste0(ifelse(input$logx, "x", ""), ifelse(input$logy, "y", "")))
      axis(side = 4, col = "red", col.axis = "red")
      mtext(input$ylab2, side = 4, line = 3, col = "red")
    }
    
    output$exportScript <- downloadHandler(
      filename = function() { "transformedCW.txt" },
      content = function(file) {
        write.table(values$tdata, file, sep = ",", quote = FALSE, row.names = FALSE)
      },#EO content =,
      contentType = "text"
    )#EndOf::dowmloadHandler()
    
  })
  
  observe({
    # nested renderText({}) for code output on "R plot code" tab
    code.output <- callModule(RLumShiny:::printCode, "printCode", n_input = 1, 
                              fun = paste0(input$method, "(data,"), args = values$args)
    
    output$plotCode<- renderText({
      code.output
    })##EndOf::renderText({})
    
    callModule(RLumShiny:::exportCodeHandler, "export", code = code.output)
    callModule(RLumShiny:::exportPlotHandler, "export", fun = "plot", args = values$pargs)
  })
  
  output$dataset <- renderDataTable({
    if (!is.null(values$tdata))
      values$tdata
  })
  

}##EndOf::function(input, output)