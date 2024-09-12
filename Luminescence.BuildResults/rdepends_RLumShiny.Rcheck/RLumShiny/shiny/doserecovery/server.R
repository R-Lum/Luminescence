##############################################################################
###                        MAIN PROGRAM                                    ###
##############################################################################
function(input, output, session) {
  
  # input data (with default)
  values <- reactiveValues(data_primary =  if ("startData" %in% names(.GlobalEnv)) startData else ExampleData.DeValues$BT998[7:11,],
                           data_secondary =  setNames(as.data.frame(matrix(NA_real_, nrow = 5, ncol = 2)), c("x", "y")),
                           data = NULL,
                           args = NULL)
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # check and read in file (DATA SET 1)
  observeEvent(input$file1, {
    inFile<- input$file1
    
    if(is.null(inFile)) 
      return(NULL) # if no file was uploaded return NULL
    
    values$data_primary <- fread(file = inFile$datapath, data.table = FALSE) # inFile[1] contains filepath 
  })
  
  # check and read in file (DATA SET 2)
  observeEvent(input$file2, {
    inFile<- input$file2
    
    if(is.null(inFile)) 
      return(NULL) # if no file was uploaded return NULL
    
    values$data_secondary <- fread(file = inFile$datapath, data.table = FALSE) # inFile[1] contains filepath 
  })
  
  ### GET DATA SETS
  observe({
    
    data <- list(values$data_primary, values$data_secondary)
    data <- lapply(data, function(x) { 
      x_tmp <- x[complete.cases(x), ]
      if (nrow(x_tmp) == 0) return(NULL)
      else return(x_tmp)
    })
    data <- data[!sapply(data, is.null)]
    data <- lapply(data, function(x) setNames(x, c("Dose", "Error")))
    
    values$data <- data
  })
  
  output$table_in_primary <- renderRHandsontable({
    rhandsontable(values$data_primary, 
                  height = 300, 
                  colHeaders = c("Dose", "Error"), 
                  rowHeaders = NULL)
  })
  
  observeEvent(input$table_in_primary, {
    
    # Workaround for rhandsontable issue #138 
    # https://github.com/jrowen/rhandsontable/issues/138
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
  
  
  output$table_in_secondary <- renderRHandsontable({
    
    rhandsontable(values$data_secondary, 
                  height = 300,
                  colHeaders = c("Dose", "Error"), 
                  rowHeaders = NULL)
  })
  
  observeEvent(input$table_in_secondary, {
    
    # Workaround for rhandsontable issue #138 
    # https://github.com/jrowen/rhandsontable/issues/138
    # See detailed explanation above
    df_tmp <- input$table_in_secondary
    row_names <-  as.list(as.character(seq_len(length(df_tmp$data))))
    df_tmp$params$rRowHeaders <- row_names
    df_tmp$params$rowHeaders <- row_names
    df_tmp$params$rDataDim <- as.list(c(length(row_names),
                                        length(df_tmp$params$columns)))
    if (df_tmp$changes$event == "afterRemoveRow")
      df_tmp$changes$event <- "afterChange"
    
    if (!is.null(hot_to_r(df_tmp)))
      values$data_secondary <- hot_to_r(df_tmp)
    
  })
  
  
  output$xlim<- renderUI({
    
    data <- values$data
    
    n <- max(sapply(data, nrow))
    
    sliderInput(inputId = "xlim", label = "Range x-axi s", 
                min = 0, max = n*2, 
                value = c(1, n+1))
  })
  
  observe({
    updateTextInput(session, inputId = "xlab", 
                    value = if(input$preheat==TRUE){"Preheat Temperature [\u00B0C]"}else{"# Aliquot"})
  })
  
  observe({
    
    input$refresh
    
    outputOptions(x = output, name = "xlim", suspendWhenHidden = FALSE)
    
    # if custom datapoint style get char from separate input panel
    pch <- ifelse(input$pch == "custom", input$custompch, as.integer(input$pch) - 1)
    pch2 <- ifelse(input$pch2 == "custom", input$custompch2, as.integer(input$pch2) - 1)
    
    # if custom datapoint color get RGB code from separate input panel
    color <- ifelse(input$color == "custom", input$rgb, color<- input$color)
    
    # if custom datapoint color get RGB code from separate input panel
    if(length(values$data) > 1) {
      color2 <- ifelse(input$color2 == "custom", input$rgb2, input$color2)
    } else {
      color2 <- ifelse(input$preheat, color, "white")
    }
    
    if (length(values$data) == 1){
      given.dose<- input$dose
      legend<- input$legendname
    } else {
      given.dose<- c(input$dose, input$dose2)
      legend<- c(input$legendname, input$legendname2)
    }
    
    # save all arguments in a list
    values$args<- list(
      values = values$data, 
      error.range = input$error,
      given.dose = given.dose,
      summary = input$stats,
      summary.pos = input$sumpos,
      boxplot = input$boxplot,
      legend = legend,
      legend.pos = input$legend.pos,
      main = input$main,
      mtext = input$mtext,
      col = c(color, color2),
      pch = c(pch, pch2),
      xlab = input$xlab,
      ylab = input$ylab,
      xlim = input$xlim,
      ylim = input$ylim,
      cex = input$cex)
    
    if (input$preheat) {

      n<- length(values$data[[1]][,1])
      ph<- c(input$ph1, input$ph2, input$ph3, input$ph4, input$ph5, input$ph6, input$ph7, input$ph8)
      ph<- ph[1:n]

    isolate({
      values$args<- c(values$args, "preheat" = NA)
      values$args$preheat<- ph

      values$args$pch<- rep(values$args$pch, n)
      values$args$col<- rep(values$args$col, n)
    })

    }
      
  })
  
  #### PLOT ####
  output$main_plot <- renderPlot({
    
    validate(
      need(expr = input$xlim, message = 'Waiting for data... Please wait!')
    )
    
    # plot DRT Results
    do.call(what = plot_DRTResults, args = values$args)
    
  })
  
  observe({
    # nested renderText({}) for code output on "R plot code" tab
    code.output <- callModule(RLumShiny:::printCode, "printCode", n_input = 2, 
                              fun = "plot_DRTResults(data,", args = values$args)
    
    output$plotCode<- renderText({
      code.output
    })##EndOf::renderText({})
    
    callModule(RLumShiny:::exportCodeHandler, "export", code = code.output)
    callModule(RLumShiny:::exportPlotHandler, "export", fun = "plot_DRTResults", args = values$args)
  })
  
  # renderTable() that prints the data to the second tab
  output$dataset<- renderDataTable(
    options = list(pageLength = 10, autoWidth = FALSE),
    callback = "function(table) {
  table.on('click.dt', 'tr', function() {
  $(this).toggleClass('selected');
  Shiny.onInputChange('rows',
  table.rows('.selected').values$data.toArray());
  });}",
    {
      data<- values$data
      colnames(data[[1]])<- c("De", "De error")
      data[[1]]
    })##EndOf::renterTable()
  
  
  # renderTable() that prints the data to the second tab
  output$dataset2<- renderDataTable(
    options = list(pageLength = 10, autoWidth = FALSE),
    callback = "function(table) {
  table.on('click.dt', 'tr', function() {
  $(this).toggleClass('selected');
  Shiny.onInputChange('rows',
  table.rows('.selected').values$data.toArray());
  });}",
    {
      data<- values$data
      if(length(data)>1) {
        colnames(data[[2]])<- c("De", "De error")
        data[[2]]
      }
    })##EndOf::renterTable()
  
}
