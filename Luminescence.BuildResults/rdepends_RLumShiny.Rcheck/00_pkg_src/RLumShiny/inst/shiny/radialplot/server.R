## Server.R
## MAIN FUNCTION
function(input, output, session) {
  
  
  # input data (with default)
  values <- reactiveValues(data_primary = if ("startData" %in% names(.GlobalEnv)) startData else ExampleData.DeValues$CA1,
                           data_secondary = setNames(as.data.frame(matrix(NA_real_, nrow = 5, ncol = 2)), c("x", "y")),
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
    
    ### GET DATA
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
  
  # dynamically inject sliderInput for central value
  output$centValue<- renderUI({
    
    centValue.data <- do.call(rbind, values$data)
    
    sliderInput(inputId = "centValue", 
                label = "Central Value",
                min = min(centValue.data[,1])*0.9, 
                max = max(centValue.data[,1])*1.1,
                value = mean(centValue.data[,1]))
  })## EndOf::renderUI()
  
  
  # dynamically inject sliderInput for z-axis range
  output$xlim<- renderUI({
    
    xlim.data<- do.call(rbind, values$data)
    
    if(input$logz == TRUE) {
      sd<- xlim.data[,2] / xlim.data[,1]
    } else {
      sd<- xlim.data[,2] 
    }
    
    prec<- 1/sd
    
    sliderInput(inputId = "xlim", 
                label = "Range x-axis",
                min = 0, 
                max = max(prec)*2,
                value = c(0, max(prec)*1.05), round=FALSE, step=0.0001)
    
  })## EndOf::renderUI()
  
  
  # dynamically inject sliderInput for z-axis range
  output$zlim<- renderUI({
    
    zlim.data<- do.call(rbind, values$data)
    sliderInput(inputId = "zlim",
                label = "Range z-axis", 
                min = min(zlim.data[,1])*0.25,
                max = max(zlim.data[,1])*1.75,
                value = c(min(zlim.data[,1])*0.8, max(zlim.data[,1])*1.2))
    
  })## EndOf::renderUI()
  
  observe({
    
    # refresh plot on button press
    input$refresh
    
    # make sure that input panels are registered on non-active tabs.
    # by default tabs are suspended and input variables are hence
    # not available
    outputOptions(x = output, name = "zlim", suspendWhenHidden = FALSE)
    outputOptions(x = output, name = "centValue", suspendWhenHidden = FALSE)
    outputOptions(x = output, name = "xlim", suspendWhenHidden = FALSE)
    
    # if custom datapoint color get RGB code from separate input panel
    color <- ifelse(input$color == "custom", input$rgb, input$color)
    
    if(!all(is.na(unlist(values$data_secondary)))) {
      # if custom datapoint color get RGB code from separate input panel
      if(input$color2 == "custom") {
        color2<- input$rgb2
      } else {
        color2<- input$color2
      }
    } else {
      color2<- adjustcolor("white", alpha.f = 0)
    }
    
    # if custom datapoint style get char from separate input panel
    pch<- ifelse(input$pch == "custom", input$custompch, as.integer(input$pch)-1)
    
    # if custom datapoint style get char from separate input panel
    pch2<- ifelse(input$pch2 == "custom", input$custompch2, as.integer(input$pch2)-1)
    
    # workaround to initialize plotting after app startup
    centValue <- ifelse(is.null(input$centValue), 3000, input$centValue)
    
    # create numeric vector of lines
    line <- sapply(1:8, function(x) input[[paste0("line", x)]])
    
    # create char vector of line colors
    line.col <- sapply(1:8, function(x) input[[paste0("colline", x)]])
    
    # create char vector of line labels
    line.label <- sapply(1:8, function(x) input[[paste0("labline", x)]])
    
    
    # if custom bar color get RGB from separate input panel or "none"
    bar.col <- ifelse(input$bar == "custom", 
                      adjustcolor(col = input$rgbBar, 
                                  alpha.f = input$alpha.bar/100),
                      ifelse(input$bar == "none",
                             input$bar,
                             adjustcolor(col = input$bar, 
                                         alpha.f = input$alpha.bar/100)))
      
    # if custom bar color get RGB from separate input panel or "none"
    # SECONDARY DATA SET
    bar.col2 <- ifelse(input$bar2 == "custom",
                       adjustcolor(col = input$rgbBar2, 
                                   alpha.f = input$alpha.bar/100),
                       ifelse(input$bar2 == "none",
                              input$bar,
                              adjustcolor(col = input$bar2, 
                                          alpha.f = input$alpha.bar/100)))
    
    # if custom grid color get RGB from separate input panel or "none"
    grid.col <- ifelse(input$grid == "custom",
                       adjustcolor(col = input$rgbGrid, 
                                   alpha.f = input$alpha.grid/100),
                       ifelse(input$grid == "none",
                              input$grid,
                              adjustcolor(col = input$grid, 
                                          alpha.f = input$alpha.grid/100)))
    
    # workaround: if no legend wanted set label to NA and hide 
    # symbol on coordinates -999, -999
    if(input$showlegend == FALSE) {
      legend<- c(NA,NA)
      legend.pos<- c(-999,-999)
    } else {
      if(!all(is.na(unlist(values$data_secondary))))
      {
        legend<- c(input$legendname, input$legendname2)
        legend.pos<- input$legend.pos
      } else {
        legend<- c(input$legendname, "")
        legend.pos<- input$legend.pos
      }
      
    }
    
    # plot radial Plot
    values$args <- list(
      data = values$data, 
      xlim = input$xlim, 
      zlim = input$zlim, 
      xlab = c(input$xlab1, input$xlab2), 
      ylab = input$ylab,
      zlab = input$zlab,
      y.ticks = input$yticks,
      grid.col = grid.col,
      bar.col = c(bar.col, bar.col2),
      pch = c(pch,pch2),
      col = c(color,color2),
      line = line,
      line.col = line.col,
      line.label = line.label,
      main = input$main,
      cex = input$cex,
      mtext = input$mtext,
      log.z = input$logz, 
      stats = input$statlabels, 
      plot.ratio = input$curvature, 
      summary = if (input$summary) input$stats else NA,
      summary.pos = input$sumpos, 
      legend = legend, 
      legend.pos = legend.pos,
      na.rm = TRUE, 
      central.value = input$centValue, 
      centrality = input$centrality,
      lwd = c(input$lwd, input$lwd2),
      lty = c(as.integer(input$lty), as.integer(input$lty2)))
    
  })
  
  # render Radial Plot
  output$main_plot <- renderPlot({
    
    # validate(need()) makes sure that all data are available to
    # renderUI({}) before plotting and will wait until there
    validate(
      need(expr = input$centValue, message = 'Waiting for data... Please wait!'),
      need(expr = input$zlim, message = 'Waiting for data... Please wait!')
    )
    
    do.call(plot_RadialPlot, args = values$args)
    
  })##EndOf::renderPlot({})
  
  observe({
    
    # nested renderText({}) for code output on "R plot code" tab
    code.output <- callModule(RLumShiny:::printCode, "printCode", n_input = 2, 
                              fun = "plot_RadialPlot(data,", args = values$args)
    
    output$plotCode<- renderText({
      code.output
    })##EndOf::renderText({})
    
    callModule(RLumShiny:::exportCodeHandler, "export", code = code.output)
    callModule(RLumShiny:::exportPlotHandler, "export", fun = "plot_RadialPlot", args = values$args)
    
  })
  
  # renderTable() that prints the data to the second tab
  output$dataset<- renderDataTable(
    options = list(pageLength = 10, autoWidth = FALSE),
    callback = "function(table) {
    table.on('click.dt', 'tr', function() {
    $(this).toggleClass('selected');
    Shiny.onInputChange('rows',
    table.rows('.selected').values$data.toArray());
    });
}",
    {
      data<- values$data[[1]]
      colnames(data)<- c("De","De error")
      
      data
    })##EndOf::renterTable()
  
  # renderTable() that prints the secondary data to the second tab
  output$dataset2<- renderDataTable(
    options = list(pageLength = 10, autoWidth = FALSE),
    callback = "function(table) {
  table.on('click.dt', 'tr', function() {
  $(this).toggleClass('selected');
  Shiny.onInputChange('rows',
  table.rows('.selected').values$data.toArray());
  });
  }",
    {
      if(!all(is.na(unlist(values$data_secondary)))) {
        data<- values$data[[2]]
        colnames(data)<- c("De","De error")
        data
      } else {
      }
    })##EndOf::renterTable()
  
  
  # renderTable() to print the results of the
  # central age model (CAM)
  output$CAM<- renderDataTable(
    options = list(pageLength = 10, autoWidth = FALSE),
    {
      
      data <- values$data
      
      t<- as.data.frame(matrix(nrow = length(data), ncol = 7))
      colnames(t)<- c("Data set","n", "log data", "Central dose", "SE abs.", "OD (%)", "OD error (%)")
      res<- lapply(data, function(x) { calc_CentralDose(x, verbose = FALSE, plot = FALSE) })
      for(i in 1:length(res)) {
        t[i,1]<- ifelse(i==1,"pimary","secondary")
        t[i,2]<- length(res[[i]]@data$data[,1])
        t[i,3]<- res[[i]]@data$args$log
        t[i,4:7]<- round(res[[i]]@data$summary[1:4],2)
      }
      t
    })##EndOf::renterTable()
  
}##EndOf::shinyServer(function(input, output)