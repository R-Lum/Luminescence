## Server.R
## MAIN FUNCTION
function(input, output, session) {
  
  # input data (with default)
  values <- reactiveValues(data_primary = ExampleData.DeValues$CA1,
                           data_secondary = setNames(as.data.frame(matrix(NA_real_, nrow = 5, ncol = 2)), c("x", "y")),
                           data = NULL,
                           args = NULL)
  
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
    
    ### DATA FILTER
    input$exclude
    
    sub <- data
    
    isolate({
      filter.prim<- input$filter.prim
      filter.sec<- input$filter.sec
    })
    
    if(!is.null(filter.prim)) {
      index<- grep(paste(filter.prim, collapse = "|"), data[[1]][,1])
      sub[[1]]<- data[[1]][-index,]
    }
    if(length(data) == 2 && !is.null(filter.sec)) {
      index<- grep(paste(filter.sec, collapse = "|"), data[[2]][,1])
      sub[[2]]<- data[[2]][-index,]
    }
    
    stillSelected.prim<- filter.prim
    stillSelected.sec<- filter.sec
    
    updateSelectInput(session, inputId = "filter.prim", choices = sort(data[[1]][,1]), selected = stillSelected.prim)
    if(length(data) == 2) {
      updateSelectInput(session, inputId = "filter.sec", choices = sort(data[[2]][,1]), selected = stillSelected.sec)
    }
    
    values$data <- sub
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
    # Desc.: the rownames are not updated when copying values in the table
    # that exceed the current number of rows; hence, we have to manually 
    # update the rownames before running hot_to_r(), which would crash otherwise
    
    # to modify the rhandsontable we need to create a local non-reactive variable
    df_tmp <- input$table_in_primary
    row_names <-  as.list(as.character(seq_len(length(df_tmp$data))))
    
    # now overwrite the erroneous entries in the list: 'rRowHeaders', 'rowHeaders'
    # and 'rDataDim'
    df_tmp$params$rRowHeaders <- row_names
    df_tmp$params$rowHeaders <- row_names
    df_tmp$params$rDataDim <- as.list(c(length(row_names),
                                        length(df_tmp$params$columns)))
    
    # With the above workaround we run into the problem that the 'afterRemoveRow'
    # event checked in rhandsontable:::toR also tries to remove the surplus rowname(s)
    # For now, we can overwrite the event and handle the 'afterRemoveRow' as a usual
    # 'afterChange' event
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
  
  # dynamically inject sliderInput for x-axis range
  output$xlim<- renderUI({
    
    data<- values$data
    
    if(input$logz == TRUE) {
      sd<- unlist(lapply(data, function(x) x[,2]/x[,1]))
    } else {
      sd<- unlist(lapply(data, function(x) x[,2]))
    }
    prec<- 1/sd
    sliderInput(inputId = "xlim", sep="",
                label = "Range x-axis",
                min = 0, 
                max = round(max(prec)*2, 3),
                value = c(0, max(prec)*1.05))
  })## EndOf::renderUI()
  
  # dynamically inject sliderInput for z-axis range
  output$zlim<- renderUI({
    
    data<- unlist(lapply(values$data, function(x) x[,1]))
    
    min<- min(data)
    max<- max(data)
    sliderInput(inputId = "zlim",  sep="",
                label = "Range z-axis", 
                min = min*0.25,
                max = round(max*1.75, 3),
                value = c(min*0.8, max*1.2))
  })## EndOf::renderUI()
  
  
  output$ylim<- renderUI({
    ylim<- plot_AbanicoPlot(values$data, output = TRUE)$ylim
    sliderInput(inputId = "ylim",  sep="",
                label = "Range y-axis",
                min = ylim[1]*4, 
                max = round(ylim[2]*4, 3),
                value = c(ylim[1], ylim[2]))
  })
  
  
  # dynamically inject sliderInput for KDE bandwidth
  output$bw<- renderUI({
    data<- unlist(lapply(values$data, function(x) x[,1]))
    if(input$logz == TRUE) {
      data<- log(data)
      min<- 0.001
      value<- bw.nrd0(data)*2
      max<- value*2
    } else {
      value<- bw.nrd0(data)
      min<- value/4
      max<- value*4
    }
    sliderInput(inputId = "bw",  sep="",
                label = "KDE bandwidth", 
                min = round(min, 3), 
                max = round(max, 3),
                value = value)
  })## EndOf::renderUI()
  
  
  output$centralityNumeric<- renderUI({
    
    data <- values$data
    
    numericInput(inputId = "centralityNumeric", 
                 label = "Value", 
                 value = round(mean(data[[1]][,1]), 2),
                 step = 0.01)
  })
  
  observe({
    # refresh plot on button press
    input$refresh
    
    # make sure that input panels are registered on non-active tabs.
    # by default tabs are suspended and input variables are hence
    # not available
    outputOptions(x = output, name = "bw", suspendWhenHidden = FALSE)
    outputOptions(x = output, name = "zlim", suspendWhenHidden = FALSE)
    outputOptions(x = output, name = "xlim", suspendWhenHidden = FALSE)
    outputOptions(x = output, name = "ylim", suspendWhenHidden = FALSE)
    outputOptions(x = output, name = "centralityNumeric", suspendWhenHidden = FALSE)
    
    # if custom datapoint color get RGB code from separate input panel
    color <- ifelse(input$color == "custom", input$jscol1, input$color)
    
    if(!all(is.na(unlist(values$data_secondary)))) {
      # if custom datapoint color get RGB code from separate input panel
      if(input$color2 == "custom") {
        if(input$jscol2 == "") {
          color2<- "black"
        } else {
          color2<- input$jscol2
        }
      } else {
        color2<- input$color2
      }
    } else {
      color2<- "black" #adjustcolor("white", alpha.f = 0)
    }
    
    # if custom datapoint style get char from separate input panel
    pch<- ifelse(input$pch == "custom", input$custompch, as.integer(input$pch)-1)
    
    # if custom datapoint style get char from separate input panel
    pch2<- ifelse(input$pch2 == "custom", input$custompch2, as.integer(input$pch2)-1)
    
    # create numeric vector of lines
    line <- sapply(1:8, function(x) input[[paste0("line", x)]])
    
    # create char vector of line colors
    line.col <- sapply(1:8, function(x) input[[paste0("colline", x)]])
    
    # create char vector of line labels
    line.label <- sapply(1:8, function(x) input[[paste0("labline", x)]])
    
    # create integer vector of line types
    line.lty <- sapply(1:8, function(x) as.numeric(input[[paste0("linelty", x)]]))
    
    # if custom polygon color get RGB from separate input panel or "none"
    polygon.col <- ifelse(input$polygon == "custom",
                          adjustcolor(col = input$rgbPolygon, alpha.f = input$alpha.polygon/100),
                          ifelse(input$polygon == "none", 
                                 input$polygon, 
                                 adjustcolor(col = input$polygon, alpha.f = input$alpha.polygon/100)))
      
    # if custom polygon color get RGB from separate input panel or "none"
    # (secondary data set)
    polygon.col2 <- ifelse(input$polygon2 == "custom",
                           adjustcolor(col = input$rgbPolygon2, alpha.f = input$alpha.polygon/100),
                           ifelse(input$polygon2 == "none", 
                                  input$polygon2, 
                                  adjustcolor(col = input$polygon2, alpha.f = input$alpha.polygon/100)))
    
    
    # if custom bar color get RGB from separate input panel or "none"
    bar.col <- ifelse(input$bar == "custom", 
                      adjustcolor(col = input$rgbBar, alpha.f = input$alpha.bar/100), 
                      ifelse(input$bar == "none", 
                             input$bar, 
                             adjustcolor(col = input$bar, alpha.f = input$alpha.bar/100)))
      
    
    # if custom bar color get RGB from separate input panel or "none"
    # SECONDARY DATA SET
    bar.col2 <- ifelse(input$bar2 == "custom",
                       adjustcolor(col = input$rgbBar2, alpha.f = input$alpha.bar/100),
                       ifelse(input$bar2 == "none", 
                              input$bar, 
                              adjustcolor(col = input$bar2, alpha.f = input$alpha.bar/100)))
    
    # if custom grid color get RGB from separate input panel or "none"
    grid.col <- ifelse(input$grid == "custom",
                       adjustcolor(col = input$rgbGrid, alpha.f = input$alpha.grid/100),
                       ifelse(input$grid == "none",
                              input$grid, 
                              adjustcolor(col = input$grid, alpha.f = input$alpha.grid/100)))
    
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
    
    # TODO: arg 'bar' handling (custom values, 1 or 2 bars)
    if (input$customSigBar) {
      if (!input$addBar)
        bar <- input$sigmabar1
      if (input$addBar)
        bar <- c(input$sigmabar1, input$sigmabar2)
    } else {
      bar <- TRUE
    }
    
    
    # check wether a keyword or a numeric value is used for
    # centrality
    centrality <- ifelse(input$centrality == "custom", input$centralityNumeric, input$centrality)
    
    # check wether predefined or custom dispersion
    dispersion<- ifelse(input$dispersion == "custom", paste("p", input$cinn, sep=""), input$dispersion)
    
    # save all arguments in a list
    values$args<- list(data = values$data,
                y.axis = input$yaxis,
                bw = input$bw,
                bar = bar,
                dispersion = dispersion,
                plot.ratio = input$p.ratio,
                z.0 = centrality, 
                log.z = input$logz, 
                summary = if (input$summary) input$stats else NA,
                summary.pos = input$sumpos,
                summary.method = input$summary.method,
                col = c(color,color2),
                pch = c(pch,pch2),
                zlab = input$zlab,
                main = input$main,
                zlim = input$zlim, 
                cex = input$cex,
                mtext = input$mtext,
                stats = input$statlabels,
                error.bars = input$errorbars,
                line = line,
                line.col = line.col,
                line.label = line.label,
                line.lty = line.lty,
                polygon.col = c(polygon.col,polygon.col2),
                bar.col = c(bar.col, bar.col2),
                grid.col = grid.col,
                legend = legend,
                legend.pos = legend.pos,
                na.rm = TRUE,
                lwd = c(input$lwd, input$lwd2),
                xlab = c(input$xlab1, input$xlab2),
                ylab = input$ylab,
                lty = c(as.integer(input$lty), as.integer(input$lty2)),
                xlim = input$xlim,
                ylim = input$ylim,
                rug = input$rug,
                layout = input$layout,
                rotate = input$rotate,
                boxplot = input$boxplot,
                kde = input$kde,
                hist = input$histogram,
                dots = input$dots,
                frame = input$frame)
  })
  
  # render Abanico Plot
  output$main_plot <- renderPlot({
    
    # validate(need()) makes sure that all data are available to
    # renderUI({}) before plotting and will wait until there
    validate(need(expr = input$bw, message = ''),
             need(expr = input$zlim, message = ''),
             need(expr = input$ylim, message = ''),
             need(expr = input$centralityNumeric, message = 'Waiting for data... Please wait!'))
    
    # plot Abanico Plot 
    do.call(what = plot_AbanicoPlot, args = values$args)
    
  })##EndOf::renderPlot({})
  
  observe({
    # nested renderText({}) for code output on "R plot code" tab
    code.output <- callModule(RLumShiny:::printCode, "printCode", 
                              n_input = ifelse(!all(is.na(unlist(values$data_secondary))), 2, 1),
                              fun = "plot_AbanicoPlot(data,", args = values$args)
    
    output$plotCode<- renderText({
      code.output
    })##EndOf::renderText({})
    
    callModule(RLumShiny:::exportCodeHandler, "export", code = code.output)
    callModule(RLumShiny:::exportPlotHandler, "export", fun = "plot_AbanicoPlot", args = values$args)
  })
  
  Selected<- reactive({
    input$refresh
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
  data <- values$data
  colnames(data[[1]])<- c("De","De error")
  data[[1]]
  
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
    data <- values$data
    colnames(data[[2]])<- c("De","De error")
    data[[2]]
  } 
})##EndOf::renterTable()
  
  # renderTable() to print the results of the
  # central age model (CAM)
  output$CAM<- renderDataTable(
    options = list(pageLength = 10, autoWidth = FALSE),
    {
      data<- values$data
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