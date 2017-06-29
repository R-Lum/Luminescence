## Server.R
## MAIN FUNCTION
function(input, output, session) {
  
  # input data (with default)
  values <- reactiveValues(data = ExampleData.DeValues$CA1)
  
  # check and read in file (DATA SET 1)
  observeEvent(input$file1, {
    inFile<- input$file1
    
    if(is.null(inFile)) 
      return(NULL) # if no file was uploaded return NULL
    
    values$data <- fread(file = inFile$datapath, data.table = FALSE) # inFile[1] contains filepath 
  })
  
  
  # dynamically inject sliderInput for x-axis range
  output$xlim<- renderUI({
    
    # check if file is loaded
    # # case 1: yes -> slinderInput with custom values
    xlim.plot<- range(hist(values$data[,1], plot = FALSE)$breaks)
    
    sliderInput(inputId = "xlim", 
                label = "Range x-axis",
                min = xlim.plot[1]*0.5, 
                max = xlim.plot[2]*1.5,
                value = c(xlim.plot[1], xlim.plot[2]), round=FALSE, step=0.0001)
    
  })## EndOf::renderUI()
  
  output$table_in_primary <- renderRHandsontable({
      rhandsontable(values$data, 
                    height = 300, 
                    colHeaders = c("Dose", "Error"), 
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
      values$data <- hot_to_r(df_tmp)
  })
  
  output$main_plot <- renderPlot({
    
    # refresh plot on button press
    input$refresh
    
    # progress bar
    progress<- Progress$new(session, min = 0, max = 3)
    progress$set(message = "Calculation in progress",
                 detail = "Retrieve data")
    on.exit(progress$close())
    
    # make sure that input panels are registered on non-active tabs.
    # by default tabs are suspended and input variables are hence
    # not available
    outputOptions(x = output, name = "xlim", suspendWhenHidden = FALSE)
    
    progress$set(value = 1)
    progress$set(message = "Calculation in progress",
                 detail = "Get values")
    
    # check if any summary stats are activated, else NA
    if (input$summary) {
      summary<- input$stats
    } else {
      summary<- NA
    }
    
    # if custom datapoint color get RGB code from separate input panel
    if(input$pchColor == "custom") {
      pch.color<- input$pchRgb
    } else {
      pch.color<- input$pchColor
    }
    
    # if custom datapoint color get RGB code from separate input panel
    if(input$barsColor == "custom") {
      bars.color<-  adjustcolor(col = input$barsRgb,
                                alpha.f = input$alpha.bars/100)
    } else {
      bars.color<-  adjustcolor(col = input$barsColor,
                                alpha.f = input$alpha.bars/100)
    }
    
    # if custom datapoint color get RGB code from separate input panel
    if(input$rugsColor == "custom") {
      rugs.color<- input$rugsRgb
    } else {
      rugs.color<- input$rugsColor
    }
    
    # if custom datapoint color get RGB code from separate input panel
    if(input$normalColor == "custom") {
      normal.color<- input$normalRgb
    } else {
      normal.color<- input$normalColor
    }
    
    # update progress bar
    progress$set(value = 2)
    progress$set(message = "Calculation in progress",
                 detail = "Combine values")
    
    colors<- c(bars.color, rugs.color, normal.color, pch.color)
    
    # if custom datapoint style get char from separate input panel
    if(input$pch == "custom") {
      pch<- input$custompch
    } else {
      pch<- as.integer(input$pch)-1 #-1 offset in pch values
    }
    
    # validate(need()) makes sure that all data are available to
    # renderUI({}) before plotting and will wait until there
    validate(
      need(expr = input$xlim, message = 'Waiting for data... Please wait!')
    )
    
    progress$set(value = 3)
    progress$set(message = "Calculation in progress",
                 detail = "Ready to plot")
    
    args <- list(data = values$data,
                 na.rm = TRUE, 
                 cex.global = input$cex, 
                 pch = pch,
                 xlim = input$xlim,
                 summary.pos = input$sumpos, 
                 mtext = input$mtext, 
                 main = input$main,
                 rug = input$rugs, 
                 se = input$errorBars, 
                 normal_curve = input$norm, 
                 summary = summary,
                 xlab = input$xlab,
                 ylab = c(input$ylab1, input$ylab2),
                 colour = colors)
    
    do.call(plot_Histogram, args = args)
    
    # prepare code as text output
    header <- paste("# To reproduce the plot in your local R environment",
                    "# copy and run the following code to your R console.",
                    "library(Luminescence)",
                    "library(data.table)",
                    "file<- file.choose()",
                    "data <- data.table::fread(file, data.table = FALSE)",
                    "\n",
                    sep = "\n")
    
    names <- names(args)
    
    verb.arg <- paste(mapply(function(name, arg) {
      if (all(inherits(arg, "character")))
        arg <- paste0("'", arg, "'")
      if (length(arg) > 1)
        arg <- paste0("c(", paste(arg, collapse = ", "), ")")
      if (is.null(arg))
        arg <- "NULL"
      paste(name, "=", arg) 
    }, names[-1], args[-1]), collapse = ",\n")
    
    funCall <- paste0("plot_Histogram(data = data,\n", verb.arg, ")")
    
    code.output <- paste0(header, funCall, collapse = "\n")
    
    # nested renderText({}) for code output on "R plot code" tab
    output$plotCode<- renderText({
      
      code.output
      
    })##EndOf::renderText({})
    
    output$exportScript <- downloadHandler(
      filename = function() { paste(input$filename, ".", "R", sep="") },
      content = function(file) {
        write(code.output, file)
      },#EO content =,
      contentType = "text"
    )#EndOf::dowmloadHandler()
    
    
    # nested downloadHandler() to print plot to file
    output$exportFile <- downloadHandler(
      filename = function() { paste(input$filename, ".", input$fileformat, sep="") },
      content = function(file) {
        
        # determine desired fileformat and set arguments
        if(input$fileformat == "pdf") {
          pdf(file, 
              width = input$imgwidth, 
              height = input$imgheight, 
              paper = "special",
              useDingbats = FALSE, 
              family = input$fontfamily)
        }
        if(input$fileformat == "svg") {
          svg(file, 
              width = input$imgwidth, 
              height = input$imgheight, 
              family = input$fontfamily)
        }
        if(input$fileformat == "eps") {
          postscript(file, 
                     width = input$imgwidth, 
                     height = input$imgheight, 
                     paper = "special", 
                     family = input$fontfamily)
        }
        
        do.call(plot_Histogram, args = args)
        
        dev.off()
        
      },#EO content =,
      contentType = "image"
    )#EndOf::dowmloadHandler()
  })##EndOf::renderPlot({})
  
  
  # renderTable() that prints the data to the second tab
  output$dataset<- renderDataTable(
    options = list(pageLength = 10, autoWidth = FALSE),
    callback = "function(table) {
    table.on('click.dt', 'tr', function() {
    $(this).toggleClass('selected');
    Shiny.onInputChange('rows',
    table.rows('.selected').data().toArray());
    });
}",
    {
      setNames(values$data, c("De", "De error"))
    })##EndOf::renterTable()
  
  
  # reactive function for gVis plots that allow for dynamic input!
  myOptionsCAM<- reactive({
    options<- list(
      page="enable",
      width="500px",
      sort="disable")
    return(options)
  })
  
  # renderTable() to print the results of the
  # central age model (CAM)
  output$CAM<- renderDataTable(
    options = list(pageLength = 10, autoWidth = FALSE),
    {

      t<- as.data.frame(matrix(nrow = length(list(values$data)), ncol = 7))
      colnames(t)<- c("Data set","n", "log data", "Central dose", "SE abs.", "OD (%)", "OD error (%)")
      res<- lapply(list(values$data), function(x) { calc_CentralDose(x, verbose = FALSE, plot = FALSE) })
      for(i in 1:length(res)) {
        t[i,1]<- ifelse(i==1,"pimary","secondary")
        t[i,2]<- length(res[[i]]@data$data[,1])
        t[i,3]<- res[[i]]@data$args$log
        t[i,4:7]<- round(res[[i]]@data$summary[1:4],2)
      }
      t
    })##EndOf::renterTable()
  
}##EndOf::function(input, output)