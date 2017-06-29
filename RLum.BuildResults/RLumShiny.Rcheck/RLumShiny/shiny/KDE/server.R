## MAIN FUNCTION
function(input, output, session) {
  
  # input data (with default)
  values <- reactiveValues(data_primary = ExampleData.DeValues$CA1,
                           data_secondary = setNames(as.data.frame(matrix(NA_real_, nrow = 5, ncol = 2)), c("x", "y")))
  
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
  Data<- reactive({
    
    ### GET DATA
    data <- list(values$data_primary, values$data_secondary)
    data <- lapply(data, function(x) { 
      x_tmp <- x[complete.cases(x), ]
      if (nrow(x_tmp) == 0) return(NULL)
      else return(x_tmp)
    })
    data <- data[!sapply(data, is.null)]
    data <- lapply(data, function(x) setNames(x, c("Dose", "Error")))
    
    return(data)
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
  
  
  # dynamically inject sliderInput for x-axis range
  output$xlim<- renderUI({
    
    data <- do.call(rbind, Data())
    
    sliderInput(inputId = "xlim", 
                label = "Range x-axis",
                min = min(data[,1])*0.25,
                max = max(data[,1])*1.75,
                value = c(min(data[,1])*0.9, max(data[,1])*1.1))
    
  })## EndOf::renderUI()
  
  # dynamically inject sliderInput for KDE bandwidth
  output$bw<- renderUI({
    
    data <- do.call(rbind, Data())
    
    sliderInput(inputId = "bw", 
                label = "KDE bandwidth", 
                min = bw.nrd0(data[,1])/4, 
                max = bw.nrd0(data[,1])*4,
                value = bw.nrd0(data[,1]))
    
  })## EndOf::renderUI()
  
  
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
    outputOptions(x = output, name = "bw", suspendWhenHidden = FALSE)
    
    # check if file is loaded and overwrite example data
    data <- Data()
    
    progress$set(value = 1)
    progress$set(message = "Calculation in progress",
                 detail = "Get values")
    
    
    # check if any summary stats are activated, else NA
    if (input$summary) {
      summary<- input$stats
    } else {
      summary<- ""
    }
    
    if(input$logx == TRUE) {
      logx<- "x"
    } else {
      logx<- ""
    }
    
    
    # update progress bar
    progress$set(value = 2)
    progress$set(message = "Calculation in progress",
                 detail = "Combine values")
    
    # if custom datapoint color get RGB code from separate input panel
    if(input$color == "custom") {
      color<- input$rgb
    } else {
      color<- input$color
    }
    
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
    
    
    # validate(need()) makes sure that all data are available to
    # renderUI({}) before plotting and will wait until there
    validate(
      need(expr = input$xlim, message = 'Waiting for data... Please wait!'),
      need(expr = input$bw, message = 'Waiting for data... Please wait!')
    )
    
    progress$set(value = 3)
    progress$set(message = "Calculation in progress",
                 detail = "Ready to plot")    
    
    args <- list(data = data, 
                 cex = input$cex, 
                 log = logx,
                 xlab = input$xlab,
                 ylab = c(input$ylab1, input$ylab2),
                 main = input$main,
                 values.cumulative = input$cumulative,
                 na.rm = TRUE, 
                 rug = input$rug,
                 boxplot = input$boxplot,
                 summary = summary,
                 summary.pos = input$sumpos,
                 summary.method = input$summary.method,
                 bw = input$bw,
                 xlim = input$xlim,
                 col = c(color, color2))
    
    do.call(plot_KDE, args = args)
    
    
    # prepare code as text output
    str1 <- "data <- data.table::fread(file, data.table = FALSE)"
    
    if(!all(is.na(unlist(values$data_secondary)))) {
      str2 <- "file2 <- file.choose()"
      str3 <- "data2 <- data.table::fread(file2, data.table = FALSE)"
      str4 <- "data <- list(data, data2)"
      str1 <- paste(str1, str2, str3, str4, sep = "\n")
    }
    
    header <- paste("# To reproduce the plot in your local R environment",
                    "# copy and run the following code to your R console.",
                    "library(Luminescence)",
                    "file<- file.choose()",
                    str1,
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
    
    funCall <- paste0("plot_KDE(data = data,\n", verb.arg, ")")
    
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
        
        if(is.null(input$fileformat))       
          updateRadioButtons(session, inputId = "fileformat", label = "Fileformat", selected = "pdf")
        
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
        
        do.call(plot_KDE, args = args)
        
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

    data <- Data()[[1]]
    colnames(data) <- c("De","De error")
    data
    
})##EndOf::renterTable()

# renderTable() that prints the secondary data to the second tab
output$dataset2<- renderDataTable(
  options = list(pageLength = 10, autoWidth = FALSE),
  callback = "function(table) {
  table.on('click.dt', 'tr', function() {
  $(this).toggleClass('selected');
  Shiny.onInputChange('rows',
  table.rows('.selected').data().toArray());
  });
  }",
{
  if(!all(is.na(unlist(values$data_secondary)))) {
    
    data <- Data()[[2]]
    colnames(data) <- c("De","De error")
    data
    
  } else {
  }
})##EndOf::renterTable()


# renderTable() to print the results of the
# central age model (CAM)
output$CAM<- renderDataTable(
  options = list(pageLength = 10, autoWidth = FALSE),
{
 
  data <- Data()
  
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